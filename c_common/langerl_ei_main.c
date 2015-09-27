#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ei.h>
#include "langerl_error.h"
#include "langerl_interpreter.h"
#include "langerl_logger.h"

static void main_message_loop();
static void reconnect();
static int handle_msg(erlang_pid *);

struct {
  void *interpreter;
  char *erlang_node;

  int fd;
  ei_cnode ec;
  ei_x_buff x_in;
  ei_x_buff x_out;
  ei_x_buff x_rpc_in;
  ei_x_buff x_rpc_out;
} EI_INTERPRETER_STATE;

int main(int argc, char **argv) {
  char *interpreter_node;
  char *interpreter_host;
  char *cookie;
  struct hostent *host;
  struct in_addr *addr;
  char *fullnodeid;

  if(argc != 5) {
    exit(1);
  }
  interpreter_node = argv[1];
  interpreter_host = argv[2];
  EI_INTERPRETER_STATE.erlang_node = strdup(argv[3]);
  cookie = argv[4];

  /* Attempt to turn off buffering on stdout/err. */
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(stderr, NULL, _IONBF, 0);

  erl_init(NULL, 0);

  ei_x_new(&EI_INTERPRETER_STATE.x_in);
  ei_x_new(&EI_INTERPRETER_STATE.x_out);
  ei_x_new(&EI_INTERPRETER_STATE.x_rpc_in);
  ei_x_new(&EI_INTERPRETER_STATE.x_rpc_out);

  if((host = gethostbyname(interpreter_host)) == NULL) {
    exit(3);
  }
  fullnodeid = (char *) malloc(strlen(interpreter_node) + 1 + strlen(interpreter_host) + 1);
  sprintf(fullnodeid, "%s@%s", interpreter_node, interpreter_host);
  addr = (struct in_addr *) host->h_addr;

  if(ei_connect_xinit(&EI_INTERPRETER_STATE.ec, interpreter_host, interpreter_node, fullnodeid, addr, cookie, 0) < 0) {
    exit(4);
  }
  LANGERL_LOG("Interpreter Erlang Node '%s' starting.", ei_thisnodename(&EI_INTERPRETER_STATE.ec));
  if((EI_INTERPRETER_STATE.fd = ei_connect(&EI_INTERPRETER_STATE.ec, 
          EI_INTERPRETER_STATE.erlang_node)) < 0) {
    exit(5);
  }

  LANGERL_LOG("Interpreter Erlang Node started.");
  printf("READY\n"); fflush(stdout);
  if(! start_interpreter(&EI_INTERPRETER_STATE.interpreter)) {
    exit(6);
  }

  main_message_loop();

  stop_interpreter(EI_INTERPRETER_STATE.interpreter);

  return 0;
}

static void main_message_loop() {
  erlang_msg message;

  int running = 1;
  ei_x_buff *x_in = &EI_INTERPRETER_STATE.x_in;
  ei_x_buff *x_out = &EI_INTERPRETER_STATE.x_out;
  ei_x_buff *x_rpc_in = &EI_INTERPRETER_STATE.x_rpc_in;
  ei_x_buff *x_rpc_out = &EI_INTERPRETER_STATE.x_rpc_out;

  while(running) {
    x_in->index = 0;
    switch(ei_xreceive_msg(EI_INTERPRETER_STATE.fd, &message, x_in)) {
      case ERL_TICK:
        break;
      case ERL_MSG:
        switch(message.msgtype) {
          case ERL_LINK:
            LANGERL_LOG("DEBUG: Lua Erlang Node linked.");
            break;
          case ERL_UNLINK:
          case ERL_EXIT:
            LANGERL_LOG("DEBUG: Lua Erlang Node unlinked; terminating.");
            running = 0;
            break;
          case ERL_SEND:
          case ERL_REG_SEND:
            {
              erlang_pid pid = { 0 };
              x_in->index = 0;

              running = handle_msg(&pid);

              x_rpc_in->index = x_rpc_out->index = 0;
              ei_x_encode_empty_list(x_rpc_in);
              if(ei_rpc(&EI_INTERPRETER_STATE.ec, EI_INTERPRETER_STATE.fd,
                    "erlang", "is_alive",
                    x_rpc_in->buff, x_rpc_in->index,
                    x_rpc_out) < 0) {
                LANGERL_LOG("DEBUG: Lua Erlang Node error in 'is alive?' rpc to '%s'.", pid.node);
                reconnect();
              }
              if(x_out->index > 0 && ei_send(EI_INTERPRETER_STATE.fd, &pid,
                    x_out->buff, x_out->index) < 0) {
                LANGERL_LOG("FATAL: Lua Erlang Node error in send to '%s'.", pid.node);
                exit(8);
              }
            }
            break;
        }
        break;
      case ERL_ERROR:
      default:
        LANGERL_LOG("DEBUG: Lua Erlang Node error in receive");
        reconnect();
        break;
    }
  }
}

static void reconnect() {
  LANGERL_LOG("Lua Erlang Node '%s' reconnecting.", ei_thisnodename(&EI_INTERPRETER_STATE.ec));
  if((EI_INTERPRETER_STATE.fd = ei_connect(&EI_INTERPRETER_STATE.ec, EI_INTERPRETER_STATE.erlang_node)) < 0) {
    LANGERL_LOG("FATAL: Cannot reconnect to parent node '%s'", EI_INTERPRETER_STATE.erlang_node);
    exit(7);
  }
  LANGERL_LOG("INFO: Lua Erlang Node reconnected.");
}

static int handle_msg(erlang_pid *pid) {
  ei_x_buff *x_in = &EI_INTERPRETER_STATE.x_in;
  ei_x_buff *x_out = &EI_INTERPRETER_STATE.x_out;

  int version;
  int arity;
  int type;
  int len;
  char interpreter_atom[MAXATOMLEN+1] = {0};
  char *code, *args_str;

  if(ei_decode_version(x_in->buff, &x_in->index, &version) < 0) {
    LANGERL_LOG("WARNING: Ignoring malformed message (bad version: %d).", version);
    error_msg(x_out, "bad_version");
    return 1;
  }
  if(ei_decode_tuple_header(x_in->buff, &x_in->index, &arity) < 0) {
    LANGERL_LOG("WARNING: Ignoring malformed message (not tuple).");
    error_msg(x_out, "malformed_message");
    return 1;
  }
  if(arity < 2) {
    LANGERL_LOG("WARNING: Ignoring malformed message (not 4-arity tuple).");
    error_msg(x_out, "wrong_message_size");
    return 1;
  }
  if(ei_decode_atom(x_in->buff, &x_in->index, interpreter_atom) < 0) {
    LANGERL_LOG("WARNING: Ignoring malformed message (first tuple element not atom).");
    error_msg(x_out, "missing_command");
    return 1;
  }
  if(ei_decode_pid(x_in->buff, &x_in->index, pid) < 0) {
    LANGERL_LOG("WARNING: Ignoring malformed message (second tuple element not pid).");
    error_msg(x_out, "missing_sender");
    return 1;
  }

  if(strcmp(interpreter_atom, "test") == 0) {
    if(arity != 3) {
      error_msg(x_out, "missing_parameter");
    } else {
      ei_get_type(x_in->buff, &x_in->index, &type, &len);
      if(ERL_SMALL_INTEGER_EXT == type || ERL_INTEGER_EXT == type) {
        long  value;
        if(ei_decode_long(x_in->buff, &x_in->index, &value) < 0) {
          error_msg(x_out, "invalid_parameter");
        } else {
          x_out->index = 0;
          ei_x_encode_version(x_out);
          ei_x_encode_tuple_header(x_out, 2);
          ei_x_encode_atom(x_out, "test");
          ei_x_encode_long(x_out, test_interpreter(value));
        }
      } else {
        error_msg(x_out, "invalid_parameter");
      }
    }
  } else if(strcmp(interpreter_atom, "call") == 0) {
    // TODO
  } else if(strcmp(interpreter_atom, "load") == 0) {
    if(arity != 3) {
      error_msg(x_out, "missing_parameter");
    } else {
      ei_get_type(x_in->buff, &x_in->index, &type, &len);
      if(ERL_BINARY_EXT == type) {
        char *file = (char*)malloc(sizeof(char)*(len+1));
        memset(file, 0, len + 1);
        if(ei_decode_binary(x_in->buff, &x_in->index, file, NULL) < 0) {
          error_msg(x_out, "invalid_parameter");
        } else {
          switch (load_file_interpreter(file)) {
            case LOAD_OK:
              x_out->index = 0;
              ei_x_encode_version(x_out);
              ei_x_encode_tuple_header(x_out, 2);
              ei_x_encode_atom(x_out, "load");
              ei_x_encode_binary(x_out, file, strlen(file));
              break;
            case LOAD_ALREADY:
              error_msg(x_out, "alreary_loaded");
              break;
            case LOAD_MISSING_FILE:
              error_msg(x_out, "missing_file");
              break;
            case LOAD_ERROR:
            default:
              error_msg(x_out, "load_error");
          }
        }
        free(file);
      } else {
        error_msg(x_out, "invalid_parameter");
      }
    }
  } else if(strcmp(interpreter_atom, "exec") == 0) {
    // TODO
  } else if(strcmp(interpreter_atom, "stop") == 0) {
    LANGERL_LOG("DEBUG: Interpreter Erlang Node stopping normally.");
    x_out->index = 0;
    return 0;
  }

  return 1;
}
