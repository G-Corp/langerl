#include <string.h>
#include <stdlib.h>
#include <ei.h>
#include "langerl_interpreter.h"

int start_interpreter(void **interpreter) {
  *interpreter = malloc(sizeof(char)*10);
  memset(*interpreter, 0, 9);
  memcpy(*interpreter, "hello", 6);
  LANGERL_LOG("start interpreter for complex!");
  return 1;
}

int stop_interpreter(void *interpreter) {
  LANGERL_LOG("stop interpreter for complex! %s", interpreter);
  free(interpreter);
  return 1;
}

int test_interpreter(int value) {
  LANGERL_LOG("test interpreter for complex!");
  return value * value;
}

int load_file_interpreter(char *file) {
  return LOAD_OK;
}

void * exec_interpreter(char *code, int *result) {
  *result = EXEC_OK;
  return NULL;
}

void * call_interpreter(char *module, char *fun, int arity, void **params) {
  return NULL;
}

void to_erlang(ei_x_buff *x_out, void *data) {
  ei_x_encode_atom(x_out, "nil");
  return;
}

void * to_interpreter(ei_x_buff * buff) {
  return NULL;
}
