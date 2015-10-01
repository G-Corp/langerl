#include <string.h>
#include <stdlib.h>
#include <Python.h>
#include <ei.h>
#include "langerl_interpreter.h"

int start_interpreter(void **interpreter) {
  *interpreter = NULL;
  LANGERL_LOG("start interpreter for python!");
  Py_SetProgramName("node.python");
  Py_Initialize();
  return 1;
}

int stop_interpreter(void *interpreter) {
  LANGERL_LOG("stop interpreter for complex!");
  Py_Finalize();
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

void ** to_interpreter_array(ei_x_buff * buff) {
  return (void**)malloc(0);
}
