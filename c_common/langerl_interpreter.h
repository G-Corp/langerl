#ifndef __LANGERL_INTERPRETER_INCLUDE
#define __LANGERL_INTERPRETER_INCLUDE

#include "langerl_logger.h"

#define LOAD_OK 1
#define LOAD_ERROR 2
#define LOAD_ALREADY 3
#define LOAD_MISSING_FILE 4

int start_interpreter(void **);
int stop_interpreter(void *);
int test_interpreter(int);
int load_file_interpreter(char *);

#endif
