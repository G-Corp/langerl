#ifndef __LANGERL_RUBY_CLASS_INCLUDE
#define __LANGERL_RUBY_CLASS_INCLUDE

VALUE mLangerl;
VALUE cTuple;

typedef struct TTuple {
  VALUE data;
  int b;
} TTuple;

void create_erlang_module(void);
VALUE langerl_tuple_new(VALUE, VALUE);

#define CHECK_TUPLE(val) ((CLASS_OF(val) == cTuple))

#endif
