#undef RUBY_EXPORT
#include <ruby/ruby.h>
#include "langerl_interpreter.h"

#define ROOT_RB "./ruby/tests/sum.rb"

int start_interpreter(void **interpreter) {
  *interpreter = NULL;
  LANGERL_LOG("start interpreter for ruby!");
  const char *argv[] = {"node.ruby"};
  int argc = 1;

   ruby_sysinit(&argc, (char***)&argv); 
   {
     RUBY_INIT_STACK;
     ruby_init();
     ruby_init_loadpath();

     rb_funcall(rb_mKernel, rb_intern("require"), 1, rb_str_new2(ROOT_RB));

     ID sym_mymodule = rb_intern("Summer");
     VALUE mymodule = rb_const_get(rb_cObject, sym_mymodule);
     VALUE result = rb_funcall(mymodule, rb_intern("sum"), 1, INT2FIX(10));

     LANGERL_LOG("Result = %d\n", NUM2INT(result));
   }
  return 1;
}

int stop_interpreter(void *interpreter) {
  LANGERL_LOG("stop interpreter for complex! %s", interpreter);
  ruby_cleanup(0);
  return 1;
}

int test_interpreter(int value) {
  LANGERL_LOG("test interpreter for complex!");
  return value * value;
}
