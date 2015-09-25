#undef RUBY_EXPORT
#include <ruby/ruby.h>
#include <erl_interface.h>
#include <ei.h>

#include "erl_listen.h"

int main(int argc, char **argv) {
  erl_init(NULL, 0);

  if(erl_connect_init(1, "secretcookie", 0) == -1) {
    erl_err_quit("erl_connect_init");
  }

  ruby_sysinit(&argc, &argv); 
  {
    RUBY_INIT_STACK;
    ruby_init();
    ruby_init_loadpath();

    rb_funcall(rb_mKernel, rb_intern("require"), 1, rb_str_new2("./sum.rb"));

    ID sym_mymodule = rb_intern("Summer");
    VALUE mymodule = rb_const_get(rb_cObject, sym_mymodule);
    VALUE result = rb_funcall(mymodule, rb_intern("sum"), 1, INT2FIX(10));

    printf("Result = %d\n", NUM2INT(result));
    return ruby_cleanup(0);
  }
}


