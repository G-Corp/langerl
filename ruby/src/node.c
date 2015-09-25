#undef RUBY_EXPORT
#include <ruby/ruby.h>
#include <erl_interface.h>
#include <ei.h>

#include "erl_listen.h"

// TODO in config file
#define PORT 3456
#define COOKIE_NAME "secretcookie"
#define ROOT_RB "./sum.rb"
#define BUFSIZE 1000

int main(int argc, char **argv) {
  int port = PORT;
  int listen;

  erl_init(NULL, 0);

  if(erl_connect_init(1, COOKIE_NAME, 0) == -1) {
    erl_err_quit("erl_connect_init");
  }

  /* Make a listen socket */
  if((listen = erl_listen(port)) <= 0) {
    erl_err_quit("invalid port for node.rb");
  }

  if(erl_publish(port) == -1) {
    erl_err_quit("epmd not started!");
  }

  ruby_sysinit(&argc, &argv); 
  {
    RUBY_INIT_STACK;
    ruby_init();
    ruby_init_loadpath();

    rb_funcall(rb_mKernel, rb_intern("require"), 1, rb_str_new2(ROOT_RB));

    ID sym_mymodule = rb_intern("Summer");
    VALUE mymodule = rb_const_get(rb_cObject, sym_mymodule);
    VALUE result = rb_funcall(mymodule, rb_intern("sum"), 1, INT2FIX(10));

    printf("Result = %d\n", NUM2INT(result));
    return ruby_cleanup(0);
  }
}


