#include <ruby/ruby.h>
#include "langerl_ruby_class.h"

static VALUE langerl_method_test(VALUE klass, VALUE param) {
  return param;
}

static void langerl_tuple_free(void *tuple) {
  free(tuple);
}

VALUE langerl_tuple_new(VALUE klass, VALUE elements) {
  if(T_ARRAY == TYPE(elements)) {
    TTuple *tuple = ALLOC(TTuple);
    tuple->data = elements;
    return Data_Wrap_Struct(klass, NULL, langerl_tuple_free, tuple);
  }
  return Qnil;
}

static VALUE langerl_tuple_size(VALUE self) {
  TTuple *tuple;
  Data_Get_Struct(self, TTuple, tuple);
  return INT2NUM(RARRAY_LEN(tuple->data));
}

static VALUE langerl_tuple_element(VALUE self, VALUE pos) {
  TTuple *tuple;
  Data_Get_Struct(self, TTuple, tuple);
  return rb_ary_entry(tuple->data, NUM2INT(pos));
}

void create_erlang_module(void) {
  mLangerl = rb_define_module("Erlang");
  rb_define_singleton_method(mLangerl, "test", langerl_method_test, 1);

  cTuple = rb_define_class_under(mLangerl, "Tuple", rb_cObject);
  rb_define_singleton_method(cTuple, "new", langerl_tuple_new, 1);
  rb_define_method(cTuple, "size", langerl_tuple_size, 0);
  rb_define_method(cTuple, "element", langerl_tuple_element, 1);
}

