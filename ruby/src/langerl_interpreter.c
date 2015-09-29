#undef RUBY_EXPORT
#include <ruby/ruby.h>
#include <unistd.h>
#include <ei.h>
#include "langerl_interpreter.h"
#include "langerl_ruby_class.h"

void ruby_to_erlang_tuple(ei_x_buff *, VALUE);

int start_interpreter(void **interpreter) {
  *interpreter = NULL;
  LANGERL_LOG("start interpreter for ruby!");
  const char *argv[] = {"node.ruby"};
  int argc = 1;

  ruby_sysinit(&argc, (char***)&argv); 
  RUBY_INIT_STACK;
  ruby_init();
  ruby_init_loadpath();

  create_erlang_module();

  return 1;
}

int stop_interpreter(void *interpreter) {
  LANGERL_LOG("stop interpreter for ruby! %s", interpreter);
  ruby_cleanup(0);
  return 1;
}

int test_interpreter(int value) {
  LANGERL_LOG("test interpreter for ruby!");
  return value + value;
}

int load_file_interpreter(char *file) {
  if(access(file, F_OK) == 0) {
    VALUE result = rb_funcall(rb_mKernel, rb_intern("require"), 1, rb_str_new2(file));
    if(result == Qtrue) {
      LANGERL_LOG("Load file %s: OK", file);
      return LOAD_OK;
    } else {
      LANGERL_LOG("Load file %s: ERROR", file);
      return LOAD_ALREADY;
    }
  }
  LANGERL_LOG("Load file %s: MISSING", file);
  return LOAD_MISSING_FILE;
}

void * exec_interpreter(char *code, int *rcod) {
  int state;
  VALUE result = rb_eval_string_protect(code, &state);
  if(0 == state) {
    *rcod = EXEC_OK;
    return (void*)result;
  }
  *rcod = EXEC_ERROR;
  return NULL;
}

void * call_interpreter(char *module, char *fun, int arity, void **params) {
  LANGERL_LOG("CALL %s.%s(%d)", module, fun, arity);
  VALUE module_val = rb_cObject;
  if(NULL != module) {
    ID module_id = rb_intern(module);
    if(rb_const_defined(rb_cObject, module_id)) {
      module_val = rb_const_get(rb_cObject, rb_intern(module));
    } else {
      return NULL;
    }
  }

  ID fun_id = rb_intern(fun);
  if(Qundef != rb_check_funcall(module_val, fun_id, arity, (VALUE *)params)) {
    return (void*)rb_funcall2(module_val, fun_id, arity, (VALUE*)params);
  } 
  return NULL;
}

void * to_interpreter(ei_x_buff * x_buff) {
  long i;
  char *s;
  ei_term term;
  VALUE result = rb_funcall(rb_str_new_cstr("undefined_erlang_data_type"), rb_intern("to_sym"), 0);
  VALUE tmp;


  if(ei_decode_ei_term(x_buff->buff, &x_buff->index, &term) >= 0) {
    switch (term.ei_type) {
      case ERL_ATOM_EXT:
      case ERL_SMALL_ATOM_EXT:
      case ERL_ATOM_UTF8_EXT:
      case ERL_SMALL_ATOM_UTF8_EXT:
        LANGERL_LOG("===> ERL_*_ATOM_*");
        tmp = rb_str_new_cstr(term.value.atom_name);
        result = rb_funcall(tmp, rb_intern("to_sym"), 0);
        break;
      case ERL_SMALL_INTEGER_EXT:
      case ERL_INTEGER_EXT:
        LANGERL_LOG("===> ERL_*_INTEGER_*");
        result = LL2NUM(term.value.i_val);
        break;
      case ERL_FLOAT_EXT:
      case NEW_FLOAT_EXT:
        LANGERL_LOG("===> ERL_*_FLOAT_*");
        result = DBL2NUM(term.value.d_val);
        break;
      case ERL_STRING_EXT:
        LANGERL_LOG("===> ERL_*_STRING_* : %d", term.size);
        s = (char *)malloc(sizeof(char)*(term.size + 1));
        ei_decode_string(x_buff->buff, &x_buff->index, s);
        result = rb_ary_new2(term.size);
        for(i = 0; i < term.size; i++) {
          LANGERL_LOG("  ===> #%d : %d", i, s[i]);
          rb_ary_push(result, LL2NUM(s[i])); 
        }
        free(s);
        break;
      case ERL_BINARY_EXT:
        LANGERL_LOG("===> ERL_*_BINARY_*");
        s = (char *)malloc(sizeof(char)*term.size);
        ei_decode_binary(x_buff->buff, &x_buff->index, s, &i);
        result = rb_str_new(s, i);
        free(s);
        break;
      case ERL_SMALL_TUPLE_EXT:
      case ERL_LARGE_TUPLE_EXT:
        LANGERL_LOG("===> ERL_*_TUPLE_*");
        tmp = rb_ary_new2(term.arity);
        for(i = 0; i < term.arity; i++) {
          LANGERL_LOG("  => #%d", i);
          rb_ary_push(tmp, (VALUE)to_interpreter(x_buff));
        }
        result = langerl_tuple_new(cTuple, tmp);
        break;
      case ERL_LIST_EXT:
        LANGERL_LOG("===> ERL_*_LIST_* : %d", term.arity);
        result = rb_ary_new2(term.arity);
        for(i = 0; i < term.arity; i++) {
          LANGERL_LOG("  => #%d", i);
          rb_ary_push(result, (VALUE)to_interpreter(x_buff));
        }
        break;
      case ERL_NIL_EXT:
        LANGERL_LOG("===> ERL_*_NIL_*");
        result = rb_ary_new();
        break;
      case ERL_MAP_EXT:
        LANGERL_LOG("===> ERL_*_MAP_*");
        // TODO
        break;
      default:
        LANGERL_LOG("===> ERL_*_???_*");
        break;
    }
  }
  return (void*)result;
}

void ** to_interpreter_array(ei_x_buff *x_buff) {
  void **result = NULL;
  ei_term term;
  int i;
  char *s;

  if (ei_decode_ei_term(x_buff->buff, &x_buff->index, &term) < 0) {
    return NULL;
  }
  switch(term.ei_type) {
    case ERL_STRING_EXT: 
      LANGERL_LOG("CALL PARAMS is ERL_STRING_EXT : %d", term.size);
      s = (char *)malloc(sizeof(char)*(term.size + 1));
      result = (void **)malloc(sizeof(void *)*term.size);
      ei_decode_string(x_buff->buff, &x_buff->index, s);
      for(i = 0; i < term.size; i++) {
        LANGERL_LOG("  ===> #%d = %d", i, s[i]);
        result[i] = (void *)LL2NUM((long)s[i]);
      }
      free(s);
      break;
    case ERL_LIST_EXT:
      LANGERL_LOG("CALL PARAMS is ERL_LIST_EXT : %d", term.arity);
      result = (void **)malloc(sizeof(void *)*(term.arity));
      for(i = 0; i < term.arity; i++) {
        LANGERL_LOG("  => #%d", i);
        result[i] = to_interpreter(x_buff);
      }
      break;
    case ERL_NIL_EXT:
    default:
      LANGERL_LOG("CALL PARAMS is ERL_NIL_EXT");
      result = (void**)malloc(0);
  }
  LANGERL_LOG("CALL PARAMS is DONE");
  return result;
}

void to_erlang(ei_x_buff *x_out, void *data) {
  long i, j;
  char *str = NULL;

  VALUE rdata = (VALUE)data;
  switch (TYPE(rdata)) {
    case T_NIL:
      LANGERL_LOG("===> T_NIL");
      ei_x_encode_atom(x_out, "nil");
      break;
    case T_FLOAT:
      LANGERL_LOG("===> T_FLOAT");
      ei_x_encode_double(x_out, NUM2DBL(rdata));
      break;
    case T_STRING:
      LANGERL_LOG("===> T_STRING");
      str = StringValueCStr(rdata);
      ei_x_encode_binary(x_out, str, strlen(str));
      break;
    case T_ARRAY:
      i = RARRAY_LEN(rdata);
      LANGERL_LOG("===> T_ARRAY : %d", i);
      ei_x_encode_list_header(x_out, (int)i);
      for(j = 0; j < i; j++) {
        to_erlang(x_out, (void *)rb_ary_entry(rdata, j));
      }
      ei_x_encode_empty_list(x_out);
      break;
    case T_HASH:
      LANGERL_LOG("===> T_HASH");
      // TODO
      ei_x_encode_atom(x_out, "undefined_t_hash");
      break;
    case T_OBJECT:
    case T_DATA:
      LANGERL_LOG("===> T_OBJECT");
      if(CHECK_TUPLE(rdata)) {
        ruby_to_erlang_tuple(x_out, rdata);
      } else {
        ei_x_encode_atom(x_out, "undefined_t_object");
      }
      break;
    case T_BIGNUM:
      LANGERL_LOG("===> T_BIGNUM");
      ei_x_encode_longlong(x_out, NUM2LL(rdata));
      break;
    case T_FIXNUM:
      LANGERL_LOG("===> T_FIXNUM");
      ei_x_encode_long(x_out, NUM2LONG(rdata));
      break;
    case T_TRUE:
      LANGERL_LOG("===> T_TRUE");
      ei_x_encode_boolean(x_out, 1);
      break;
    case T_FALSE:
      LANGERL_LOG("===> T_FALSE");
      ei_x_encode_boolean(x_out, 0);
      break;
    case T_SYMBOL :
      LANGERL_LOG("===> T_SYMBOL");
      VALUE sym = rb_funcall(rdata, rb_intern("to_s"), 0);
      str = StringValueCStr(sym);
      ei_x_encode_atom(x_out, str);
      break;
    default:
      LANGERL_LOG("===> UNDEFINED: %d", TYPE(rdata));
      ei_x_encode_atom(x_out, "undefined_interpreter_data_type");
      break;
  }
  return;
}

void ruby_to_erlang_tuple(ei_x_buff *x_out, VALUE self) {
  TTuple *tuple;
  int size;
  int i;

  Data_Get_Struct(self, TTuple, tuple);
  size = RARRAY_LEN(tuple->data);

  ei_x_encode_tuple_header(x_out, size);
  for(i = 0; i < size; i++) {
    to_erlang(x_out, (void *)rb_ary_entry(tuple->data, i));
  }
}
