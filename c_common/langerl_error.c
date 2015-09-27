#include "langerl_error.h"

void error_msg(ei_x_buff *x_out, const char *reason) {
  x_out->index = 0;
  ei_x_encode_version(x_out);
  ei_x_encode_tuple_header(x_out, 2);
  ei_x_encode_atom(x_out, "error");
  ei_x_encode_atom(x_out, reason);
}
