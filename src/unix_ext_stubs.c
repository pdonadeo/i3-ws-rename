/* This binding to getloadavg has been stoled by Jane Street Shell library:
   https://github.com/janestreet/shell
*/

#include <stdlib.h>
#include <math.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>


#define __unused __attribute__ ((unused))


CAMLprim value getloadavg_stub (value v_unit __unused)
{
    CAMLparam0();
    CAMLlocal1(v_ret);
    double loadavg[3];
    int ret = getloadavg(loadavg,3);
    if (ret < 0) uerror("getloadavg",Nothing);
    v_ret = caml_alloc_tuple(3);
    Store_field(v_ret, 2, caml_copy_double(ret >= 3 ? loadavg[2] : NAN));
    Store_field(v_ret, 1, caml_copy_double(ret >= 2 ? loadavg[1] : NAN));
    Store_field(v_ret, 0, caml_copy_double(ret >= 1 ? loadavg[0] : NAN));
    CAMLreturn(v_ret);
}