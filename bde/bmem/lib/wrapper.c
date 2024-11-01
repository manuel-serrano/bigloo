/*=====================================================================*/
/*    serrano/bigloo-nan/bde/bmem/lib/wrapper.c                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  7 19:30:50 2021                          */
/*    Last change :  Fri Nov  1 00:22:05 2024 (serrano)                */
/*    Copyright   :  2021-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo standard library wrappers                                 */
/*=====================================================================*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>

#include <bigloo.h>
#include <bmem.h>
#include <wrapper.h>

/* pair */
void *(*____make_pair)() = 0L;

/* vectors */
void *(*____create_vector)() = 0L;
void *(*____create_vector_uncollectable)() = 0L;

/* procedures */
void *(*____make_fx_procedure)() = 0L;
void *(*____make_va_procedure)() = 0L;

/* internals */
void *(*____make_dynamic_env)() = 0L;

/* cell */
void *(*____make_cell)() = 0L;

/* strings */
void *(*____string_to_bstring_len)() = 0L;
void *(*____string_to_bstring)() = 0L;
void *(*____make_string)() = 0L;
void *(*____make_string_sans_fill)() = 0L;
void *(*____string_append)() = 0L;
void *(*____c_substring)() = 0L;

void *(*____make_ucs2_string)() = 0L;
   
/* symbol */
void *(*____bstring_to_symbol)() = 0L;

/* keyword */
void *(*____bstring_to_keyword)() = 0L;

/* real */
#if (!BGL_NAN_TAGGING)
void *(*____make_real)() = 0L;
#endif

/* threads & locks */
void *(*____bgl_make_mutex)() = 0L;
void *(*____bgl_make_spinlock)() = 0L;

/* ports */
void *(*____bgl_open_output_string)() = 0L;
void *(*____bgl_make_output_port)() = 0L;
void *(*____bgl_make_input_port)() = 0L;

/* classes */
void *(*____bgl_make_class)() = 0L;
void *(*____create_struct)() = 0L;

/* bignums */
void *(*____bgl_bignum_add)() = 0L;
void *(*____bgl_bignum_sub)() = 0L;
void *(*____bgl_bignum_mul)() = 0L;
void *(*____bgl_bignum_div)() = 0L;
void *(*____bgl_bignum_expt)() = 0L;
void *(*____bgl_bignum_quotient)() = 0L;
void *(*____bgl_bignum_remainder)() = 0L;
void *(*____bgl_bignum_or)() = 0L;
void *(*____bgl_bignum_xor)() = 0L;
void *(*____bgl_bignum_and)() = 0L;
void *(*____bgl_bignum_mask)() = 0L;
void *(*____bgl_bignum_not)() = 0L;
void *(*____bgl_long_to_bignum)() = 0L;
void *(*____bgl_bignum_lsh)() = 0L;
void *(*____bgl_bignum_rsh)() = 0L;
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bmem_init_wrapper ...                                            */
/*---------------------------------------------------------------------*/
void
bmem_init_wrapper(void *hdl) {
   /* pair */
   LOADF(hdl, make_pair);
   
   /* vector */
   LOADF(hdl, create_vector);
   LOADF(hdl, create_vector_uncollectable);

   /* cell */
   LOADF(hdl, make_cell);

   /* procedure */
   LOADF(hdl, make_fx_procedure);
   LOADF(hdl, make_va_procedure);
   
   /* internals */
   LOADF(hdl, make_dynamic_env);

   /* cell */
   LOADF(hdl, make_cell);

   /* strings */
   LOADF(hdl, string_to_bstring_len);
   LOADF(hdl, string_to_bstring);
   LOADF(hdl, make_string_sans_fill);
   LOADF(hdl, make_string);
   LOADF(hdl, string_append);
   LOADF(hdl, c_substring);

   LOADF(hdl, make_ucs2_string);
   
   /* symbol */
   LOADF(hdl, bstring_to_symbol);

   /* keyword */
   LOADF(hdl, bstring_to_keyword);

   /* real */
#if (!BGL_NAN_TAGGING)
   LOADF(hdl, make_real);
#endif
   
   /* threads & locks */
   LOADF(hdl, bgl_make_mutex);
   LOADF(hdl, bgl_make_spinlock);
   
   /* port */
   LOADF(hdl, bgl_open_output_string);
   LOADF(hdl, bgl_make_output_port);
   LOADF(hdl, bgl_make_input_port);

   /* classes and structs */
   LOADF(hdl, bgl_make_class);
   LOADF(hdl, create_struct);

   /* bignum */
   LOADF(hdl, bgl_bignum_add);
   LOADF(hdl, bgl_bignum_sub);
   LOADF(hdl, bgl_bignum_mul);
   LOADF(hdl, bgl_bignum_div);
   LOADF(hdl, bgl_bignum_expt);
   LOADF(hdl, bgl_bignum_quotient);
   LOADF(hdl, bgl_bignum_remainder);
   LOADF(hdl, bgl_bignum_mask);
   LOADF(hdl, bgl_bignum_and);
   LOADF(hdl, bgl_bignum_or);
   LOADF(hdl, bgl_bignum_xor);
   LOADF(hdl, bgl_bignum_not);
   LOADF(hdl, bgl_long_to_bignum);
   LOADF(hdl, bgl_bignum_lsh);
   LOADF(hdl, bgl_bignum_rsh);
   
   /* types by function names */
   MARKF("bgl_bstring_to_symbol", SYMBOL_TYPE_NUM);
   MARKF("bgl_init_process_table", IGNORE_TYPE_NUM);
   MARKF("bgl_init_objects", IGNORE_TYPE_NUM);
   MARKF("make_bignum", BIGNUM_TYPE_NUM);
   MARKF("bgl_make_nil_mutx", MUTEX_TYPE_NUM);
   MARKF("bgl_escape_C_string", STRING_TYPE_NUM);
}

/*---------------------------------------------------------------------*/
/*    WRAPPERS ...                                                     */
/*---------------------------------------------------------------------*/
/* pair */
WRAP(make_pair,
     PAIR_TYPE_NUM,
     (obj_t a, obj_t d),
     (a, d));

/* vector */
WRAP(create_vector,
     VECTOR_TYPE_NUM,
     (long len),
     (len));
WRAP(create_vector_uncollectable,
     VECTOR_TYPE_NUM,
     (long len),
     (len));

/* procedure */
WRAP(make_fx_procedure,
     PROCEDURE_TYPE_NUM,
     (obj_t (*e)(), int a, int s),
     ((void *(*)())e, a, s));
WRAP(make_va_procedure,
     PROCEDURE_TYPE_NUM,
     (obj_t (*e)(), int a, int s),
     ((void *(*)())e, a, s));

/* internal */
WRAP(make_dynamic_env,
     _DYNAMIC_ENV_TYPE_NUM,
     (),
     ());

/* cell */
WRAP(make_cell,
     CELL_TYPE_NUM,
     (obj_t o),
     (o));

/* strings */
WRAP(string_to_bstring_len,
     STRING_TYPE_NUM,
     (char *s, int l),
     (s, l));
WRAP(string_to_bstring,
     STRING_TYPE_NUM,
     (char *s),
     (s));
WRAP(make_string,
     STRING_TYPE_NUM,
     (long l, unsigned char c),
     (l, c));
WRAP(make_string_sans_fill,
     STRING_TYPE_NUM,
     (long l),
     (l));
WRAP(string_append,
     STRING_TYPE_NUM,
     (obj_t s1, obj_t s2),
     (s1, s2));
WRAP(c_substring,
     STRING_TYPE_NUM,
     (obj_t s, long i, long a),
     (s, i, a));

WRAP(make_ucs2_string,
     UCS2_STRING_TYPE_NUM,
     (int len, ucs2_t c),
     (len, c));

/* symbol */
WRAP(bstring_to_symbol,
     SYMBOL_TYPE_NUM,
     (obj_t n),
     (n));

/* keyword */
WRAP(bstring_to_keyword,
     KEYWORD_TYPE_NUM,
     (obj_t n),
     (n));

/* real */
#if (!BGL_NAN_TAGGING)
WRAP(make_real,
     REAL_TYPE_NUM,
     (double d),
     (d));
#endif

/* threads & locks */
WRAP(bgl_make_mutex,
     MUTEX_TYPE_NUM,
     (obj_t n),
     (n));
WRAP(bgl_make_spinlock,
     MUTEX_TYPE_NUM,
     (obj_t n),
     (n));

/* ports */
WRAP(bgl_open_output_string,
     OUTPUT_STRING_PORT_TYPE_NUM,
     (obj_t b),
     (b));
WRAP(bgl_make_output_port,
     OUTPUT_PORT_TYPE_NUM,
     (obj_t n, bgl_stream_t s, int t, obj_t k, obj_t b,
      ssize_t (*write)(), long (*seek)(), int (*close)()),
     (n, s, t, k, b, write, seek, close));
WRAP(bgl_make_input_port,
     INPUT_PORT_TYPE_NUM,
     (obj_t name, FILE *file, obj_t kindof, obj_t buf),
     (name, file, kindof, buf));
     
/* classes */
WRAP(bgl_make_class,
     CLASS_TYPE_NUM,
     (obj_t name, obj_t module, long num, long idx,
      obj_t super, obj_t sub,
      obj_t alloc, long hash,
      obj_t fd, obj_t allfd,
      obj_t constr, obj_t virt, obj_t new, obj_t nil, obj_t shrink,
      long depth, 
      obj_t evdata),
     (name, module, num, idx, super, sub, alloc, hash,
      fd, allfd, constr, virt, new, nil, shrink, depth, evdata));
WRAP(create_struct,
     STRUCT_TYPE_NUM,
     (obj_t key, int len),
     (key, len));

/* bignum */
WRAP(bgl_bignum_add,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_sub,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_mul,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_div,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_expt,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_quotient,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_remainder,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_mask,
     BIGNUM_TYPE_NUM,
     (obj_t x, long y),
     (x, y));
WRAP(bgl_bignum_and,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_or,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_xor,
     BIGNUM_TYPE_NUM,
     (obj_t x, obj_t y),
     (x, y));
WRAP(bgl_bignum_not,
     BIGNUM_TYPE_NUM,
     (obj_t x),
     (x));
WRAP(bgl_long_to_bignum,
     BIGNUM_TYPE_NUM,
     (long l),
     (l));
WRAP(bgl_bignum_lsh,
     BIGNUM_TYPE_NUM,
     (obj_t x, long y),
     (x, y));
WRAP(bgl_bignum_rsh,
     BIGNUM_TYPE_NUM,
     (obj_t x, long y),
     (x, y));
