/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cbignum.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  JosÃ© Romildo Malaquias                           */
/*    Creation    :  Fri Nov 10 11:51:17 2006                          */
/*    Last change :  Thu Dec 12 09:52:45 2024 (serrano)                */
/*    Copyright   :  2003-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C implementation of bignum                                       */
/*=====================================================================*/
#include <limits.h>
#include <errno.h>
#include <string.h>
#include <bigloo.h>

static obj_t bgl_belongzero, bgl_bllongzero;

#if (BGL_HAVE_GMP)   
static mpz_t onez;
#endif

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_init_bignum ...                                              */
/*---------------------------------------------------------------------*/
void
bgl_init_bignum() {
   bgl_belongzero = make_belong(0);
   bgl_bllongzero = make_bllong(0);

#if (BGL_HAVE_GMP)   
   mpz_init_set_si(onez, 1);
#endif
}

#if (BGL_HAVE_GMP && !BGL_BOOT)   

/* field access macros.  */
#define SIZ(x) ((x)->_mp_size)
#define ABSIZ(x) BXABS(SIZ(x))
#define LIMBS(x) ((x)->_mp_d)
#define ALLOC(x) ((x)->_mp_alloc)

#define BXABS(x) ((x) >= 0 ? (x) : -(x))
#define BXABSIZ(x) BXABS(BXSIZ (x))
#define BXALLOC(x) (BIGNUM(x).mpz._mp_alloc)
#define BXLIMBS(x) (BIGNUM(x).mpz._mp_d)

#define ZERO(x) (SIZ(x) == 0)
#define POSITIVE(x) (SIZ(x) > 0)
#define NEGATIVE(x) (SIZ(x) < 0)

#define BIGNUM_ALLOC_SIZE(sz) \
   (BIGNUM_SIZE + ((sz) * sizeof(mp_limb_t)))

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    make_bignum ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
make_bignum(size_t sz) {
   obj_t o = GC_MALLOC_ATOMIC(BIGNUM_ALLOC_SIZE(sz));
   
   o->bignum.header = BGL_MAKE_HEADER(BIGNUM_TYPE, 0);
   o->bignum.mpz._mp_d = (mp_limb_t *)&(o->bignum.mp_d);
   o->bignum.mpz._mp_alloc = sz;

   return BREF(o);
}

#define MAKE_STACK_BIGNUM(o, sz) \
   (o = alloca(BIGNUM_ALLOC_SIZE(sz)), \
      o->bignum.header = BGL_MAKE_HEADER(BIGNUM_TYPE, 0), \
      o->bignum.mpz._mp_d = (mp_limb_t *)&(o->bignum.mp_d), \
      o->bignum.mpz._mp_alloc = sz, \
    BREF(o))
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bignum_set_size ...                                              */
/*---------------------------------------------------------------------*/
static void
bignum_set_size(obj_t x, const int size) {
   int count = size - 1;
   
   while (count > 0 && BXLIMBS(x)[count] == 0)
      count--;
   if (count == 0 && BXLIMBS(x)[0] == 0)
      BXSIZ(x) = 0;
   else
      BXSIZ(x) = count + 1;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    mpz_to_bignum ...                                                */
/*---------------------------------------------------------------------*/
static obj_t
mpz_to_bignum(const mpz_t z) {
   obj_t bignum;

   if (SIZ(z) == 0) {
      bignum = make_bignum(ALLOC(z));
      memcpy(BXLIMBS(bignum), LIMBS(z), ALLOC(z) * sizeof(mp_limb_t));
   }
   else {
      bignum = make_bignum(ABSIZ(z));
      memcpy(BXLIMBS(bignum), LIMBS(z), ABSIZ(z) * sizeof(mp_limb_t));
   }
   BXSIZ(bignum) = SIZ(z);

   return bignum;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_long_to_bignum ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_long_to_bignum(const long n) {
#if (BGL_LONGLONG_LIMBS == 1)
   obj_t x = make_bignum(BGL_LONG_LIMBS);
   
   if (n < 0) {
      BXLIMBS(x)[0] = (mp_limb_t)(unsigned long int) -n;
      BXSIZ(x) = -1;
   } else {
      BXLIMBS(x)[0] = (mp_limb_t) (unsigned long int) n;
      BXSIZ(x) = n!=0;
   }
#else
   obj_t x;
   mpz_t z;
   
   mpz_init_set_si(z, n);
   x = mpz_to_bignum(z);
   mpz_clear(z);
#endif
   
   return x;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_llong_to_bignum ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_llong_to_bignum(const BGL_LONGLONG_T n) {
#if (BGL_ELONG_BIT_SIZE > 32)
   return bgl_long_to_bignum((long)n);
#else
   obj_t x = make_bignum(sizeof(n));
   
   mpz_import(&(BIGNUM(x).mpz), 1, 1, sizeof(n), 0, 0, &n);
   if (n < 0) mpz_neg(&(BIGNUM(x).mpz), &(BIGNUM(x).mpz));
   
   return x;
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_int64_to_bignum ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_int64_to_bignum(const int64_t n) {
#if (BGL_ELONG_BIT_SIZE > 32)
   return bgl_long_to_bignum((long)n);
#else
   obj_t x = make_bignum(sizeof(n));
   
   mpz_import(&(BIGNUM(x).mpz), 1, 1, sizeof(n), 0, 0, &n);
   if (n < 0) mpz_neg(&(BIGNUM(x).mpz), &(BIGNUM(x).mpz));
   
   return x;
#endif
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_uint64_to_bignum ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_uint64_to_bignum(const uint64_t n) {
#if (BGL_ELONG_BIT_SIZE > 32)
   obj_t x = make_bignum(BGL_LONGLONG_LIMBS);
   
#  if (BGL_LONGLONG_LIMBS == 1)
   BXLIMBS(x)[0] = (mp_limb_t) (uint64_t) n;
   BXSIZ(x) = (n!=0);
#  else
   mp_size_t size = 0;
   uint64_t vl = (uint64_t) (n < 0 ? -n : n);
      
   do {
      BXLIMBS(x)[size] = (mp_limb_t) (vl & GMP_NUMB_MASK);
      size++;
      vl >>= GMP_NUMB_BITS;
   } while (vl);

   BXSIZ(x) = n > 0 ? size : (n < 0 ? -size : 0);
#  endif
   
   return x;
#else
   obj_t x = make_bignum(sizeof(n));
   
   mpz_import(&(BIGNUM(x).mpz), 1, 1, sizeof(n), 0, 0, &n);
   
   return x;
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_flonum_to_bignum ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_flonum_to_bignum(double r) {
   obj_t x;
   mpz_t z;
   
   mpz_init_set_d(z, r);
   x = mpz_to_bignum(z);
   mpz_clear(z);
   
   return x;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF long                                             */
/*    bgl_bignum_to_long ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_bignum_to_long(obj_t x) {
   return mpz_get_si(&(BIGNUM(x).mpz));
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF BGL_LONGLONG_T                                   */
/*    bgl_bignum_to_llong ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BGL_LONGLONG_T
bgl_bignum_to_llong(obj_t x) {
#if (BGL_ELONG_BIT_SIZE > 32)
   return (BGL_LONGLONG_T)mpz_get_si(&(BIGNUM(x).mpz));
#else
   const int size = sizeof(BGL_LONGLONG_T);
   const int numb = 8 * size - 0;
   int count = (mpz_sizeinbase(&(BIGNUM(x).mpz), 2) + numb-1) / numb;
   const BGL_LONGLONG_T *i = alloca(count * size);

   mpz_export((void *)i, &count, 1, sizeof(BGL_LONGLONG_T), 0, 0, &(BIGNUM(x).mpz));

   return i[count - 1];
#endif
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int64_t                                          */
/*    bgl_bignum_to_int64 ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int64_t
bgl_bignum_to_int64(obj_t x) {
#if (BGL_ELONG_BIT_SIZE > 32)
   return (int64_t)mpz_get_si(&(BIGNUM(x).mpz));
#else   
   const int size = sizeof(int64_t);
   const int numb = 8 * size - 0;
   int count = (mpz_sizeinbase(&(BIGNUM(x).mpz), 2) + numb-1) / numb;
   const int64_t *i = alloca(count * size);

   mpz_export((void *)i, &count, 1, sizeof(int64_t), 0, 0, &(BIGNUM(x).mpz));

   return i[count - 1];
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF uint64_t                                         */
/*    bgl_bignum_to_uint64 ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF uint64_t
bgl_bignum_to_uint64(obj_t x) {
#if (BGL_ELONG_BIT_SIZE > 32)
   return (uint64_t)mpz_get_ui(&(BIGNUM(x).mpz));
#else
   const int size = sizeof(uint64_t);
   const int numb = 8 * size - 0;
   int count = (mpz_sizeinbase(&(BIGNUM(x).mpz), 2) + numb-1) / numb;
   const uint64_t *i = alloca(count * size);

   mpz_export((void *)i, &count, 1, sizeof(uint64_t), 0, 0, &(BIGNUM(x).mpz));

   return i[count - 1];
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF double                                           */
/*    bgl_bignum_to_flonum ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF double
bgl_bignum_to_flonum(obj_t x) {
   return mpz_get_d(&(BIGNUM(x).mpz));
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTMIE_DEF obj_t                                            */
/*    bgl_string_to_bignum ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_string_to_bignum(char *str, int radix) {
   obj_t bignum;
   mpz_t z;

   mpz_init_set_str(z, str, radix);
   bignum = mpz_to_bignum(z);
   mpz_clear(z);
   
   return bignum;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_string_to_integer_obj ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_string_to_integer_obj(char *str, int radix) {
   long x;

   errno = 0;
   x = strtol(str, NULL, radix);
   
   if ((errno == ERANGE) && (x == LONG_MAX || x == LONG_MIN)) {
      return bgl_string_to_bignum(str, radix);
   } else {
      obj_t n = BINT(x);
      if (CINT(n) != x)
	 return bgl_long_to_bignum(x);
      else
	 return n;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_to_string ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_to_string(obj_t obj, int radix) {
   obj_t str;
   char *buffer = alloca(mpz_sizeinbase(&(BIGNUM(obj).mpz), radix) + 2);

   mpz_get_str(buffer, radix, &(BIGNUM(obj).mpz));
   str = string_to_bstring(buffer);
   
   return str;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bignum_cmp_aux ...                                               */
/*    -------------------------------------------------------------    */
/*    assumes x > 0 and y > 0                                          */
/*---------------------------------------------------------------------*/
static int
bignum_cmp_aux(const mp_limb_t *x, const int size_x,
	       const mp_limb_t *y, const int size_y) {
   if (size_x < size_y)
      return -1;
   else if (size_x > size_y)
      return 1;
   else
      return mpn_cmp(x, y, size_x);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int                                              */
/*    bgl_bignum_cmp ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_bignum_cmp(obj_t x, obj_t y) {
   if (BXPOSITIVE(x))
      if (BXPOSITIVE(y))
	 return bignum_cmp_aux(BXLIMBS(x), BXSIZ(x),
			       BXLIMBS(y), BXSIZ(y));
      else
	 return 1;
   else if (BXNEGATIVE(x))
      if (BXNEGATIVE(y))
	 return bignum_cmp_aux(BXLIMBS(y), - BXSIZ(y),
			       BXLIMBS(x), - BXSIZ(x));
      else
	 return -1;
   else
      if (BXPOSITIVE(y))
	 return -1;
      else if (BXNEGATIVE(y))
	 return 1;
      else
	 return 0;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int                                              */
/*    bgl_bignum_even ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_bignum_even(obj_t x) {
   return mpz_even_p(&(BIGNUM(x).mpz));
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int                                              */
/*    bgl_bignum_odd ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_bignum_odd(obj_t x) {
   return mpz_odd_p(&(BIGNUM(x).mpz));
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_neg ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_neg(obj_t x) {
   if (BXPOSITIVE(x)) {
      obj_t y = make_bignum(BXSIZ(x));
      memcpy(BXLIMBS(y), BXLIMBS(x), BXSIZ(x) * sizeof(mp_limb_t));
      BXSIZ(y) = -BXSIZ(x);
      return y;
   } else if (BXNEGATIVE(x)) {
      obj_t y = make_bignum(-BXSIZ(x));
      memcpy(BXLIMBS(y), BXLIMBS(x), - BXSIZ(x) * sizeof(mp_limb_t));
      BXSIZ(y) = -BXSIZ(x);
      return y;
   } else
      return x;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_abs ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_abs(obj_t x) {
   if (BXNEGATIVE(x)) {
      obj_t y = make_bignum(-BXSIZ(x));
      memcpy(BXLIMBS(y), BXLIMBS(x), -BXSIZ(x) * sizeof(mp_limb_t));
      BXSIZ(y) =- BXSIZ(x);
      return y;
   } else
      return x;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bignum_add_pos_pos_aux ...                                       */
/*    -------------------------------------------------------------    */
/*    assumes x > 0 and y > 0 and size_x >= size_y                     */
/*---------------------------------------------------------------------*/
static obj_t
bignum_add_pos_pos_aux(const mp_limb_t *x, const int size_x,
		       const mp_limb_t *y, const int size_y) {
   obj_t z = make_bignum(size_x + 1);
   const int carry = mpn_add(BXLIMBS(z), x, size_x, y, size_y);

   if (carry) {
/*       obj_t o = GC_REALLOC(CREF(z), BIGNUM_ALLOC_SIZE(size_x + 1)); */
/*       o->bignum.mpz._mp_d = (mp_limb_t *)&(o->bignum.mp_d);         */
/*       z = BREF(o);                                                  */
      BXLIMBS(z)[size_x] = carry;
      BXSIZ(z) = BXALLOC(z) = size_x + 1;
   } else {
      BXSIZ(z) = BXALLOC(z) = size_x;
   }
   
   return z;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bignum_add_pos_pos ...                                           */
/*    -------------------------------------------------------------    */
/*    assumes x > 0 and y > 0                                          */
/*---------------------------------------------------------------------*/
static obj_t
bignum_add_pos_pos(const mp_limb_t *x, const int size_x,
		   const mp_limb_t *y, const int size_y) {
   if (size_x >= size_y) {
      return bignum_add_pos_pos_aux(x, size_x, y, size_y);
   } else {
      return bignum_add_pos_pos_aux(y, size_y, x, size_x);
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bignum_add_pos_neg_aux ...                                       */
/*    -------------------------------------------------------------    */
/*    assumes x > 0 and y < 0 and |x| > |y|                            */
/*---------------------------------------------------------------------*/
static obj_t
bignum_add_pos_neg_aux(const mp_limb_t *x, const int size_x,
		       const mp_limb_t *y, const int size_y) {
   int count;
   obj_t z = make_bignum(size_x);
   const int borrow = mpn_sub(BXLIMBS(z), x, size_x, y, size_y);
   
   assert(borrow == 0);
   
   count = size_x - 1;
   while (count > 0 && BXLIMBS(z)[count] == 0)
      count--;
   count++;
   
   if (count != size_x) {
/*       obj_t o = GC_REALLOC(CREF(z), BIGNUM_ALLOC_SIZE(count));      */
/*       o->bignum.mpz._mp_d = (mp_limb_t *)&(o->bignum.mp_d);         */
/*       z = BREF(o);                                                  */
      BXALLOC(z) = count;
   }
   
   if (count == 1 && BXLIMBS(z)[0] == 0)
      BXSIZ(z) = 0;
   else
      BXSIZ(z) = count;
   
   return z;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bignum_add_neg_pos_aux ...                                       */
/*    -------------------------------------------------------------    */
/*    assumes x < 0 and y > 0 and |x| > |y|                            */
/*---------------------------------------------------------------------*/
static obj_t
bignum_add_neg_pos_aux(const mp_limb_t *x, const int size_x,
			const mp_limb_t *y, const int size_y) {
   obj_t z = bignum_add_pos_neg_aux(x, size_x, y, size_y);
   BXSIZ(z) = -BXSIZ(z);
   return z;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bignum_add_pos_neg ...                                           */
/*    -------------------------------------------------------------    */
/*    assumes x > 0 and y < 0                                          */
/*---------------------------------------------------------------------*/
static obj_t
bignum_add_pos_neg(const mp_limb_t *x, const int size_x,
		   const mp_limb_t *y, const int size_y) {
   if (size_x > size_y)
      return bignum_add_pos_neg_aux(x, size_x, y, size_y);
   else if (size_x < size_y)
      return bignum_add_neg_pos_aux(y, size_y, x, size_x);
   else {
      const int test = mpn_cmp(x, y, size_x);
      if (test > 0)
	 return bignum_add_pos_neg_aux(x, size_x, y, size_y);
      else if (test < 0)
	 return bignum_add_neg_pos_aux(y, size_y, x, size_x);
      else
	 return bgl_long_to_bignum(0L);
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bignum_add_neg_neg ...                                           */
/*    -------------------------------------------------------------    */
/*    assumes x < 0 and y < 0                                          */
/*---------------------------------------------------------------------*/
static obj_t
bignum_add_neg_neg(const mp_limb_t *x, const int size_x,
		   const mp_limb_t *y, const int size_y) {
   obj_t z = bignum_add_pos_pos(x, size_x, y, size_y);
   BXSIZ(z) = -BXSIZ(z);
   return z;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_add ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_add(obj_t x, obj_t y) {
   if (BXPOSITIVE(x))
      if (BXPOSITIVE(y))
	 return bignum_add_pos_pos(BXLIMBS(x), BXSIZ(x),
				   BXLIMBS(y), BXSIZ(y));
      else if (BXNEGATIVE(y))
	 return bignum_add_pos_neg(BXLIMBS(x), BXSIZ(x),
				   BXLIMBS(y), -BXSIZ(y));
      else
	 return x;
   else if (BXNEGATIVE(x))
      if (BXPOSITIVE(y))
	 return bignum_add_pos_neg(BXLIMBS(y), BXSIZ(y),
				   BXLIMBS(x), -BXSIZ(x));
      else if (BXNEGATIVE(y))
	 return bignum_add_neg_neg(BXLIMBS(x), -BXSIZ(x),
				   BXLIMBS(y), -BXSIZ(y));
      else
	 return x;
   else
      return y;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_sub ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_sub(obj_t x, obj_t y) {
   if (BXPOSITIVE(x)) {
      if (BXPOSITIVE(y)) {
	 return bignum_add_pos_neg(BXLIMBS(x), BXSIZ(x),
				   BXLIMBS(y), BXSIZ(y));
      } else if (BXNEGATIVE(y)) {
	 return bignum_add_pos_pos(BXLIMBS(x), BXSIZ(x),
				   BXLIMBS(y), -BXSIZ(y));
      } else {
	 return x;
      }
   } else if (BXNEGATIVE(x)) {
      if (BXPOSITIVE(y)) {
	 return bignum_add_neg_neg(BXLIMBS(x), -BXSIZ(x),
				   BXLIMBS(y), BXSIZ(y));
      } else if (BXNEGATIVE(y)) {
	 return bignum_add_pos_neg(BXLIMBS(y), -BXSIZ(y),
				   BXLIMBS(x), -BXSIZ(x));
      } else {
	 return x;
      }
   } else {
      return bgl_bignum_neg(y);
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_mul ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_mul(obj_t x, obj_t y) {
   const int size_x = BXABSIZ(x);
   const int size_y = BXABSIZ(y);
   
   if ((size_y == 0) || (size_x == 0)) {
      return bgl_long_to_bignum(0);
   } else {
      const int size_z = size_x + size_y;
      obj_t z = make_bignum(size_z);
      
      if (size_x >= size_y)
	 mpn_mul(BXLIMBS(z), BXLIMBS(x), size_x, BXLIMBS(y), size_y);
      else
	 mpn_mul(BXLIMBS(z), BXLIMBS(y), size_y, BXLIMBS(x), size_x);
      if (BXLIMBS(z)[size_z - 1] == 0)
	 BXSIZ(z) = size_z - 1;
      else
	 BXSIZ(z) = size_z;
      if (BXPOSITIVE(x)) {
	 if (BXNEGATIVE(y))
	    BXSIZ(z) = -BXSIZ(z);
      }
      else if (BXNEGATIVE(x))
	 if (BXPOSITIVE(y))
	    BXSIZ(z) = -BXSIZ(z);
      return z;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_expt ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_expt(obj_t x, obj_t y) {
   obj_t z;
   mpz_t z1;
   
   mpz_init(z1);
   
   mpz_pow_ui(z1, &(BIGNUM(x).mpz), (uint)bgl_bignum_to_long(y));
   z = mpz_to_bignum(z1);
   
   return z;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_quotient ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_quotient(obj_t x, obj_t y) {
   obj_t q;
   const int size_x = BXABSIZ(x);
   const int size_y = BXABSIZ(y);
   if (size_x < size_y)
      q = bgl_long_to_bignum(0L);
   else {
      obj_t r;
      const int size_q = size_x - size_y + 1;
      q = make_bignum(size_q);
      r = MAKE_STACK_BIGNUM(r, size_y);
      mpn_tdiv_qr(BXLIMBS(q), BXLIMBS(r), 0,
		   BXLIMBS(x), size_x, BXLIMBS(y), size_y);
      bignum_set_size(q, size_q);
      if (BXNEGATIVE(x)) {
	 if (BXPOSITIVE(y))
	    BXSIZ(q) = -BXSIZ(q);
      }
      else if (BXPOSITIVE(x))
	 if (BXNEGATIVE(y))
	    BXSIZ(q) = -BXSIZ(q);
   }

   return q;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_remainder ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_remainder(obj_t x, obj_t y) {
   obj_t r;
   const int size_x = BXABSIZ(x);
   const int size_y = BXABSIZ(y);
   if (size_x < size_y) {
      r = x;
   } else {
      obj_t q;
      const int size_q = size_x - size_y + 1;
      q = MAKE_STACK_BIGNUM(q, size_q);
      r = make_bignum(size_y);
      mpn_tdiv_qr(BXLIMBS(q), BXLIMBS(r), 0,
		  BXLIMBS(x), size_x, BXLIMBS(y), size_y);
      bignum_set_size(r, size_y);
      if (BXNEGATIVE(x)) {
	 BXSIZ(r) = -BXSIZ(r);
      }
   }

   return r;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_div ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_div(obj_t x, obj_t y) {
   obj_t q;
   obj_t r;
   const int size_x = BXABSIZ(x);
   const int size_y = BXABSIZ(y);
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();

   if (size_x < size_y) {
      q = bgl_long_to_bignum(0L);
      r = x;
   } else {
      const int size_q = size_x - size_y + 1;
      
      q = make_bignum(size_q);
      r = make_bignum(size_y);
      
      mpn_tdiv_qr(BXLIMBS(q), BXLIMBS(r), 0,
		  BXLIMBS(x), size_x, BXLIMBS(y), size_y);
      bignum_set_size(q, size_q);
      bignum_set_size(r, size_y);
      
      if (BXNEGATIVE(x)) {
	 BXSIZ(r) = -BXSIZ(r);
	 if (BXPOSITIVE(y))
	    BXSIZ(q) = -BXSIZ(q);
      } else if (BXPOSITIVE(x))
	 if (BXNEGATIVE(y))
	    BXSIZ(q) = -BXSIZ(q);
   }

   BGL_ENV_MVALUES_NUMBER_SET(env, 2);
   BGL_ENV_MVALUES_VAL_SET(env, 1, r);
   
   return q;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_gcd ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_gcd(obj_t x, obj_t y) {
   obj_t z;
   mpz_t x1, y1, z1;
   
   mpz_init_set(x1, &(BIGNUM(x).mpz));
   mpz_init_set(y1, &(BIGNUM(y).mpz));
   mpz_init(z1);
   
   mpz_gcd(z1, x1, y1);
   z = mpz_to_bignum(z1);
   
   mpz_clear(x1);
   mpz_clear(y1);
   mpz_clear(z1);
   
   return z;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_lcm ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_lcm(obj_t x, obj_t y) {
   obj_t z;
   mpz_t x1, y1, z1;
   
   mpz_init_set(x1, &(BIGNUM(x).mpz));
   mpz_init_set(y1, &(BIGNUM(y).mpz));
   mpz_init(z1);
   
   mpz_lcm(z1, x1, y1);
   z = mpz_to_bignum(z1);
   
   mpz_clear(x1);
   mpz_clear(y1);
   mpz_clear(z1);
   
   return z;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_safe_bignum_to_fixnum ...                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_safe_bignum_to_fixnum(obj_t bx) {
   size_t bs = mpz_sizeinbase(&(BIGNUM(bx).mpz), 2);

   if (bs < BGL_INT_BIT_SIZE) {
      return BINT(bgl_bignum_to_long(bx));
   } else {
      return bx;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_plus_fx ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_plus_fx(long x, long y) {
#if BGL_HAVE_OVERFLOW
   long z;
   if (__builtin_saddl_overflow(x, y, &z))
#else   
   long z = x + y;
   if ((x & C_LONG_SIGN_BIT) == (y & C_LONG_SIGN_BIT) &&
       (z & C_LONG_SIGN_BIT) != (x & C_LONG_SIGN_BIT))
#endif   
      return bgl_bignum_add(bgl_long_to_bignum(x), bgl_long_to_bignum(y));
   else
      return BINT(z);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_minus_fx ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_minus_fx(long x, long y) {
#if BGL_HAVE_OVERFLOW
   long z;
   if (__builtin_ssubl_overflow(x, y, &z))
#else   
   long z = x - y;
   if ((x & C_LONG_SIGN_BIT) != (y & C_LONG_SIGN_BIT) &&
       (z & C_LONG_SIGN_BIT) != (x & C_LONG_SIGN_BIT))
#endif
      return bgl_bignum_sub(bgl_long_to_bignum(x), bgl_long_to_bignum(y));
   else
      return BINT(z);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_mul_fx ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_mul_fx(long x, long y) {
#if BGL_HAVE_OVERFLOW
      long z;
      if (__builtin_smull_overflow(x, y, &z))
	 return bgl_bignum_mul(bgl_long_to_bignum(x),
			       bgl_long_to_bignum(y));
      else
	 return BINT(z);
#else   
   if (!y || !x)
      return BINT(0);
   else {
      //long z = ((x * y) << PTR_ALIGNMENT) >> PTR_ALIGNMENT;
      long z = CINT(BINT((x * y)));

      if (z / y == x && z % y == 0)
	 return BINT(z);
      else
	 return bgl_bignum_mul(bgl_long_to_bignum(x),
			       bgl_long_to_bignum(y));
   }
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_quotient_fx ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_quotient_fx(long x, long y) {
   if (x == BGL_LONG_MIN && y == -1) {
      return bgl_bignum_div(bgl_long_to_bignum(x), bgl_long_to_bignum(y));
   } else {
      return BINT(x / y);
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_plus_elong ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_plus_elong(long x, long y) {
#if BGL_HAVE_OVERFLOW
   long z;
   if (__builtin_saddl_overflow(x, y, &z))
#else   
   long z = x + y;

   if ((x & C_ELONG_SIGN_BIT) == (y & C_ELONG_SIGN_BIT) &&
       (z & C_ELONG_SIGN_BIT) != (x & C_ELONG_SIGN_BIT))
#endif      
      return bgl_bignum_add(bgl_long_to_bignum(x), bgl_long_to_bignum(y));
   else
      return make_belong(z);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_minus_elong ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_minus_elong(long x, long y) {
#if BGL_HAVE_OVERFLOW
   long z;
   if (__builtin_ssubl_overflow(x, y, &z))
#else   
   long z = x - y;

   if ((x & C_ELONG_SIGN_BIT) != (y & C_ELONG_SIGN_BIT) &&
       (z & C_ELONG_SIGN_BIT) != (x & C_ELONG_SIGN_BIT))
#endif      
      return bgl_bignum_sub(bgl_long_to_bignum(x), bgl_long_to_bignum(y));
   else
      return make_belong(z);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_mul_elong ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_mul_elong(long x, long y) {
#if BGL_HAVE_OVERFLOW
   long z;
   if (__builtin_smull_overflow(x, y, &z))
      return bgl_bignum_mul(bgl_long_to_bignum(x), bgl_long_to_bignum(y));
   else
      return make_belong(z);
#else   
   if (!y)
      return bgl_belongzero;
   else {
      long z = x * y;

      if (z / y == x)
	 return make_belong(z);
      else
	 return bgl_bignum_mul(bgl_long_to_bignum(x), bgl_long_to_bignum(y));
   }
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_quotient_elong ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_quotient_elong(long x, long y) {
   if (x == LONG_MIN && y == -1)
      return bgl_bignum_div(bgl_long_to_bignum(x), bgl_long_to_bignum(y));
   else
      return make_belong(x/y);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_plus_llong ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_plus_llong(BGL_LONGLONG_T x, BGL_LONGLONG_T y) {
   BGL_LONGLONG_T z = x + y;

   if ((x & C_LLONG_SIGN_BIT) == (y & C_LLONG_SIGN_BIT) &&
       (z & C_LLONG_SIGN_BIT) != (x & C_LLONG_SIGN_BIT))
      return bgl_bignum_add(bgl_llong_to_bignum(x),
			    bgl_llong_to_bignum(y));
   else
      return make_bllong(z);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_minus_llong ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_minus_llong(BGL_LONGLONG_T x, BGL_LONGLONG_T y) {
   BGL_LONGLONG_T z = x - y;

   if ((x & C_LLONG_SIGN_BIT) != (y & C_LLONG_SIGN_BIT) &&
       (z & C_LLONG_SIGN_BIT) != (x & C_LLONG_SIGN_BIT))
      return bgl_bignum_sub(bgl_llong_to_bignum(x),
			    bgl_llong_to_bignum(y));
   else
      return make_bllong(z);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_mul_llong ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_mul_llong(BGL_LONGLONG_T x, BGL_LONGLONG_T y) {
   if (! y)
      return bgl_bllongzero;
   else {
      BGL_LONGLONG_T z = x * y;

      if (z / y == x)
	 return make_bllong(z);
      else
	 return bgl_bignum_mul(bgl_llong_to_bignum(x),
				bgl_llong_to_bignum(y));
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_safe_quotient_llong ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_safe_quotient_llong(BGL_LONGLONG_T x, BGL_LONGLONG_T y) {
   if (x == BGL_LONGLONG_MIN && y == -1)
      return bgl_bignum_div(bgl_llong_to_bignum(x),
			    bgl_llong_to_bignum(y));
   else
      return make_bllong(x / y);
}

/*---------------------------------------------------------------------*/
/*    gmp_randstate_t                                                  */
/*    gmp_random_state ...                                             */
/*---------------------------------------------------------------------*/
gmp_randstate_t gmp_random_state;

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_rand_bignum ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_rand_bignum(obj_t n) {
   mpz_t x;
   obj_t y;
   
   mpz_init(x);
   mpz_urandomm(x, gmp_random_state, &(BIGNUM(n).mpz));
   y = mpz_to_bignum(x);
   mpz_clear(x);
   
   return y;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF void                                             */
/*    bgl_seed_rand ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_seed_rand(unsigned long int seed) {
   srand(seed);
   gmp_randseed_ui(gmp_random_state, seed);
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_lsh ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_lsh(obj_t z, long n) {
   mpz_t rop;
   obj_t o;
   
   mpz_init(rop);
   
   mpz_mul_2exp(rop, &(BIGNUM(z).mpz), n);
   o = mpz_to_bignum(rop);
   
   mpz_clear(rop);
   
   return o;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_rsh ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_rsh(obj_t z, long n) {
   mpz_t rop;
   obj_t o;
   
   mpz_init(rop);
   
   mpz_div_2exp(rop, &(BIGNUM(z).mpz), n);
   o = mpz_to_bignum(rop);
   
   mpz_clear(rop);
   
   return o;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_mask ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_mask(obj_t x, long n) {
   obj_t obj;
   mpz_t rop;
   
   mpz_init(rop);
   
   mpz_mul_2exp(rop, onez, n);
   mpz_sub(rop, rop, onez);
   
   mpz_and(rop, &(BIGNUM(x).mpz), rop); 
   obj = mpz_to_bignum(rop);
   
   mpz_clear(rop);

   return obj;
}
      
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_and ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_and(obj_t x, obj_t y) {
   obj_t obj;
   mpz_t rop;
   
   mpz_init(rop);
   
   mpz_and(rop, &(BIGNUM(x).mpz), &(BIGNUM(y).mpz)); 
   obj = mpz_to_bignum(rop);
   
   mpz_clear(rop);
   
   return obj;
}
      
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_or ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_or(obj_t x, obj_t y) {
   obj_t obj;
   mpz_t rop;
   
   mpz_init(rop);
   
   mpz_ior(rop, &(BIGNUM(x).mpz), &(BIGNUM(y).mpz));
   obj = mpz_to_bignum(rop);
   
   mpz_clear(rop);
   
   return obj;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_xor ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_xor(obj_t x, obj_t y) {
   obj_t obj;
   mpz_t rop;
   
   mpz_init(rop);
   
   mpz_ior(rop, &(BIGNUM(x).mpz), &(BIGNUM(y).mpz));
   obj = mpz_to_bignum(rop);
   
   mpz_clear(rop);
   
   return obj;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bignum_not ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bignum_not(obj_t x) {
   obj_t obj;
   mpz_t rop;
   
   mpz_init(rop);
   
   mpz_com(rop, &(BIGNUM(x).mpz));
   obj = mpz_to_bignum(rop);
   
   mpz_clear(rop);
   
   return obj;
}
#else

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_bignum ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_bignum(obj_t v) {
   obj_t o = GC_MALLOC(BIGNUM_SIZE);
   
   o->bignum.header = BGL_MAKE_HEADER(BIGNUM_TYPE, 0);
   o->bignum.u16vect = v;

   return BREF(o);
}

#endif
