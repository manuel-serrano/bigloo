/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/coverflow.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Sat Aug 27 17:00:51 2022                          */
/*    Last change :  Fri Nov  8 11:38:15 2024 (serrano)                */
/*    Copyright   :  2022-24 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Handling overflow in arithmetic when the C compiler does not     */
/*    support builtin operations.                                      */
/*=====================================================================*/
#include <limits.h>
#include <errno.h>
#include <bigloo.h>

#if !BGL_HAVE_OVERFLOW
/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_sadd_overflow ...                                            */
/*---------------------------------------------------------------------*/
bool_t bgl_sadd_overflow(int x, int y, int *res) {
   int z = x + y;
   
   if ((x & C_INT_SIGN_BIT) == (y & C_INT_SIGN_BIT) &&
       (z & C_INT_SIGN_BIT) != (x & C_INT_SIGN_BIT)) {
      return 1;
   } else {
      *res = z;
      return 0;
   }
}

bool_t bgl_sub_overflow(int x, int y, int *res) {
   int z = x - y;

   if ((x & C_INT_SIGN_BIT) != (y & C_INT_SIGN_BIT) &&
       (z & C_INT_SIGN_BIT) != (x & C_INT_SIGN_BIT)) {
      return 1;
   } else {
      *res = z;
      return 0;
   }
}

bool_t bgl_mul_overflow(int x, int y, int *res) {
   if (!y || !x) {
      *res = 0;
      return 0;
   } else {
      int z = x * y;

      if (z / y == x && z % y == 0) {
	 *res = BINT(z);
         return 0;
      } else {
         return 1;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_saddl_overflow ...                                           */
/*---------------------------------------------------------------------*/
bool_t bgl_saddl_overflow(long x, long y, long *res) {
   long z = x + y;
   
   if ((x & C_LONG_SIGN_BIT) == (y & C_LONG_SIGN_BIT) &&
       (z & C_LONG_SIGN_BIT) != (x & C_LONG_SIGN_BIT)) {
      return 1;
   } else {
      *res = z;
      return 0;
   }
}

bool_t bgl_subl_overflow(long x, long y, long *res) {
   long z = x - y;

   if ((x & C_LONG_SIGN_BIT) != (y & C_LONG_SIGN_BIT) &&
       (z & C_LONG_SIGN_BIT) != (x & C_LONG_SIGN_BIT)) {
      return 1;
   } else {
      *res = z;
      return 0;
   }
}

bool_t bgl_mull_overflow(long x, long y, long *res) {
   if (!y || !x) {
      *res = 0;
      return 0;
   } else {
      long z = x * y;

      if (z / y == x && z % y == 0) {
	 *res = BINT(z);
         return 0;
      } else {
         return 1;
      }
   }
}
#endif
