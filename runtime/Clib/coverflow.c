/*=====================================================================*/
/*    serrano/prgm/project/bigloo/4.5a/runtime/Clib/coverflow.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Sat Aug 27 17:00:51 2022                          */
/*    Last change :  Thu Dec  1 16:39:43 2022 (serrano)                */
/*    Copyright   :  2022 manuel serrano                               */
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
