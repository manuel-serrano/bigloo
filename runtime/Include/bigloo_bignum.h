/*=====================================================================*/
/*    .../project/bigloo/bigloo/runtime/Include/bigloo_bignum.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jul 26 08:58:25 2017                          */
/*    Last change :  Tue Apr 17 08:02:07 2018 (serrano)                */
/*    Copyright   :  2017-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo BIGNUMs                                                   */
/*=====================================================================*/
#ifndef BIGLOO_BIGNUM_H 
#define BIGLOO_BIGNUM_H

/*---------------------------------------------------------------------*/
/*    Does someone really wants C++ here?                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus_just_for_emacs_indent
}
#endif

/*---------------------------------------------------------------------*/
/*    extern                                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL obj_t bgl_string_to_bignum( char *, int );

/*---------------------------------------------------------------------*/
/*    bgl_bignum                                                       */
/*---------------------------------------------------------------------*/
struct bgl_bignum {
      header_t header;
#if( BGL_HAVE_GMP )
      /* from gmp.h */
      __mpz_struct mpz;
#else
      union scmobj *u16vect;
#endif
};

#define BIGNUM( o ) (CREF( o )->bignum)

#define BIGNUM_SIZE (sizeof( struct bgl_bignum ))

/*---------------------------------------------------------------------*/
/*    tagging                                                          */
/*---------------------------------------------------------------------*/
#define BIGNUMP( o ) (POINTERP( o ) && (TYPE( o ) == BIGNUM_TYPE))

/*---------------------------------------------------------------------*/
/*    Bignum                                                           */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_GMP )   
#  define BXSIZ( o ) (BIGNUM( o ).mpz._mp_size)
   
#  define BXZERO( x ) (BXSIZ( x ) == 0)
#  define BXPOSITIVE( x ) (BXSIZ( x ) > 0)
#  define BXNEGATIVE( x ) (BXSIZ( x ) < 0)

#  define BGL_SAFE_BX_TO_FX( x ) bgl_safe_bignum_to_fixnum( x )
#  define BGL_SAFE_PLUS_FX( x, y ) bgl_safe_plus_fx( x, y )
#  define BGL_SAFE_MINUS_FX( x, y ) bgl_safe_minus_fx( x, y )
#  define BGL_SAFE_MUL_FX( x, y ) bgl_safe_mul_fx( x, y )
#  define BGL_SAFE_QUOTIENT_FX( x, y ) bgl_safe_quotient_fx( x, y )
#  define BGL_SAFE_PLUS_ELONG( x, y ) bgl_safe_plus_elong( x, y )
#  define BGL_SAFE_MINUS_ELONG( x, y ) bgl_safe_minus_elong( x, y )
#  define BGL_SAFE_MUL_ELONG( x, y ) bgl_safe_mul_elong( x, y )
#  define BGL_SAFE_QUOTIENT_ELONG( x, y ) bgl_safe_quotient_elong( x, y )
#  define BGL_SAFE_PLUS_LLONG( x, y ) bgl_safe_plus_llong( x, y )
#  define BGL_SAFE_MINUS_LLONG( x, y ) bgl_safe_minus_llong( x, y )
#  define BGL_SAFE_MUL_LLONG( x, y ) bgl_safe_mul_llong( x, y )
#  define BGL_SAFE_QUOTIENT_LLONG( x, y ) bgl_safe_quotient_llong( x, y )
   
extern gmp_randstate_t gmp_random_state;
   
extern obj_t bgl_safe_bignum_to_fixnum( obj_t );
extern obj_t bgl_safe_plus_fx( long, long );
extern obj_t bgl_safe_minus_fx( long, long );
extern obj_t bgl_safe_mul_fx( long, long );
extern obj_t bgl_safe_quotient_fx( long, long );
   
extern obj_t bgl_safe_plus_elong( long, long );
extern obj_t bgl_safe_minus_elong( long, long );
extern obj_t bgl_safe_mul_elong( long, long );
extern obj_t bgl_safe_quotient_elong( long, long );
   
extern obj_t bgl_safe_plus_llong( BGL_LONGLONG_T, BGL_LONGLONG_T );
extern obj_t bgl_safe_minus_llong( BGL_LONGLONG_T, BGL_LONGLONG_T );
extern obj_t bgl_safe_mul_llong( BGL_LONGLONG_T, BGL_LONGLONG_T );
extern obj_t bgl_safe_quotient_llong( BGL_LONGLONG_T, BGL_LONGLONG_T );
#else
#  define BGL_BIGNUM_U16VECT( bx ) (BIGNUM( bx ).u16vect)

#  define BGL_SAFE_BX_TO_FX( x ) (x)
#  define BGL_SAFE_PLUS_FX( x, y ) BINT( (x) + (y) )
#  define BGL_SAFE_MINUS_FX( x, y ) BINT( (x) - (y) )
#  define BGL_SAFE_MUL_FX( x, y ) BINT( (x) * (y) )
#  define BGL_SAFE_QUOTIENT_FX( x, y ) BINT( (x) / (y) )
#  define BGL_SAFE_PLUS_ELONG( x, y ) ELONG_TO_BELONG( (x) + (y) )
#  define BGL_SAFE_MINUS_ELONG( x, y ) ELONG_TO_BELONG( (x) - (y) )
#  define BGL_SAFE_MUL_ELONG( x, y ) ELONG_TO_BELONG( (x) * (y) )
#  define BGL_SAFE_QUOTIENT_ELONG( x, y ) ELONG_TO_BELONG( (x) / (y) )
#  define BGL_SAFE_PLUS_LLONG( x, y ) LLONG_TO_BLLONG( (x) + (y) )
#  define BGL_SAFE_MINUS_LLONG( x, y ) LLONG_TO_BLLONG( (x) - (y) )
#  define BGL_SAFE_MUL_LLONG( x, y ) LLONG_TO_BLLONG( (x) * (y) )
#  define BGL_SAFE_QUOTIENT_LLONG( x, y ) LLONG_TO_BLLONG( (x) / (y) )
   
extern bool_t BXZERO( obj_t );
extern bool_t BXPOSITIVE( obj_t );
extern bool_t BXNEGATIVE( obj_t );
#endif
   
/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

