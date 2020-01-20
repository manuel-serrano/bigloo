/*=====================================================================*/
/*    /tmp/BGL/bigloo4.3g/runtime/Include/bigloo_int.h                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar  2 05:40:03 2017                          */
/*    Last change :  Thu Jan 16 08:59:29 2020 (serrano)                */
/*    Copyright   :  2017-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo INTEGERs                                                  */
/*=====================================================================*/
#ifndef BIGLOO_INT_H 
#define BIGLOO_INT_H

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
/*    Integers                                                         */
/*---------------------------------------------------------------------*/
#define INTEGERP( o ) ((((long)o) & TAG_MASK) == TAG_INT)

#if( !BGL_NAN_TAGGING && !BGL_SMI )
/* normal tagging */
#  define BGL_LONG_MIN (LONG_MIN >> TAG_SHIFT)
#  define BGL_LONG_MAX (LONG_MAX >> TAG_SHIFT)

#  define BINT( i ) (obj_t)TAG( i, TAG_SHIFT, TAG_INT )
#  define CINT( o ) (long)UNTAG( o, TAG_SHIFT, TAG_INT )
#  define ADDFX( x, y ) (obj_t)((long)(x) + ((long)(y)) - TAG_INT)
#  define SUBFX( x, y ) (obj_t)((long)(x) - ((long)(y)) + TAG_INT)

#  define LTFX( x, y ) ((long)(x) < (long)(y))
#  define LEFX( x, y ) ((long)(x) <= (long)(y))
#  define GTFX( x, y ) ((long)(x) > (long)(y))
#  define GEFX( x, y ) ((long)(x) >= (long)(y))
#  define EGFX( x, y ) ((long)(x) == (long)(y))
#elif( !BGL_NAN_TAGGING && BGL_SMI )
/* smi (int32) tatting */
#  define BGL_LONG_MIN (INT32_MIN)
#  define BGL_LONG_MAX (INT32_MAX)

#  define BINT( i ) ((obj_t)((((long)((int32_t)i)) << 32) + TAG_INT))
#  define CINT( i ) ((long)((int32_t)((long)((long)i) >> 32)))
#  define ADDFX( x, y ) (obj_t)((long)(x) + ((long)(y)) - TAG_INT)
#  define SUBFX( x, y ) (obj_t)((long)(x) - ((long)(y)) + TAG_INT)

#  define LTFX( x, y ) ((long)(x) < (long)(y))
#  define LEFX( x, y ) ((long)(x) <= (long)(y))
#  define GTFX( x, y ) ((long)(x) > (long)(y))
#  define GEFX( x, y ) ((long)(x) >= (long)(y))
#  define EGFX( x, y ) ((long)(x) == (long)(y))
#else
/* nan tagging */      
#  define BGL_LONG_MIN (INT32_MIN)
#  define BGL_LONG_MAX (INT32_MAX)

#  define BINT( i ) ((obj_t)(((long)((uint32_t)((int32_t)i)) | TAG_INT)))
#  define CINT( i ) ((long)((int32_t)((long)i)))
#  define ADDFX( x, y ) BINT( CINT( x ) + CINT( y ) )
#  define SUBFX( x, y ) BINT( CINT( x ) - CINT( y ) )

#  define LTFX( x, y ) (CINT( x ) < CINT( y ))
#  define LEFX( x, y ) (CINT( x ) <= CINT( y ))
#  define GTFX( x, y ) (CINT( x ) > CINT( y ))
#  define GEFX( x, y ) (CINT( x ) >= CINT( y ))
#  define EGFX( x, y ) (((long)(x)) == ((long)(y)))
#endif

#define ODDP_FX( i )  ((i) & 1)
#define EVENP_FX( i ) (!ODDP_FX( i ))

/*---------------------------------------------------------------------*/
/*    Long long                                                        */
/*---------------------------------------------------------------------*/
#define DEFINE_LLONG( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; BGL_LONGLONG_T llong; } \
      const aux = { __CNST_FILLER MAKE_HEADER( LLONG_TYPE, 0 ), (BGL_LONGLONG_T)(num) }; \
   const obj_t name = BREF( &(aux.header) )

#define LLONG_SIZE (sizeof( struct llong ))

#define LLONGP( o ) (POINTERP( o ) && (TYPE( o ) == LLONG_TYPE))

#define LLONG( o ) CREF( o )->llong
   
#define LONG_TO_LLONG( l ) ((BGL_LONGLONG_T)l)   
#define LLONG_TO_LONG( o ) ((long) o)
	    
#define BLLONG_TO_LLONG( l ) (LLONG( l ).val)
#define BLLONG_TO_LONG( l ) ((long)BLLONG_TO_LLONG( l ))
#define LONG_TO_BLLONG( l ) (make_bllong((BGL_LONGLONG_T)l))
#define LLONG_TO_BLLONG( l ) (make_bllong((BGL_LONGLONG_T)l))
   
/*---------------------------------------------------------------------*/
/*    Exact long                                                       */
/*---------------------------------------------------------------------*/
#define DEFINE_ELONG( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; long elong; } \
      const aux = { __CNST_FILLER MAKE_HEADER( ELONG_TYPE, 0 ), num }; \
   const obj_t name = BREF( &(aux.header) )
		 
#define ELONG_SIZE (sizeof( struct elong ))

#define ELONGP( o ) (POINTERP( o ) && (TYPE( o ) == ELONG_TYPE))

#define ELONG( o ) CREF( o )->elong

#define ELONG_TO_BELONG( _1 ) make_belong( _1 )
#define BELONG_TO_LONG( l ) (ELONG( l ).val)

/*---------------------------------------------------------------------*/
/*    Stdint                                                           */
/*---------------------------------------------------------------------*/
#define BGL_INT8P( o ) \
   BGL_CNSTP( o, BINT8H, BGL_CNST_SHIFT_INT16 )
#define BGL_UINT8P( o ) \
   BGL_CNSTP( o, BUINT8H, BGL_CNST_SHIFT_INT16 )

#define BGL_INT16P( o ) \
   BGL_CNSTP( o, BINT16H, BGL_CNST_SHIFT_INT16 )
#define BGL_UINT16P( o ) \
   BGL_CNSTP( o, BUINT16H, BGL_CNST_SHIFT_INT16 )

#if( defined( BGL_CNST_SHIFT_INT32 ) )   
#  define BGL_INT32P( o ) \
     BGL_CNSTP( o, BINT32H, BGL_CNST_SHIFT_INT32 )
#  define BGL_UINT32P( o ) \
     BGL_CNSTP( o, BUINT32H, BGL_CNST_SHIFT_INT32 )
#else
#  define BGL_INT32P( o ) (POINTERP( o ) && (TYPE( o ) == INT32_TYPE))
#  define BGL_UINT32P( o ) (POINTERP( o ) && (TYPE( o ) == UINT32_TYPE))
#endif
   
#define BGL_INT64P( o ) (POINTERP( o ) && (TYPE( o ) == INT64_TYPE))
#define BGL_UINT64P( o ) (POINTERP( o ) && (TYPE( o ) == UINT64_TYPE))

#define BGL_INT8_TO_BINT8( i ) \
   BGL_CNST_TO_BCNST( i, 0xffL, BINT8H, BGL_CNST_SHIFT_INT16, int8_t )
#define BGL_UINT8_TO_BUINT8( i ) \
   BGL_CNST_TO_BCNST( i, 0xffL, BUINT8H, BGL_CNST_SHIFT_INT16, uint8_t )

#define BGL_BINT8_TO_INT8( o ) \
   BGL_BCNST_TO_CNST( o, 0xffL, BGL_CNST_SHIFT_INT16, int8_t )
#define BGL_BUINT8_TO_UINT8( o ) \
   BGL_BCNST_TO_CNST( o, 0xffL, BGL_CNST_SHIFT_INT16, uint8_t )

#define BGL_INT16_TO_BINT16( i ) \
   BGL_CNST_TO_BCNST( i, 0xffffL, BINT16H, BGL_CNST_SHIFT_INT16, int16_t )
#define BGL_UINT16_TO_BUINT16( i ) \
   BGL_CNST_TO_BCNST( i, 0xffffL, BUINT16H, BGL_CNST_SHIFT_INT16, uint16_t )
	    
#define BGL_BINT16_TO_INT16( o ) \
   BGL_BCNST_TO_CNST( o, 0xffffL, BGL_CNST_SHIFT_INT16, int16_t )
#define BGL_BUINT16_TO_UINT16( o ) \
   BGL_BCNST_TO_CNST( o, 0xffffL, BGL_CNST_SHIFT_INT16, uint16_t )

#if( defined( BGL_CNST_SHIFT_INT32 ) ) /* BGL_CNST_SHIFT_INT32 */
#  define DEFINE_INT32( name, aux, num ) \
   const obj_t name = BGL_INT32_TO_BINT32( num )
#  define DEFINE_UINT32( name, aux, num ) \
   const obj_t name = BGL_UINT32_TO_BUINT32( num )
   
#  define BGL_INT32_TO_BINT32( i ) \
   BGL_CNST_TO_BCNST( i, 0xffffffffL, BINT32H, BGL_CNST_SHIFT_INT32, int32_t )
#  define BGL_UINT32_TO_BUINT32( i ) \
   BGL_CNST_TO_BCNST( i, 0xffffffffL, BUINT32H, BGL_CNST_SHIFT_INT32, uint32_t )
#  define BGL_BINT32_TO_INT32( o ) \
   BGL_BCNST_TO_CNST( o, 0xffffffffL, BGL_CNST_SHIFT_INT32, int32_t )
#  define BGL_BUINT32_TO_UINT32( o ) \
   BGL_BCNST_TO_CNST( o, 0xffffffffL, BGL_CNST_SHIFT_INT32, uint32_t )
#else /* !BGL_CNST_SHIFT_INT32 */
#  define DEFINE_INT32( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; int32_t val; } \
      const aux = { __CNST_FILLER MAKE_HEADER( INT32_TYPE, 0 ), (int32_t)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#  define DEFINE_UINT32( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; int32_t val; } \
      const aux = { __CNST_FILLER MAKE_HEADER( UINT32_TYPE, 0 ), (uint32_t)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#  define BGL_INT32_SIZE (sizeof( struct bgl_sint32 ))
#  define BGL_UINT32_SIZE (sizeof( struct bgl_uint32 ))
#  define BGL_INT32( o ) CREF( o )->sint32
#  define BGL_UINT32( o ) CREF( o )->uint32
#  define BGL_INT32_TO_BINT32( _1 ) bgl_make_bint32( _1 )
#  define BGL_UINT32_TO_BUINT32( _1 ) bgl_make_buint32( _1 )
#  define BGL_BINT32_TO_INT32( o ) BGL_INT32( o ).val
#  define BGL_BUINT32_TO_UINT32( o ) BGL_UINT32( o ).val
#endif /* BGL_CNST_SHIFT_INT32 */

#define DEFINE_INT64( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; int64_t val; } \
      const aux = { __CNST_FILLER MAKE_HEADER( INT64_TYPE, 0 ), (int64_t)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#define DEFINE_UINT64( name, aux, num ) \
   static struct { __CNST_ALIGN header_t header; uint64_t val; } \
      const aux = { __CNST_FILLER MAKE_HEADER( UINT64_TYPE, 0 ), (uint64_t)(num) }; \
      const obj_t name = BREF( &(aux.header) )
		 
#define BGL_INT64_SIZE (sizeof( struct bgl_sint64 ))
#define BGL_UINT64_SIZE (sizeof( struct bgl_uint64 ))
#define BGL_INT64( o ) CREF( o )->sint64
#define BGL_UINT64( o ) CREF( o )->uint64
#define BGL_INT64_TO_BINT64( _1 ) bgl_make_bint64( _1 )
#define BGL_UINT64_TO_BUINT64( _1 ) bgl_make_buint64( _1 )
#define BGL_BINT64_TO_INT64( o ) BGL_INT64( o ).val
#define BGL_BUINT64_TO_UINT64( o ) BGL_UINT64( o ).val

/*---------------------------------------------------------------------*/
/*    overflow                                                         */
/*    -------------------------------------------------------------    */
/*    These macros are only used when:                                 */
/*      1- Bigloo has been configured with GCC                         */
/*      2- Used to compile with another C compiler,                    */
/*      3- and the C overflow functions are explicitly used by the app */
/*    -------------------------------------------------------------    */
/*    See Hacker's Delight, second edition, page 29.                   */
/*---------------------------------------------------------------------*/
#if( !defined( __GNUC__ ) )
static int __builtin_saddl_overflow( long x, long y, long *res ) {
#if( BGL_ELONG_BIT_SIZE == 32 )
   long z = (~((long)x ^ (long)y)) & 0x80000000;
#else   
   long z = (~((long)x ^ (long)y)) & 0x8000000000000000;;
#endif
   if( z & (~((((long)x ^ (long)z) + ((long)y)) ^ ((long) y))) ) {
      return 1;
   } else {
      *res = (x + y);
      return 0;
   }
}

static int __builtin_ssubl_overflow( long x, long y, long *res ) {
#if( BGL_ELONG_BIT_SIZE == 32 )
   long z = ((long)x ^ (long)y) & 0x80000000;
#else
   long z = ((long)x ^ (long)y) & 0x8000000000000000;
#endif   
   if( z & ((((long)x ^ (long)z) - ((long)y)) ^ ((long) y)) ) {
      return 1;
   } else {
      *res = (x - y);
      return 0;
   }
}
#endif
   
/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

