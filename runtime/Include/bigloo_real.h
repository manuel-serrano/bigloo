/*=====================================================================*/
/*    /tmp/OFAOT/nan/lib/bigloo/4.3h/bigloo_real.h                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Mar  6 07:07:32 2016                          */
/*    Last change :  Tue Apr 28 10:36:49 2020 (serrano)                */
/*    Copyright   :  2016-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo REALs                                                     */
/*=====================================================================*/
#ifndef BIGLOO_REAL_H 
#define BIGLOO_REAL_H

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
#if( !BGL_NAN_TAGGING ) 
BGL_RUNTIME_DECL obj_t bigloo_nan, bigloo_infinity, bigloo_minfinity;
BGL_RUNTIME_DECL obj_t make_real( double );

#   if( BGL_SAW == 1 ) 
BGL_RUNTIME_DECL obj_t bgl_saw_make_real( double );
#   endif
#else
BGL_RUNTIME_DECL union nanobj bigloo_nan, bigloo_infinity, bigloo_minfinity;
#endif

/*---------------------------------------------------------------------*/
/*    bgl_real ...                                                     */
/*---------------------------------------------------------------------*/
#if( !BGL_NAN_TAGGING ) 
struct bgl_real {
#if( !defined( TAG_REAL ) )
   header_t header;
#endif
   double val;
};

#  define REAL( o ) (CREAL( o )->real)
#  define REAL_SIZE  (sizeof( struct bgl_real ))
#else
#  define REAL( _o ) ( _o.real )
#endif
   
/*---------------------------------------------------------------------*/
/*    tagging                                                          */
/*---------------------------------------------------------------------*/
#if( BGL_NAN_TAGGING )
union nanobj {
   double real;
   obj_t ptr;
};
      
#   define BREAL( _n ) (((union nanobj){ real: _n }).ptr)
#   define CREAL( _p ) (((union nanobj){ ptr: _p }).real)

#   define BGL_REAL_CNST( name ) name.ptr
#   define DEFINE_REAL( name, aux, _flonum ) \
      static const union nanobj name = { real: _flonum }; \

#   define FLONUMP( c ) (((unsigned long)c >> 48 & 0x7ff8) != 0x7ff8)
#   define NANP( c ) (((unsigned long)c == TAG_QNAN) || ((unsigned long)c == TAG_SNAN))
#   define REALP( c ) (FLONUMP( c ) || NANP( c ))

#   define BGL_REAL_SET( o, v ) BREAL(v)
#elif( defined( TAG_REAL ) )
#   define BREAL( p ) ((obj_t)((long)p + TAG_REAL))
#   define CREAL( p ) ((obj_t)((long)p - TAG_REAL))
#   define BGL_REAL_CNST( name ) name
#   define DEFINE_REAL( name, aux, flonum ) \
      static struct { double real; } aux = { flonum }; \
      static const obj_t name = BREAL( &aux )

#   define FLONUMP( c ) ((c && ((((long)c)&TAG_MASK) == TAG_REAL)))
#   define REALP( c ) FLONUMP( c )

#   define BGL_REAL_SET( o, v ) ((REAL( o ).val = v), o)
#else
#   define BREAL( p ) BREF( p )
#   define CREAL( p ) CREF( p )
#   define BGL_REAL_CNST( name ) name
#   define DEFINE_REAL( name, aux, flonum ) \
      static struct { __CNST_ALIGN header_t header; double real; } \
	 aux = { __CNST_FILLER MAKE_HEADER( REAL_TYPE, 0 ), flonum }; \
      static const obj_t name = BREAL( &(aux.header) )

#   define FLONUMP( c ) (POINTERP( c ) && (TYPE( c ) == REAL_TYPE))
#   define REALP( c ) FLONUMP( c )
   
#   define BGL_REAL_SET( o, v ) ((REAL( o ).val = v), o)
#endif

/*---------------------------------------------------------------------*/
/*    alloc                                                            */
/*---------------------------------------------------------------------*/
#if( !defined( TAG_REAL ) )
#  define IFN_REAL_TAG( expr ) expr
#else
#  define IFN_REAL_TAG( expr )
#endif   

#if( !BGL_NAN_TAGGING )
#  if( BGL_GC != BGL_SAW_GC )
#    define DOUBLE_TO_REAL( d ) (make_real( d ))
#  else					
BGL_RUNTIME_DECL obj_t bgl_saw_make_real( double );
#    define DOUBLE_TO_REAL( d ) (bgl_saw_make_real( d ))
#  endif
#  define REAL_TO_DOUBLE( r ) (REAL( r ).val)
#  define FLOAT_TO_REAL( d ) (DOUBLE_TO_REAL( (double)(d) ))
#  define REAL_TO_FLOAT( r ) ((float)(REAL( r ).val))
#else
#  define make_real( d ) BREAL( d )
#  define REAL_TO_DOUBLE( r ) CREAL( r )
#  define DOUBLE_TO_REAL( r ) make_real( r )
#  define FLOAT_TO_REAL( d ) (DOUBLE_TO_REAL( (double)(d) ))
#  define REAL_TO_FLOAT( r ) ((float)(REAL_TO_DOUBLE( r ) ))
#endif

/* boehm allocation */
#if( BGL_NAN_TAGGING )
#  define MAKE_REAL( v ) make_real( v )
#  define BGL_MAKE_INLINE_REAL( v ) MAKE_REAL( v )
#elif( BGL_GC == BGL_BOEHM_GC )
#  define BGL_INIT_REAL( an_object, d ) \
     IFN_REAL_TAG( (an_object)->real.header = \
		   MAKE_HEADER( REAL_TYPE, REAL_SIZE ) ); \
     (an_object)->real.val = d;
#  if( BGL_GC_CUSTOM || !defined( __GNUC__ ) )
#     define MAKE_REAL( v ) make_real( v )
#  else
#     define MAKE_REAL( v ) \
         ({ obj_t an_object = GC_MALLOC( REAL_SIZE ); \
	    BGL_INIT_REAL( an_object, v ); \
	    BREAL( an_object ); })
#  endif
#  define MAKE_YOUNG_REAL( v ) MAKE_REAL( v )
#  define BGL_MAKE_INLINE_REAL( d ) \
     an_object = GC_MALLOC_ATOMIC( REAL_SIZE ); \
     IFN_REAL_TAG( an_object->real_t.header = \
  		 MAKE_HEADER( REAL_TYPE, REAL_SIZE ) )\
     an_object->real.val = d; \
     BREAL( an_object )
#endif

/*---------------------------------------------------------------------*/
/*    api                                                              */
/*---------------------------------------------------------------------*/
#define __DOUBLE_TO_LLONG_BITS( dd ) \
  double ddd = dd; \
  BGL_LONGLONG_T result; \
  memcpy( &result, &ddd, sizeof( result ) ); \
  result;

#define __LLONG_BITS_TO_DOUBLE( ll ) \
  BGL_LONGLONG_T lll = ll; \
  double result; \
  memcpy( &result, &lll, sizeof( result ) ); \
  result;

/* If ints are bigger than floats (which can happen on 64bit machines) */
/* then * we only want to set the least significant bytes of the int.  */
#define __FLOAT_TO_INT_BITS( ff ) \
  float fff = ff; \
  int result = 0; \
  int offset = sizeof( int ) - sizeof( float ); \
  memcpy( ((char*)&result) + offset, &fff, sizeof( float ) ); \
  result;

/* If ints are bigger than floats (which can happen on 64bit machines) */
/* then we only want to set the least significant bytes of the int.    */
#define __INT_BITS_TO_FLOAT( ii ) \
  int iii = ii; \
  float result; \
  int offset = sizeof( int ) - sizeof( float ); \
  memcpy( &result, ((char*)&iii) + offset, sizeof( float ) ); \
  result;

#if( defined( __GNUC__ ) )
#  define DOUBLE_TO_LLONG_BITS( dd ) ( { __DOUBLE_TO_LLONG_BITS( dd ) } )
#  define LLONG_BITS_TO_DOUBLE( ll ) ( { __LLONG_BITS_TO_DOUBLE( ll ) } )
#  define FLOAT_TO_INT_BITS( ff ) ( { __FLOAT_TO_INT_BITS( ff ) } )
#  define INT_BITS_TO_FLOAT( ii ) ( { __INT_BITS_TO_FLOAT( ii ) } )
#else
BGL_RUNTIME_DECL BGL_LONGLONG_T DOUBLE_TO_LLONG_BITS( double );
BGL_RUNTIME_DECL double LLONG_BITS_TO_DOUBLE( BGL_LONGLONG_T );
BGL_RUNTIME_DECL int FLOAT_TO_INT_BITS( float );
BGL_RUNTIME_DECL float INT_BITS_TO_FLOAT( int );
#endif

#define NEG( x ) (- (x))

#define RANDOMFL() ((double)rand()/RAND_MAX)
   
#if BGL_ISOC99 || defined( __USE_ISOC99 )
#  define BGL_SIGNBIT( a ) signbit( a ) 
#  define BGL_NAN ((double) NAN)
#  define BGL_INFINITY ((double) INFINITY)
#  define BGL_IS_FINITE isfinite
#  define BGL_IS_INF isinf
#  define BGL_IS_NAN isnan
#  define BGL_FL_MAX2( a, b ) fmax( a, b )
#  define BGL_FL_MIN2( a, b ) fmin( a, b )
#  define BGL_FL_ROUND( a ) round( a )
#else
#  if BGL_CCDIV0
#    define BGL_NAN (0.0 / 0.0)
#    define BGL_INFINITY (1.0 / 0.0)
#  else
BGL_RUNTIME_DECL double bgl_nan();
BGL_RUNTIME_DECL double bgl_infinity();
#    define BGL_NAN bgl_nan()
#    define BGL_INFINITY bgl_infinity()
#  endif   
#  define BGL_SIGNBIT( r ) ((r<0.0)?1:(((1.0/r)<0.0)?1:0))
#  define BGL_IS_FINITE( r ) (!(BGL_IS_INF( r ) || BGL_IS_NAN( r )))
#  define BGL_IS_INF( r ) (r==BGL_INFINITY || r==-BGL_INFINITY)
#  define BGL_IS_NAN( r ) ((r) != (r))
/* min and max have to look at -0.0 and +0.0, too */
#  define BGL_FL_MAX2( a, b ) (a<b?b:(a==0.0&&b==0.0&&1.0/a<0.0)?b:a)
#  define BGL_FL_MIN2( a, b ) (a>b?b:(a==0.0&&b==0.0&&1.0/a>0.0)?b:a)
#  define BGL_FL_ROUND( a ) (BGL_SIGNBIT(a)?-(floor(-a+0.5)):floor(a+0.5))
#endif

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

