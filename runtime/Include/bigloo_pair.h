/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/runtime/Include/bigloo_pair.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 08:05:01 2016                          */
/*    Last change :  Sat Dec  7 18:56:17 2019 (serrano)                */
/*    Copyright   :  2016-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo PAIRs                                                     */
/*=====================================================================*/
#ifndef BIGLOO_PAIR_H 
#define BIGLOO_PAIR_H

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
BGL_RUNTIME_DECL obj_t make_pair( obj_t , obj_t );
BGL_RUNTIME_DECL obj_t make_epair( obj_t , obj_t, obj_t );

BGL_RUNTIME_DECL obj_t bgl_reverse( obj_t );
BGL_RUNTIME_DECL obj_t bgl_reverse_bang( obj_t );
   
BGL_RUNTIME_DECL long bgl_list_length( obj_t );
BGL_RUNTIME_DECL obj_t bgl_remq( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_remq_bang( obj_t, obj_t );

#if( BGL_SAW == 1 ) 
BGL_RUNTIME_DECL obj_t bgl_saw_make_pair( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_saw_make_old_pair( obj_t, obj_t );
BGL_RUNTIME_DECL obj_t bgl_saw_make_epair( obj_t, obj_t , obj_t );
BGL_RUNTIME_DECL obj_t bgl_saw_make_old_epair( obj_t, obj_t, obj_t );
#endif

/*---------------------------------------------------------------------*/
/*    bgl_pair ...                                                     */
/*---------------------------------------------------------------------*/
struct bgl_pair {
#if( !TAG_PAIR )
   /* the header, unless pairs are tagged */
   header_t header;
#endif      
   obj_t car;
   obj_t cdr;
};

struct bgl_epair {
#if( !TAG_PAIR )
   header_t header; 
#endif 
   obj_t car;       
   obj_t cdr;
   /* extended header type */
#if( (BGL_GC == BGL_BOEHM_GC) && TAG_PAIR )
   obj_t eheader;
#endif   
   /* extended slot */
   obj_t cer;
};                    

#define PAIR_SIZE (sizeof( struct bgl_pair ))
#define EPAIR_SIZE (sizeof( struct bgl_epair ))

#define PAIR( o ) (CPAIR( o )->pair)
#define EPAIR( o ) (CPAIR( o )->epair)

/*---------------------------------------------------------------------*/
/*    tagging ...                                                      */
/*---------------------------------------------------------------------*/
#if( defined( TAG_PAIR ) )
#   define BPAIR( p ) BGL_BPTR( (long)p + TAG_PAIR )
#   define CPAIR( p ) ((union scmobj *)(((long)BGL_CPTR( p )) - TAG_PAIR))
#   if( TAG_PAIR != 0 )
#      define PAIRP( c ) ((((long)c) & TAG_MASK) == TAG_PAIR)
#   else
#      define PAIRP( c ) ((c && ((((long)c) & TAG_MASK) == TAG_PAIR)))
#   endif
#else
#   define BPAIR( p ) BREF( p )
#   define CPAIR( p ) CREF( p )
#   define PAIRP( c ) (POINTERP( c ) && (TYPE( c ) == PAIR_TYPE))
#endif

#if( BGL_GC == BGL_BOEHM_GC && TAG_PAIR )
#   define EPAIRP( c ) \
      (PAIRP( c ) && \
       (((long)GC_size( CPAIR( c ) )) >= EPAIR_SIZE) && \
       (EPAIR( c ).eheader == BINT( EPAIR_TYPE )))
#else
#   define EPAIRP( c ) \
       (PAIRP( c ) && (HEADER_SIZE( CREF( c  )->header) == EPAIR_SIZE))
#endif

/*---------------------------------------------------------------------*/
/*    alloc ...                                                        */
/*---------------------------------------------------------------------*/
#if( !TAG_PAIR )
#  define IFN_PAIR_TAG( expr ) expr
#else
#  define IFN_PAIR_TAG( expr ) 0
#endif   

#if( TAG_PAIR && ( BGL_GC == BGL_BOEHM_GC) )
#  define IF_EPAIR_TAG( expr ) expr
#else
#  define IF_EPAIR_TAG( expr ) 
#endif   

#define BGL_INIT_PAIR( an_object, a, d ) \
   (IFN_PAIR_TAG( BGL_CPTR( an_object )->pair.header =	\
	 	  MAKE_HEADER( PAIR_TYPE, PAIR_SIZE ) ), \
    BGL_CPTR( an_object )->pair.car = a, \
    BGL_CPTR( an_object )->pair.cdr = d)

#define BGL_INIT_EPAIR( an_object, a, d, e ) \
   (IFN_PAIR_TAG( BGL_CPTR( an_object )->pair.header = \
		  MAKE_HEADER( PAIR_TYPE, EPAIR_SIZE ) ), \
    BGL_CPTR( an_object )->pair.car = a, \
    BGL_CPTR( an_object )->pair.cdr = d, \
    BGL_CPTR( an_object )->epair.cer = e, \
    IF_EPAIR_TAG( BGL_CPTR( an_object )->epair.eheader = BINT( EPAIR_TYPE ) ))

/* boehm allocation */
#if( BGL_GC == BGL_BOEHM_GC )
#  if( BGL_GC_CUSTOM || !defined( __GNUC__ ) )
#     define MAKE_PAIR( a, d ) make_pair( a, d )
#     define MAKE_EPAIR( a, d, e ) make_epair( a, d, e )
#  else
#     define MAKE_PAIR( a, d ) \
         ({ obj_t an_object = GC_MALLOC( PAIR_SIZE ); \
	    BGL_INIT_PAIR( an_object, a, d ); \
	    BPAIR( an_object ); })
#     define MAKE_EPAIR( a, d, e ) \
         ({ obj_t an_object = GC_MALLOC( EPAIR_SIZE ); \
	    BGL_INIT_EPAIR( an_object, a, d, e );       \
	    BPAIR( an_object ); })
#  endif

#  define MAKE_YOUNG_PAIR( a, d ) MAKE_PAIR( a, d )
#  define MAKE_YOUNG_EPAIR( a, d, e ) MAKE_EPAIR( a, d, e )
#endif

/* saw allocation */
#if( BGL_GC == BGL_SAW_GC )
#  define MAKE_YOUNG_PAIR( a, d ) bgl_saw_make_pair( a, d )
#  define MAKE_YOUNG_EPAIR( a, d, e ) bgl_saw_make_epair( a, d, e )
#  define MAKE_PAIR( a, d ) bgl_saw_make_old_pair( a, d )
#  define MAKE_EPAIR( a, d, e ) bgl_saw_make_old_epair( a, d, e )
#endif

/* stack allocation (see BGL_EXITD_PUSH_PROTECT) */
#if( BGL_HAVE_ALLOCA )
#  define MAKE_STACK_PAIR_TMP( a, d, __t ) \
     (__t = alloca( PAIR_SIZE ), BGL_INIT_PAIR( __t, a, d ), BPAIR( __t ))
#  if( BGL_HAVE_ALLOCA && defined( __GNUC__ ) )
#    define MAKE_STACK_PAIR( a, d ) \
       ({ obj_t an_object; MAKE_STACK_PAIR_TMP( a, d, an_object); })
#  else
#    define MAKE_STACK_PAIR( a, d ) MAKE_PAIR( a, d )   
#  endif
#else
#  define MAKE_STACK_PAIR_TMP( a, d, __t ) __t = MAKE_PAIR( a, d )
#  define MAKE_STACK_PAIR( a, d ) MAKE_PAIR( a, d )   
#endif

/*---------------------------------------------------------------------*/
/*    api                                                              */
/*---------------------------------------------------------------------*/
#define NULLP( c ) ((long)(c) == (long)BNIL)

#define CAR( c ) (PAIR( c ).car)
#define CDR( c ) (PAIR( c ).cdr)
#define CER( c ) (EPAIR( c ).cer)

#define SET_CAR( c, v ) BASSIGN( CAR( c ), v, c )
#define SET_CDR( c, v ) BASSIGN( CDR( c ), v, c )
#define SET_CER( c, v ) BASSIGN( CER( c ), v, c )

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

