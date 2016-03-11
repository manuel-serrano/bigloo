/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Include/bigloo_pair.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 08:05:01 2016                          */
/*    Last change :  Fri Mar 11 16:22:30 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
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
BGL_RUNTIME_DECL obj_t make_pair( obj_t , obj_t  );
BGL_RUNTIME_DECL obj_t make_extended_pair( obj_t , obj_t , obj_t  );

#if( BGL_SAW == 1 ) 
BGL_RUNTIME_DECL obj_t csaw_make_pair( obj_t , obj_t  );
BGL_RUNTIME_DECL obj_t bgl_saw_make_pair( obj_t , obj_t  );
BGL_RUNTIME_DECL obj_t bgl_saw_make_extended_pair( obj_t , obj_t , obj_t  );
#endif

/*---------------------------------------------------------------------*/
/*    bgl_pair ...                                                     */
/*---------------------------------------------------------------------*/
struct bgl_pair {
#if( !(defined( TAG_PAIR )) )
   /* the header, unless pairs are tagged */
   header_t header;
#endif      
   obj_t car;
   obj_t cdr;
};

struct bgl_extended_pair {
#if( !(defined( TAG_PAIR )) )
   header_t header; 
#endif 
   obj_t car;       
   obj_t cdr;
   /* extended header type */
#if( (BGL_GC == BGL_BOEHM_GC) && defined( TAG_PAIR ) )
   obj_t eheader;
#endif   
   /* extended slot */
   obj_t cer;
};                    

#define PAIR( o ) (CPAIR( o )->pair_t)
#define EPAIR( o ) (CPAIR( o )->extended_pair_t)

#define PAIR_SIZE (sizeof( struct bgl_pair ))
#define EXTENDED_PAIR_SIZE (sizeof( struct bgl_extended_pair ))

/*---------------------------------------------------------------------*/
/*    tagging ...                                                      */
/*---------------------------------------------------------------------*/
#if( defined( TAG_PAIR ) )
#   define BPAIR( p ) ((obj_t)((long)p + TAG_PAIR))
#   define CPAIR( p ) ((obj_t)((long)p - TAG_PAIR))
#   if( TAG_PAIR == 0 )
#      define PAIRP( c ) ((c && ((((long)c) & TAG_MASK) == TAG_PAIR)))
#   else
#      define PAIRP( c ) ((((long)c) & TAG_MASK) == TAG_PAIR)
#   endif
#else
#   define BPAIR( p ) BREF( p )
#   define CPAIR( p ) CREF( p )
#   define PAIRP( c ) (POINTERP( c ) && (TYPE( c ) == PAIR_TYPE))
#endif

#if( BGL_GC == BGL_BOEHM_GC && defined( TAG_PAIR ) )
#   define EXTENDED_PAIRP( c ) \
      (PAIRP( c ) && \
       (((long)GC_size( BPAIR( c ) )) >= EXTENDED_PAIR_SIZE) && \
       (EPAIR( c ).eheader == BINT( EXTENDED_PAIR_TYPE )))
#else
#   define EXTENDED_PAIRP( c ) \
       (PAIRP( c ) && (HEADER_SIZE( CREF( c  )->header) == 3))
#endif

/*---------------------------------------------------------------------*/
/*    alloc ...                                                        */
/*---------------------------------------------------------------------*/
#if( !defined( TAG_PAIR ) )
#  define IFN_PAIR_TAG( expr ) expr;
#else
#  define IFN_PAIR_TAG( expr )
#endif   

#if( defined( TAG_PAIR ) && ( BGL_GC == BGL_BOEHM_GC) )
#  define IF_EPAIR_TAG( expr ) expr;
#else
#  define IF_EPAIR_TAG( expr )
#endif   

#define BGL_MAKE_INLINE_PAIR( a, d ) \
   an_object = GC_MALLOC( PAIR_SIZE ); \
   IFN_PAIR_TAG( an_object->pair_t.header = MAKE_HEADER( PAIR_TYPE, 0 ) ) \
   an_object->pair_t.car = a; \
   an_object->pair_t.cdr = d; \
   BPAIR( an_object )

#define BGL_MAKE_INLINE_EPAIR( a, d, e ) \
   an_object = GC_MALLOC( PAIR_SIZE ); \
   IFN_PAIR_TAG( an_object->extended_pair_t.header=MAKE_HEADER(PAIR_TYPE, 3) ) \
   an_object->pair_t.car = a; \
   an_object->pair_t.cdr = d; \
   an_object->extended_pair_t.cer = e; \
   IF_EPAIR_TAG( an_object->extended_pair_t.eheader=BINT(EXTENDED_PAIR_TYPE) ) \
   BPAIR( an_object )

#if( BGL_GC == BGL_BOEHM_GC )
#  if( BGL_GC_CUSTOM || defined( __GNUC__ ) )
#     define MAKE_PAIR( a, d ) make_pair( a, d )
#     define MAKE_EXTENDED_PAIR( a, d, e ) make_extended_pair( a, d, e )
#  else
#     define MAKE_PAIR( a, d ) \
         ({ obj_t an_object; BGL_MAKE_INLINE_PAIR( a, d ); })
#     define MAKE_EXTENDED_PAIR( a, d, e ) \
         ({ obj_t an_object; BGL_MAKE_INLINE_PAIR( a, d ); })
#  endif
#endif


#if( BGL_GC == BGL_SAW_GC )
extern obj_t bps_make_pair(obj_t a, obj_t d);
extern obj_t bps_make_extended_pair(obj_t a, obj_t d, obj_t e);
#  if defined( __GNUC__ )
#     define MAKE_PAIR( a, d ) \
   bps_make_pair(a, d )
/*       ({ obj_t an_object; \                                         */
/* 	  if( !BGL_SAW_CAN_ALLOC( an_object, PAIR_SIZE ) ) bgl_saw_gc(); \ */
/*           BGL_SAW_ALLOC( PAIR_SIZE ); \                             */
/*           IF_PAIR_TAG( an_object->pair_t.header = MAKE_HEADER( PAIR_TYPE, 0 ) ); \ */
/*           an_object->pair_t.car = a; \                              */
/*           an_object->pair_t.cdr = d; \                              */
/*           BYOUNG( an_object ); })                                   */
#     define MAKE_EXTENDED_PAIR( a, d, e ) \
   bps_make_extended_pair( a, d, e )
/*        ({ obj_t an_object; \                                        */
/* 	  if( !BGL_SAW_CAN_ALLOC( an_object, EXTENDED_PAIR_SIZE ) ) bgl_saw_gc(); \ */
/*           BGL_SAW_ALLOC( EXTENDED_PAIR_SIZE ); \                    */
/*           IF_PAIR_TAG( an_object->pair_t.header = MAKE_HEADER( PAIR_TYPE, 3 ) ); \ */
/*           an_object->pair_t.car = a; \                              */
/*           an_object->pair_t.cdr = d; \                              */
/*           an_object->extended_pair_t.cer = e; \                     */
/*           BYOUNG( an_object ); })                                   */
#  else
#     define MAKE_PAIR( a, d ) bgl_saw_make_pair( a, d );
#     define MAKE_EXTENDED_PAIR( a, d, e ) bgl_saw_make_extended_pair( a, d, e );
#  endif
#endif

/*---------------------------------------------------------------------*/
/*    api                                                              */
/*---------------------------------------------------------------------*/
#define NULLP( c ) ((long)(c) == (long)BNIL)

#define CAR( c ) (PAIR( c ).car)
#define CDR( c ) (PAIR( c ).cdr)
#define CER( c ) (EPAIR( c ).cer)

#define SET_CAR( c, v ) (BASSIGN( CAR( c ), v, c ), BUNSPEC)
#define SET_CDR( c, v ) (BASSIGN( CDR( c ), v, c ), BUNSPEC)
#define SET_CER( c, v ) (BASSIGN( CER( c ), v, c ), BUNSPEC)

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

