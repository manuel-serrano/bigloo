/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Include/bigloo_vector.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 08:05:01 2016                          */
/*    Last change :  Mon Mar  7 09:33:05 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Bigloo VECTORs                                                   */
/*=====================================================================*/
#ifndef BIGLOO_VECTOR_H 
#define BIGLOO_VECTOR_H

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
BGL_RUNTIME_DECL obj_t create_vector( int );
BGL_RUNTIME_DECL obj_t bgl_saw_vector_copy( obj_t );

/*---------------------------------------------------------------------*/
/*    bgl_vector ...                                                   */
/*---------------------------------------------------------------------*/
struct bgl_vector {
#if( !defined( TAG_VECTOR ) )
   header_t header;
#endif
   /* 24 bit long length (see VECTOR_LENGTH) */
   unsigned long length;
   obj_t obj0;
};             

struct bgl_tvector {
   header_t header;
   unsigned long length;
   obj_t descr;
};

struct bgl_hvector {
   header_t header;
   unsigned long length;
};

#define VECTOR( o ) CVECTOR( o )->vector_t
#define TVECTOR( tv ) CREF( tv )->tvector_t
#define HVECTOR( o ) CREF( o )->hvector_t

#define VECTOR_SIZE (sizeof( struct bgl_vector ))
#define TVECTOR_SIZE (sizeof( struct bgl_tvector ))
#define HVECTOR_SIZE (sizeof( struct bgl_hvector ))

/*---------------------------------------------------------------------*/
/*    tagging                                                          */
/*---------------------------------------------------------------------*/
#if( defined( TAG_VECTOR ) )
#   define BVECTOR( p ) ((obj_t)((long)p + TAG_VECTOR))
#   define CVECTOR( p ) ((obj_t)((long)p - TAG_VECTOR))
#   define VECTORP( c ) ((c && ((((long)c) & TAG_MASK) == TAG_VECTOR)))
#else
#   define BVECTOR( p ) BREF( p )
#   define CVECTOR( p ) CREF( p )
#   define VECTORP( c ) (POINTERP( c ) && (TYPE( c ) == VECTOR_TYPE))
#endif

#define TVECTORP( o ) (POINTERP( o ) && (TYPE( o ) == TVECTOR_TYPE))
#define BGL_HVECTORP( v ) \
    (POINTERP( v ) \
     && (TYPE( v ) >= S8VECTOR_TYPE) \
     && (TYPE( v ) <= F64VECTOR_TYPE))

#define VECTOR_TAG_NB_BIT 8
#define VECTOR_TAG_SIZE ((unsigned long)( 1<< VECTOR_TAG_NB_BIT))

#define VECTOR_LENGTH_SHIFT ((sizeof( long ) << 3) - VECTOR_TAG_NB_BIT)

#define VECTOR_LENGTH_MASK \
   (~(unsigned long)((VECTOR_TAG_SIZE -1) << VECTOR_LENGTH_SHIFT))

/*---------------------------------------------------------------------*/
/*    alloc                                                            */
/*---------------------------------------------------------------------*/
#define FREE_VECTOR_UNCOLLECTABLE( v ) GC_FREE( CVECTOR( v ) )
   
/*---------------------------------------------------------------------*/
/*    vector api                                                       */
/*---------------------------------------------------------------------*/
#define VECTOR_REF( v, i ) ((&(VECTOR( v ).obj0))[ i ])
#define VECTOR_SET( v, i, o ) (BASSIGN( VECTOR_REF( v, i ), o, v))

/*---------------------------------------------------------------------*/
/*    Typed vectors                                                    */
/*---------------------------------------------------------------------*/
#define DEFINE_TVECTOR_START( aux, len, itype ) \
   static struct { __CNST_ALIGN header_t header; \
		   unsigned long length; \
		   obj_t descr; \
		   itype items[ len ]; } \
      aux = { __CNST_FILLER, MAKE_HEADER( TVECTOR_TYPE, 0 ), len, 0L,
	      
#define DEFINE_TVECTOR_STOP( name, aux ) \
	   }; static obj_t name = BREF( &(aux.header) )

#ifdef __GNUC__
# define ALLOCATE_TVECTOR_MALLOC( MALLOC, _item_name, _item_type, _len, _descr )   \
    ({obj_t an_object;                                                 \
      an_object = MALLOC(sizeof(struct bgl_tvector_of_##_item_name)    \
                         +                                             \
                         ((_len-1) * sizeof(_item_type))),             \
     (an_object->tvector_t).header = MAKE_HEADER( TVECTOR_TYPE, 0 ),   \
     (an_object->tvector_t).length = _len,                             \
     (an_object->tvector_t).descr = _descr,                            \
       ( BREF( an_object ) ); })
#else
# define ALLOCATE_TVECTOR_MALLOC( MALLOC, _item_name, _item_type, _len, _descr )   \
    (an_object = MALLOC(sizeof(struct bgl_tvector_of_##_item_name)     \
                        +                                              \
                        ((_len-1) * sizeof(_item_type))),              \
    (an_object->tvector_t).header = MAKE_HEADER( TVECTOR_TYPE, 0 ),    \
    (an_object->tvector_t).length = _len,                              \
    (an_object->tvector_t).descr = _descr,                             \
       ( BREF( an_object ) ) )
#endif

#define ALLOCATE_TVECTOR( _item_name, _item_type, _len, _descr )   \
   ALLOCATE_TVECTOR_MALLOC( GC_MALLOC, _item_name, _item_type, _len, _descr )
#define ALLOCATE_ATOMIC_TVECTOR( _item_name, _item_type, _len, _descr )   \
   ALLOCATE_TVECTOR_MALLOC( GC_MALLOC_ATOMIC, _item_name, _item_type, _len, _descr )

#define TVECTOR_ID( tv ) TVECTOR( tv ).id
#define TVECTOR_ID_SET( tv, _id_ ) (TVECTOR_ID( tv ) = _id_, BUNSPEC)

#define TVECTOR_LENGTH( tv ) TVECTOR( tv ).length
#define TVECTOR_DESCR( tv ) TVECTOR( tv ).descr
#define TVECTOR_DESCR_SET( tv, _d_ ) (TVECTOR_DESCR( tv ) = _d_, BUNSPEC)

#define TVECTOR_REF( it, tv, o ) \
      (&(((struct bgl_tvector_of_##it *) \
       CREF( tv ))->el0))[ o ]
   
#define TVECTOR_SET( it, tv, o, v ) \
     (TVECTOR_REF( it, tv, o ) = (v), BUNSPEC)

#define VECTOR_LENGTH( v ) \
   (VECTOR( v ).length & VECTOR_LENGTH_MASK)

#define VECTOR_TAG_SET( v, tag ) \
    (VECTOR( v ).length = \
     (VECTOR_LENGTH( v ) | (((unsigned long) tag) << VECTOR_LENGTH_SHIFT)), \
     BUNSPEC)

#define VECTOR_TAG( v ) \
   ((VECTOR( v ).length & ~VECTOR_LENGTH_MASK) >> VECTOR_LENGTH_SHIFT)

/*---------------------------------------------------------------------*/
/*    HVECTOR                                                          */
/*---------------------------------------------------------------------*/
   
#define STVECTOR( o, type ) \
   ((struct { header_t header; unsigned long length; type obj0; } *)(CREF( o )))
   
#define BGL_HVECTOR_LENGTH( v ) (HVECTOR( v ).length)

#define BGL_HVECTOR_IDENT( v ) (TYPE( v ) - S8VECTOR_TYPE)
   
#define BGL_S8VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == S8VECTOR_TYPE))
#define BGL_U8VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == U8VECTOR_TYPE))
#define BGL_S16VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == S16VECTOR_TYPE))
#define BGL_U16VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == U16VECTOR_TYPE))
#define BGL_S32VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == S32VECTOR_TYPE))
#define BGL_U32VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == U32VECTOR_TYPE))
#define BGL_S64VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == S64VECTOR_TYPE))
#define BGL_U64VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == U64VECTOR_TYPE))
#define BGL_F32VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == F32VECTOR_TYPE))
#define BGL_F64VECTORP( v ) (POINTERP( v ) && (TYPE( v ) == F64VECTOR_TYPE))

#define BGL_S8VREF( v, i ) (&(STVECTOR( v, int8_t )->obj0))[ i ]
#define BGL_S8VSET( v, i, o ) (BGL_S8VREF( v, i ) = o, BUNSPEC)
#define BGL_U8VREF( v, i ) (&(STVECTOR( v, uint8_t )->obj0))[ i ]
#define BGL_U8VSET( v, i, o ) (BGL_S8VREF( v, i ) = o, BUNSPEC)
   
#define BGL_S16VREF( v, i ) (&(STVECTOR( v, int16_t )->obj0))[ i ]
#define BGL_S16VSET( v, i, o ) (BGL_S16VREF( v, i ) = o, BUNSPEC)
#define BGL_U16VREF( v, i ) (&(STVECTOR( v, uint16_t )->obj0))[ i ]
#define BGL_U16VSET( v, i, o ) (BGL_S16VREF( v, i ) = o, BUNSPEC)

#define BGL_S32VREF( v, i ) (&(STVECTOR( v, int32_t )->obj0))[ i ]
#define BGL_S32VSET( v, i, o ) (BGL_S32VREF( v, i ) = o, BUNSPEC)
#define BGL_U32VREF( v, i ) (&(STVECTOR( v, uint32_t )->obj0))[ i ]
#define BGL_U32VSET( v, i, o ) (BGL_U32VREF( v, i ) = o, BUNSPEC)

#define BGL_S64VREF( v, i ) (&(STVECTOR( v, BGL_LONGLONG_T )->obj0))[ i ]
#define BGL_S64VSET( v, i, o ) (BGL_S64VREF( v, i ) = o, BUNSPEC)
#define BGL_U64VREF( v, i ) (&(STVECTOR( v, unsigned BGL_LONGLONG_T )->obj0))[i]
#define BGL_U64VSET( v, i, o ) (BGL_U64VREF( v, i ) = o, BUNSPEC)

#define BGL_F32VREF( v, i ) (&(STVECTOR( v, float )->obj0))[ i ]
#define BGL_F32VSET( v, i, o ) (BGL_F32VREF( v, i ) = o, BUNSPEC)

#define BGL_F64VREF( v, i ) (&(STVECTOR( v, double )->obj0))[ i ]
#define BGL_F64VSET( v, i, o ) (BGL_F64VREF( v, i ) = o, BUNSPEC)

#define BGL_XXX_U8VREF( v, i, type ) \
   (*((type *)(&BGL_S8VREF( v, i ))))
#define BGL_XXX_U8VSET( v, i, o, type ) \
   (*((type *)(&BGL_S8VREF( v, i ))) = (o), BUNSPEC)
   
#define BGL_S16_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, int16_t )
#define BGL_S16_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, int16_t )
#define BGL_U16_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, uint16_t )
#define BGL_U16_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, uint16_t )
   
#define BGL_S32_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, int32_t )
#define BGL_S32_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, int32_t )
#define BGL_U32_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, uint32_t )
#define BGL_U32_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, uint32_t )
   
#define BGL_S64_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, int64_t )
#define BGL_S64_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, int64_t )
#define BGL_U64_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, uint64_t )
#define BGL_U64_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, uint64_t )
   
#define BGL_F32_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, float )
#define BGL_F32_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, float )
   
#define BGL_F64_U8VREF( v, i ) BGL_XXX_U8VREF( v, i, double )
#define BGL_F64_U8VSET( v, i, o ) BGL_XXX_U8VSET( v, i, o, double )
   
BGL_RUNTIME_DECL obj_t alloc_hvector( int, int, int );
   
#define BGL_ALLOC_S8VECTOR( len ) \
   alloc_hvector( len, sizeof( int8_t ), S8VECTOR_TYPE )

#define BGL_ALLOC_U8VECTOR( len ) \
   alloc_hvector( len, sizeof( uint8_t ), U8VECTOR_TYPE )

#define BGL_ALLOC_S16VECTOR( len ) \
   alloc_hvector( len, sizeof( int16_t ), S16VECTOR_TYPE )

#define BGL_ALLOC_U16VECTOR( len ) \
   alloc_hvector( len, sizeof( uint16_t ), U16VECTOR_TYPE )

#define BGL_ALLOC_S32VECTOR( len ) \
   alloc_hvector( len, sizeof( int32_t ), S32VECTOR_TYPE )

#define BGL_ALLOC_U32VECTOR( len ) \
   alloc_hvector( len, sizeof( uint32_t ), U32VECTOR_TYPE )

#define BGL_ALLOC_S64VECTOR( len ) \
   alloc_hvector( len, sizeof( BGL_LONGLONG_T ), S64VECTOR_TYPE )

#define BGL_ALLOC_U64VECTOR( len ) \
   alloc_hvector( len, sizeof( unsigned BGL_LONGLONG_T ), U64VECTOR_TYPE )

#define BGL_ALLOC_F32VECTOR( len ) \
   alloc_hvector( len, sizeof( float ), F32VECTOR_TYPE )

#define BGL_ALLOC_F64VECTOR( len ) \
   alloc_hvector( len, sizeof( double ), F64VECTOR_TYPE )

#define BGL_SU8VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_S8VREF( target, tstart ), (void *)&BGL_S8VREF( source, sstart ), \
	   (ssend - sstart) )
   
#define BGL_SU16VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_S16VREF( target, tstart ), (void *)&BGL_S16VREF( source, sstart ), \
      (ssend - sstart) * 2 )
   
#define BGL_SU32VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_S32VREF( target, tstart ), (void *)&BGL_S32VREF( source, sstart ), \
      (ssend - sstart) * 4 )
   
#define BGL_SU64VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_S64VREF( target, tstart ), (void *)&BGL_S64VREF( source, sstart ), \
      (ssend - sstart) * 8 )
   
#define BGL_F32VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_F32VREF( target, tstart ), (void *)&BGL_F32VREF( source, sstart ), \
      (ssend - sstart) * 4 )
   
#define BGL_F64VECTOR_COPY( target, tstart, source, sstart, ssend ) \
   memcpy( (void *)&BGL_F64VREF( target, tstart ), (void *)&BGL_F64VREF( source, sstart ), \
      (ssend - sstart) * 8 )
   

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif
