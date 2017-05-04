/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Include/bigloo_object.h      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 08:05:01 2016                          */
/*    Last change :  Thu May  4 07:45:53 2017 (serrano)                */
/*    Copyright   :  2016-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo OBJECTs                                                   */
/*=====================================================================*/
#ifndef BIGLOO_OBJECT_H 
#define BIGLOO_OBJECT_H

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
/*    types                                                            */
/*---------------------------------------------------------------------*/
/* old name mangling framework */
typedef struct __object_bgl {
   header_t header;
   obj_t widening;
} *object_bglt;

/* bootstrap configuration */   
typedef struct __bgl__object_00_bgl {
   header_t header;
   obj_t widening;
} *bgl__object_00_bglt;
   
/* new name mangling framework */   
typedef struct BgL__object_00_bgl {
   header_t header;
   obj_t widening;
} *BgL__object_00_bglt;
   
typedef struct BgL_objectz00_bgl {
   header_t header;
   obj_t widening;
} *BgL_objectz00_bglt;

#define BOBJECT( r ) ((obj_t)((long)r + TAG_STRUCT))
#define COBJECT( r ) ((obj_t)((long)r - TAG_STRUCT))

/*---------------------------------------------------------------------*/
/*    Object macros                                                    */
/*---------------------------------------------------------------------*/
#define BGL_OBJECTP( _obj ) \
   ((POINTERP( _obj ) && (TYPE( _obj ) >= OBJECT_TYPE)))

#define BGL_OBJECT_MIN_DISPLAY_SIZE 6

#define BGL_OBJECT_CLASS_NUM( _obj ) \
   (TYPE( _obj ))

#define BGL_OBJECT_CLASS_NUM_SET( _1, _2 ) \
   (((obj_t)COBJECT(_1))->header = MAKE_HEADER( _2, 0 ), BUNSPEC)
   
#define BGL_OBJECT_WIDENING( _obj ) \
   (((object_bglt)(COBJECT(_obj)))->widening)

#define BGL_OBJECT_WIDENING_SET( _obj, _wdn ) \
   BASSIGN( BGL_OBJECT_WIDENING( _obj ), _wdn, (obj_t)_obj )

/*---------------------------------------------------------------------*/
/*    Classes                                                          */
/*---------------------------------------------------------------------*/
#define BGL_CLASSP( o ) (POINTERP( o ) && (TYPE( o ) == CLASS_TYPE))

#define BGL_CLASS_SIZE (sizeof( struct bgl_class ) )
#define BGL_CLASS( f ) (CREF( f )->class_t)
   
#define BGL_CLASS_NAME( f ) (BGL_CLASS( f ).name)
   
#define BGL_CLASS_INDEX( f ) (BGL_CLASS( f ).index)
   
#define BGL_CLASS_DEPTH( f ) (BGL_CLASS( f ).depth)
   
#define BGL_CLASS_SUPER( f ) (BGL_CLASS( f ).super)
#define BGL_CLASS_ANCESTORS( f ) (BGL_CLASS( f ).ancestors)
#define BGL_CLASS_ANCESTORS_REF( f, i ) (&(BGL_CLASS( f ).ancestor0))[ i ]
   
#define BGL_CLASS_SUBCLASSES( f ) (BGL_CLASS( f ).subclasses)
#define BGL_CLASS_SUBCLASSES_SET( f, v ) BASSIGN( BGL_CLASS_SUBCLASSES( f ), v, f )
   
#define BGL_CLASS_DIRECT_FIELDS( f ) (BGL_CLASS( f ).direct_fields)
#define BGL_CLASS_DIRECT_FIELDS_SET( f, v ) BASSIGN( BGL_CLASS_DIRECT_FIELDS( f ), v, f )
   
#define BGL_CLASS_ALL_FIELDS( f ) (BGL_CLASS( f ).all_fields)
#define BGL_CLASS_ALL_FIELDS_SET( f, v ) BASSIGN( BGL_CLASS_ALL_FIELDS( f ), v, f )
   
#define BGL_CLASS_VIRTUAL_FIELDS( f ) (BGL_CLASS( f ).virtual_fields)
   
#define BGL_CLASS_MODULE( f ) (BGL_CLASS( f ).module)
   
#define BGL_CLASS_ALLOC_FUN( f ) (BGL_CLASS( f ).alloc_fun)

#define BGL_CLASS_HASH( f ) (BGL_CLASS( f ).hash)
   
#define BGL_CLASS_NEW_FUN( f ) (BGL_CLASS( f ).new_fun)
   
#define BGL_CLASS_NIL_FUN( f ) (BGL_CLASS( f ).nil_fun)
   
#define BGL_CLASS_NIL( f ) (BGL_CLASS( f ).nil)
#define BGL_CLASS_NIL_SET( f, v ) BASSIGN( BGL_CLASS_NIL( f ), v, f )
   
#define BGL_CLASS_CONSTRUCTOR( f ) (BGL_CLASS( f ).constructor)
   
#define BGL_CLASS_SHRINK( f ) (BGL_CLASS( f ).shrink)
   
#define BGL_CLASS_EVDATA( f ) (BGL_CLASS( f ).evdata)   
#define BGL_CLASS_EVDATA_SET( f, o ) (BGL_CLASS_EVDATA( f ) = o)
   
/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif
