/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/csaw.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar  3 17:05:58 2016                          */
/*    Last change :  Sun Mar 18 07:19:43 2018 (serrano)                */
/*    Copyright   :  2016-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C Saw memory management.                                         */
/*=====================================================================*/
#if( BGL_SAW == 1 )

#include <bigloo.h>

#if( BGL_GC == BGL_SAW_GC )

/*---------------------------------------------------------------------*/
/*    constants                                                        */
/*---------------------------------------------------------------------*/
#define BGL_SAW_NURSERY_SIZE (1024 * 4 * OBJ_SIZE)

/*---------------------------------------------------------------------*/
/*    nursery                                                          */
/*---------------------------------------------------------------------*/
bgl_saw_nursery_t bgl_saw_nursery;
long bgl_saw_nursery_size = BGL_SAW_NURSERY_SIZE;

void dump_nursery(char *msg) {
  bgl_saw_nursery_t *o = &bgl_saw_nursery;
  fprintf(stderr, "%s in %p %p %p %p\n", msg,
	  o->heap, o->alloc, o->backptr, o->backpool);
}

/*---------------------------------------------------------------------*/
/*    copiers                                                          */
/*---------------------------------------------------------------------*/
static long bgl_saw_copiers_size;
static bgl_saw_copier_t *bgl_saw_copiers;

static obj_t bgl_saw_pair_copy( obj_t );
static obj_t bgl_saw_real_copy( obj_t );
static obj_t bgl_saw_cell_copy( obj_t );

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_saw_init ...                                                 */
/*---------------------------------------------------------------------*/
void
bgl_saw_init() {
   /* alloc the nursery */
   char *heap = (char *)GC_MALLOC( bgl_saw_nursery_size );
   bgl_saw_nursery.size = bgl_saw_nursery_size;
   bgl_saw_nursery.heap = heap;
   bgl_saw_nursery.alloc = heap;
   bgl_saw_nursery.backpool = (obj_t **)(heap + bgl_saw_nursery_size - OBJ_SIZE);
   bgl_saw_nursery.backptr = bgl_saw_nursery.backpool;

   dump_nursery("init");

   /* builtin type info */
   bgl_saw_copiers_size = OBJECT_TYPE;
   bgl_saw_copiers = malloc( sizeof( bgl_saw_copier_t ) * bgl_saw_copiers_size );

   bgl_saw_gc_add_copier( PAIR_TYPE, bgl_saw_pair_copy );
   bgl_saw_gc_add_copier( REAL_TYPE, bgl_saw_real_copy );
   bgl_saw_gc_add_copier( VECTOR_TYPE, bgl_saw_vector_copy );
   bgl_saw_gc_add_copier( CELL_TYPE, bgl_saw_cell_copy );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_saw_gc_add_copier ...                                        */
/*---------------------------------------------------------------------*/
void
bgl_saw_gc_add_copier( long type, bgl_saw_copier_t copier ) {
   if( type > bgl_saw_copiers_size ) {
      /* enlarge the copiers on demand */
      long new_size = sizeof( bgl_saw_copier_t ) * (type + 10);
      long old_size = bgl_saw_copiers_size * sizeof( bgl_saw_copier_t );
      bgl_saw_copier_t *old = bgl_saw_copiers;
      bgl_saw_copiers = malloc( new_size );

      memcpy( bgl_saw_copiers, old, old_size );
      bgl_saw_copiers_size = new_size;
   }

   bgl_saw_copiers[ type ] = copier;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_saw_gc ...                                                   */
/*---------------------------------------------------------------------*/
int nb_alloc = 0;
int nb_assign = 0;
int nb_copy = 0;

typedef struct real_frame_struct {
  bgl_saw_frame_header_t header;
  obj_t values[];
} *real_frame_def_t;

obj_t trace_obj(obj_t o) {
  //fprintf(stderr, "trace obj %p\n", o);
  if(BYOUNGP(o)) {
    obj_t oo = CREF(o);
    if(!((oo >= (obj_t) bgl_saw_nursery.heap) &&
	 (oo <= (obj_t) bgl_saw_nursery.backpool))) {
      fprintf(stderr, "YOUNG not in heap %p\n", o);
      exit(-1);
    }
    header_t type = oo->header;
    if(!(type & 1)) {
      //fprintf(stderr, "\ttrace obj %p %X\n", oo, type);
      oo->header |= 1;
      return(bgl_saw_copiers[ HEADER_TYPE( type ) ]( oo ));
    } else {
      //fprintf(stderr, "\ttraced obj %p %X %p\n", oo, type, oo->pair_t.car);
      return(oo->pair_t.car);
    }
  } else {
    return(o);
  }
}

void trace(obj_t *p) {
  obj_t o = *p;
  //fprintf(stderr, "trace obj %p\n", o);
  if(BYOUNGP(o)) {
    obj_t oo = CREF(o);
    if(!((oo >= (obj_t) bgl_saw_nursery.heap) &&
	 (oo <= (obj_t) bgl_saw_nursery.backpool))) {
      fprintf(stderr, "YOUNG not in heap %p\n", o);
      exit(-1);
    }
    header_t type = oo->header;
    if(!(type & 1)) {
      //fprintf(stderr, "\ttrace %p %X\n", oo, type);
      oo->header |= 1;
      *p = bgl_saw_copiers[ HEADER_TYPE( type ) ]( oo );
    } else {
      //fprintf(stderr, "\ttraced obj %p %X %p\n", oo, type, oo->pair_t.car);
      *p = oo->pair_t.car;
    }
  }
}

void trace_frames(real_frame_def_t lpf) {
  while(lpf) {
    int n = lpf->header.size;
    //fprintf(stderr, "***** trace frame of size %d\n", n);
    for(int i=0; i<n; i++) {
      //fprintf(stderr, "** frame[%x] = %p\n", i, lpf->values[i]);
      trace(&(lpf->values[i]));
    }
    lpf = (real_frame_def_t) lpf->header.link;
  }
}

void
bgl_saw_gc() {
   obj_t **ptr = bgl_saw_nursery.backpool;

   dump_nursery("gc");
   trace_frames((real_frame_def_t) BGL_ENV_SAW_SP(BGL_CURRENT_DYNAMIC_ENV()));
   while( ptr > bgl_saw_nursery.backptr ) {
     trace(*ptr);
     ptr--;
   }

   bgl_saw_nursery.backptr = bgl_saw_nursery.backpool;
   bgl_saw_nursery.alloc = bgl_saw_nursery.heap;
   fprintf(stderr, "gc alloc %d, back %d, copied %d\n",
	   nb_alloc, nb_assign, nb_copy);
   nb_alloc = nb_assign = nb_copy = 0;
}

  
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_gc_copy ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_gc_copy( obj_t obj ) {
   fprintf(stderr, "NEVER saw_gc_copy\n");
        exit(-1);
	/*
   if( TYPE( obj ) != NO_TYPE ) {
      obj_t new = bgl_saw_copiers[ TYPE( obj ) ]( obj );
      TYPE( obj ) == NO_TYPE;
      CAR( obj ) = new;
   }

   return CAR( obj );
	*/
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_pair ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_pair( obj_t a, obj_t d ) {
   char *res = bgl_saw_nursery.alloc;
   char *nalloc = res + MEMROUND( PAIR_SIZE );
   
   if( nalloc >= (char *) bgl_saw_nursery.backptr ) {
      bgl_saw_gc();
      res = bgl_saw_nursery.alloc;
      nalloc = res + MEMROUND( PAIR_SIZE );
      if(nalloc >= (char *)bgl_saw_nursery.backptr) {
	 fprintf( stderr, "fatal error not enough space in nursery\n" );
	 exit( -1 );
      }
      a = trace_obj( a );
      d = trace_obj( d );
   }
   
   bgl_saw_nursery.alloc = nalloc;
   nb_alloc++;
   obj_t an_object = (obj_t)res;

   BGL_INIT_PAIR( (obj_t)res, a, d );

   return BYOUNG( (obj_t)res );
}   

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_old_pair ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_old_pair( obj_t a, obj_t d ) {
   obj_t res = GC_MALLOC( PAIR_SIZE );

   BGL_INIT_PAIR( res, a, d );

   BBACKPTR( CAR( BPAIR( res ) ), a );
   BBACKPTR( CDR( BPAIR( res ) ), d );

   return BPAIR( res );
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_epair ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_epair( obj_t a, obj_t d, obj_t e ) {
   char *res = bgl_saw_nursery.alloc;
   char *nalloc = res + MEMROUND( EPAIR_SIZE );
   
   if( nalloc >= (char *) bgl_saw_nursery.backptr ) {
      bgl_saw_gc();
      res = bgl_saw_nursery.alloc;
      nalloc = res + MEMROUND( EPAIR_SIZE );
      if(nalloc >= (char *)bgl_saw_nursery.backptr) {
	 fprintf( stderr, "fatal error not enough space in nursery\n" );
	 exit( -1 );
      }
      a = trace_obj( a );
      d = trace_obj( d );
      e = trace_obj( e );
   }
   
   bgl_saw_nursery.alloc = nalloc;
   nb_alloc++;
   obj_t an_object = (obj_t)res;

   BGL_INIT_EPAIR( (obj_t)res, a, d, e );

   return BYOUNG( (obj_t)res );
}   

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_old_epair ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_old_epair( obj_t a, obj_t d, obj_t e ) {
   obj_t res = GC_MALLOC( EPAIR_SIZE );

   BGL_INIT_EPAIR( res, a, d, e );

   BBACKPTR( CAR( BPAIR( res ) ), a );
   BBACKPTR( CDR( BPAIR( res ) ), d );
   BBACKPTR( CER( BPAIR( res ) ), e );

   return BPAIR( res );
}
   
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_saw_pair_copy ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
bgl_saw_pair_copy( obj_t pair ) {
   if( EPAIRP( pair ) ) {
      obj_t an_object = GC_MALLOC( EPAIR_SIZE );
      obj_t save = pair->pair_t.car;
      pair->pair_t.car = BPAIR( an_object );
      nb_copy++;
      //fprintf(stderr, "%p\n", an_object);
      
      BGL_INIT_EPAIR( an_object,
		      trace_obj( save ),
		      trace_obj( pair->pair_t.cdr ),
		      trace_obj( pair->epair_t.cer ) );

      return BPAIR( an_object );
   } else {
     //fprintf(stderr, "\t\tcopy pair %p=(%p %p) -> ",
     //	     pair, pair->pair_t.car, pair->pair_t.cdr);
     obj_t an_object = GC_MALLOC( PAIR_SIZE );
     obj_t save = pair->pair_t.car;
     pair->pair_t.car = BPAIR( an_object );
     nb_copy++;
     //fprintf(stderr, "%p\n", an_object);
     
     BGL_INIT_PAIR( an_object,
		    trace_obj( save ),
		    trace_obj( pair->pair_t.cdr ) );

     return BPAIR( an_object );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_real ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_real( double d ) {
   obj_t an_object;

   //   fprintf(stderr, "NEVER make_real\n");
   //   bgl_saw_gc();
   BGL_MAKE_INLINE_REAL( d );

   return BREAL( an_object );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_saw_real_copy ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
bgl_saw_real_copy( obj_t real ) {
     fprintf(stderr, "copying real not yet\n");
     exit(-1);
     //   return make_real( REAL_TO_DOUBLE( real ) );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_saw_vector_copy ...                                          */
/*---------------------------------------------------------------------*/
#if( BGL_GC == BGL_SAW_GC )    
BGL_RUNTIME_DEF obj_t
bgl_saw_vector_copy( obj_t old ) {
     fprintf(stderr, "copying vector not yet\n");
     exit(-1);
     /*
   long i = VECTOR_LENGTH( old ) - 1;
   obj_t new = create_vector( i );

   while( i-- >= 0 ) {
      VECTOR_REF( new, i ) = bgl_saw_gc_copy( VECTOR_REF( old, i ) );
   }

   return new;
     */
}
#endif

/*
 * Not yet macros for debug purpose
 */
void bps_dobackptr(obj_t *field, obj_t value) {
  if((char *)(bgl_saw_nursery.backptr) <= bgl_saw_nursery.alloc) {
    bgl_saw_gc();
    *field = trace_obj(value);
  } else {
    nb_assign++;
    *(bgl_saw_nursery.backptr) = field;
    bgl_saw_nursery.backptr -= 1;
    *field = value;
  }
}

void bps_bmassign(obj_t *field, obj_t value) {
  if(BYOUNGP( value )) {
    bps_dobackptr(field, value);
  } else {
    *field = value;
  }
}

void bps_bassign(obj_t *field, obj_t value, obj_t obj) {
  if(BOLDP( obj ) && BYOUNGP( value )) {
    bps_dobackptr(field, value);
  } else {
    *field = value;
  }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_cell ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_cell( obj_t v ) {
   char *res = bgl_saw_nursery.alloc;
   char *nalloc = res + MEMROUND( CELL_SIZE );
   
   if( nalloc >= (char *) bgl_saw_nursery.backptr ) {
      bgl_saw_gc();
      res = bgl_saw_nursery.alloc;
      nalloc = res + MEMROUND( CELL_SIZE );
      if(nalloc >= (char *)bgl_saw_nursery.backptr) {
	 fprintf( stderr, "fatal error not enough space in nursery\n" );
	 exit( -1 );
      }
      v = trace_obj( v );
   }
   
   bgl_saw_nursery.alloc = nalloc;
   nb_alloc++;
   obj_t an_object = (obj_t)res;

   BGL_INIT_CELL( (obj_t)res, v );

   return BYOUNG( (obj_t)res );
}   

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_old_cell ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_old_cell( obj_t v ) {
   obj_t res = GC_MALLOC( CELL_SIZE );

   BGL_INIT_CELL( res, v );

   BBACKPTR( res->cell_t.val, v );

   return BCELL( res );
}
   
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_saw_cell_copy ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
bgl_saw_cell_copy( obj_t cell ) {
   obj_t an_object = GC_MALLOC( CELL_SIZE );
   obj_t save = cell->cell_t.val;
   cell->cell_t.val = BCELL( an_object );
   nb_copy++;

   BGL_INIT_CELL( an_object, trace_obj( save ) );

   return BCELL( an_object );
}

#endif
#endif
