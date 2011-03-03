/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bdb/blib/stat.c                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 10 08:43:29 2000                          */
/*    Last change :  Wed Mar  2 18:49:40 2011 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Statistics handling (mostly hash tables).                        */
/*=====================================================================*/
#define I_HIDE_POINTERS
#include <stdio.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    The macro that constructs a Bigloo string                        */
/*---------------------------------------------------------------------*/
#define MAKE_BSTRING( str ) SYMBOL_TO_STRING( string_to_symbol( str ) )

/*---------------------------------------------------------------------*/
/*    Because we don't have to count the local allocations, we have    */
/*    to reset the allocation mark routines.                           */
/*---------------------------------------------------------------------*/
#undef BGL_HEAP_DEBUG_MARK_OBJ
#undef BGL_HEAP_DEBUG_MARK_STR
#define BGL_HEAP_DEBUG_MARK_OBJ( x ) x
#define BGL_HEAP_DEBUG_MARK_STR( x ) x

/*---------------------------------------------------------------------*/
/*    Importations ...                                                 */
/*---------------------------------------------------------------------*/
extern long get_hash_number( char * );
extern long get_hash_number_from_int( unsigned long );
extern long bgl_types_number();
extern obj_t create_vector();

/*---------------------------------------------------------------------*/
/*    Exportations ...                                                 */
/*---------------------------------------------------------------------*/
/* live objects */
extern void bdb_live_producer_reset();
extern void bdb_live_producer_add( char *, long );
extern obj_t bdb_live_producer_list();
/* allocated objects */
extern void bdb_allocated_producer_reset();
extern void bdb_allocated_producer_add( char *, long );
extern obj_t bdb_allocated_producer_list();

/*---------------------------------------------------------------------*/
/*    Hash tables data type ...                                        */
/*---------------------------------------------------------------------*/
#define HASH_TABLE_MAX_ENTRIES 1024

typedef struct producer_info {
   /* the producer's name */
   char *name;
   /* the number of allocations from the producer */
   long count;
   /* the allocated type by the producer */
   long *types;
   /* next entry */
   struct producer_info *next;
} *producer_t;

static producer_t live_producers[ HASH_TABLE_MAX_ENTRIES ];
static producer_t allocated_producers[ HASH_TABLE_MAX_ENTRIES ];

/*---------------------------------------------------------------------*/
/*    NAME_EGAL                                                        */
/*    -------------------------------------------------------------    */
/*    This macro test if two names are equal. If the names are         */
/*    deduced from symbol it is correct to use ==. Otherwise, it       */
/*    should expand to !strcmp.                                        */
/*    -------------------------------------------------------------    */
/*    In addition, choosing == instead of strcmp requires that         */
/*    symbols are never collected. If they are collected,              */
/*    MAKE_PRODUCER_INFO must duplicate the string NAME and so         */
/*    strings are never ==.                                            */
/*---------------------------------------------------------------------*/
#define UNCOLLECTED_SYMBOLS

#if( defined( UNCOLLECTED_SYMBOLS ) )
#   define NAME_EGAL( n1, n2 ) ((n1) == (n2))
#else
#   define NAME_EGAL( n1, n2 ) (!strcmp(n1,n2))
#endif

/*---------------------------------------------------------------------*/
/*    static producer_t                                                */
/*    make_producer_info ...                                           */
/*---------------------------------------------------------------------*/
static producer_t
make_producer_info( char *name, producer_t next ) {
   producer_t new;

   new = malloc( sizeof( struct producer_info ) );

   new->count = 1;
   new->next = next;
   new->types = calloc( bgl_types_number() + 1, sizeof( long ) );
#if( defined( UNCOLLECTED_SYMBOLS ) )
   new->name = name;
#else
   new->name = strdup( name );
#endif
   return new;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    gc_make_pair ...                                                 */
/*    -------------------------------------------------------------    */
/*    Because there is no guaranty that MAKE_PAIR calls can be         */
/*    nested, we use our local function.                               */
/*---------------------------------------------------------------------*/
static obj_t
gc_make_pair( obj_t car, obj_t cdr ) {
   return MAKE_PAIR( car, cdr );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    cvector_to_bvector ...                                           */
/*    -------------------------------------------------------------    */
/*    Convert a C allocated vector into a Bigloo vector.               */
/*---------------------------------------------------------------------*/
static obj_t
cvector_to_bvector( long *vec, long len ) {
   int i;
   obj_t res = create_vector( len );

   for( i = 0; i < len; i++ )
      VECTOR_SET( res, i, BINT( vec[ i ] ) );

   return res;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bdb_producer_reset ...                                           */
/*    -------------------------------------------------------------    */
/*    Reset (i.e. flush out) an hash table.                            */
/*---------------------------------------------------------------------*/
static void
bdb_producer_reset( producer_t *producers ) {
   long i;

   for( i = 0; i < HASH_TABLE_MAX_ENTRIES; i++ ) {
      if( producers[ i ] ) {
	 free( producers[ i ]->types );
	 free( producers[ i ] );
	 producers[ i ] = 0;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bdb_producer_add ...                                             */
/*    -------------------------------------------------------------    */
/*    Put a new producer allocation into the appropriate hash table.   */
/*---------------------------------------------------------------------*/
static void
bdb_producer_add( producer_t *producers, char *alloc, long type ) {
   long hnum = get_hash_number( alloc ) % HASH_TABLE_MAX_ENTRIES;

   assert( type <= bgl_types_number() );

   if( !producers[ hnum ] ) {
      producers[ hnum ] = make_producer_info( alloc, 0 );
      producers[ hnum ]->types[ type ]++;
   }
   else {
      producer_t run;

      for( run = producers[ hnum ]; run; run = run->next )
	 if( NAME_EGAL( run->name, alloc ) ) break;

      if( run ) {
	 run->types[ type ]++;
	 run->count++;
      }
      else {
	 producers[ hnum ] = make_producer_info( alloc, producers[ hnum ] );
	 producers[ hnum ]->types[ type ]++;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bdb_producer_list ...                                            */
/*    -------------------------------------------------------------    */
/*    Convert the hash table into a Bigloo list.                       */
/*    -------------------------------------------------------------    */
/*    The C type couting vectors are converted into Bigloo vector      */
/*    in this function. This may seem stupid because we have           */
/*    to duplicate allcation (we have allocated a C vector and         */
/*    now we allocate a Bigloo vector). However, this technique        */
/*    enable not to declare the C allocated chunk as GC roots.         */
/*---------------------------------------------------------------------*/
static obj_t
bdb_producer_list( producer_t *producers ) {
   obj_t pair = BNIL;
   long i;
   long l = bgl_types_number();

   for( i = 0; i < HASH_TABLE_MAX_ENTRIES; i++ ) {
      if( producers[ i ] ) {
	 producer_t run;

	 for( run = producers[ i ]; run; run = run->next ) {
	    obj_t cell;
	    
	    /* allocate the Bigloo object describing this allocator */
	    cell = gc_make_pair( MAKE_BSTRING( run->name ),
				 gc_make_pair( BINT( run->count ),
					       cvector_to_bvector( run->types, 
								   l + 1 ) ) );

	    pair = MAKE_PAIR( cell, pair );
	 }
      }
   }

   return pair;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bdb_live_producer_reset ...                                      */
/*---------------------------------------------------------------------*/
void
bdb_live_producer_reset() {
   bdb_producer_reset( live_producers );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bdb_live_producer_add ...                                        */
/*---------------------------------------------------------------------*/
void
bdb_live_producer_add( char *alloc, long type ) {
   bdb_producer_add( live_producers, alloc, type );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bdb_live_producer_list ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bdb_live_producer_list() {
   return bdb_producer_list( live_producers );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bdb_allocated_producer_reset ...                                 */
/*---------------------------------------------------------------------*/
void
bdb_allocated_producer_reset() {
   bdb_producer_reset( allocated_producers );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bdb_allocated_producer_add ...                                   */
/*---------------------------------------------------------------------*/
void
bdb_allocated_producer_add( char *alloc, long type ) {
   bdb_producer_add( allocated_producers, alloc, type );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bdb_allocated_producer_list ...                                  */
/*---------------------------------------------------------------------*/
obj_t
bdb_allocated_producer_list() {
   return bdb_producer_list( allocated_producers );
}
	       

   
 
