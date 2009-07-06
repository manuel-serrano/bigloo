/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bde/bmem/lib/symbol.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 14 14:48:11 2003                          */
/*    Last change :  Thu Aug 28 15:20:52 2008 (serrano)                */
/*    Copyright   :  2003-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Custom symbol implementation                                     */
/*=====================================================================*/
#include <bigloo.h>
#include <esymbol.h>

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
extern void *(*____bgl_get_symtab)();
extern long (*____get_hash_power_number)( char *, unsigned long );
extern void *(*____string_to_bstring)( char * );

extern int bmem_debug;

/*---------------------------------------------------------------------*/
/*    make_symbol ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
make_symbol( obj_t name ) {
   obj_t symbol;

   set_alloc_type( SYMBOL_TYPE );
   
   symbol = GC_MALLOC( ESYMBOL_SIZE );

#if BMEMDEBUG
   if( bmem_debug > 1 ) {
      fprintf( stderr, "make_symbol: %s %p\n", name, symbol );
   }
#endif
   symbol->symbol_t.header = MAKE_HEADER( SYMBOL_TYPE, SYMBOL_SIZE );
   symbol->symbol_t.string = name;
   symbol->symbol_t.cval   = BNIL;
   
   ((esymbol_t *)(symbol))->alloc_info = 0;
   ((esymbol_t *)(symbol))->class_alloc = -1;
   ((esymbol_t *)(symbol))->stamp = -3;

   return BREF( symbol );
}
   
/*---------------------------------------------------------------------*/
/*    bstring_to_symbol ...                                            */
/*    obj_t --> obj_t                                                  */
/*---------------------------------------------------------------------*/
obj_t
bstring_to_symbol( obj_t name ) {
   long hash_number;
   obj_t bucket;
   char *cname = BSTRING_TO_STRING( name );

   hash_number = ____get_hash_power_number( cname, SYMBOL_HASH_TABLE_SIZE_SHIFT );
   bucket = VECTOR_REF( ____bgl_get_symtab(), hash_number );
   
   if( NULLP( bucket ) ) {
      obj_t symbol = make_symbol( name );
      obj_t pair   = MAKE_PAIR( symbol, BNIL );
      
      VECTOR_SET( ____bgl_get_symtab(), hash_number, pair );
      
      return symbol;
   } else {
      obj_t run = bucket, back = bucket;
      
      while( !NULLP( run ) &&
	     strcmp( (char *)BSTRING_TO_STRING( SYMBOL( CAR( run ) ).string ),
		     cname ) )
         back = run, run = CDR( run );
      
      if( !NULLP( run ) )
         return CAR( run );
      else {
         obj_t symbol = make_symbol( name );
	 obj_t pair   = MAKE_PAIR( symbol, BNIL );
	 
         SET_CDR( back, pair );

         return symbol;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    string_to_symbol ...                                             */
/*    char * --> obj_t                                                 */
/*---------------------------------------------------------------------*/
obj_t
string_to_symbol( char *cname ) {
   long hash_number;
   obj_t bucket;

   hash_number = ____get_hash_power_number( cname, SYMBOL_HASH_TABLE_SIZE_SHIFT );
   bucket = VECTOR_REF( ____bgl_get_symtab(), hash_number );

   if( NULLP( bucket ) ) {
      obj_t symbol = make_symbol( (obj_t)____string_to_bstring( cname ) );
      obj_t pair   = MAKE_PAIR( symbol, BNIL );
      
      VECTOR_SET( ____bgl_get_symtab(), hash_number, pair );
      
      return symbol;
   } else {
      obj_t run = bucket, back = bucket;
      
      while( !NULLP( run ) &&
	     strcmp( (char *)BSTRING_TO_STRING( SYMBOL( CAR( run ) ).string ),
		     cname ) )
         back = run, run = CDR( run );
      
      if( !NULLP( run ) )
         return CAR( run );
      else {
         obj_t symbol = make_symbol( (obj_t)____string_to_bstring( cname ) );
	 obj_t pair   = MAKE_PAIR( symbol, BNIL );
	 
         SET_CDR( back, pair );

         return symbol;
      }
   }
}


