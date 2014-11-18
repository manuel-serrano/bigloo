/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bde/bmem/lib/symbol.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 14 14:48:11 2003                          */
/*    Last change :  Fri Nov 14 17:50:19 2014 (serrano)                */
/*    Copyright   :  2003-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Custom symbol implementation                                     */
/*=====================================================================*/
#include <bigloo.h>
#include <esymbol.h>
#include <bmem.h>

extern void set_alloc_type( int, int  );

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
extern int bmem_debug;

/*---------------------------------------------------------------------*/
/*    make_symbol ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
make_symbol( obj_t name ) {
   obj_t symbol;
   set_alloc_type( SYMBOL_TYPE_NUM, 0 );
   
   symbol = GC_MALLOC( ESYMBOL_SIZE );

#if BMEMDEBUG
   if( bmem_debug > 1 ) {
      fprintf( stderr, "make_symbol: %s %p\n", BSTRING_TO_STRING( name ), symbol );
   }
#endif
#if( !defined( TAG_SYMBOL ) )   
   symbol->symbol_t.header = MAKE_HEADER( SYMBOL_TYPE, SYMBOL_SIZE );
#endif
   
   symbol->symbol_t.string = name;
   symbol->symbol_t.cval   = BNIL;

   ((esymbol_t *)(symbol))->alloc_info = 0;
   ((esymbol_t *)(symbol))->class_alloc = -1;
   ((esymbol_t *)(symbol))->class_offset = 0;
   ((esymbol_t *)(symbol))->stamp = -3;

   return BSYMBOL( symbol );
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_bstring_to_symbol ...                                        */
/*---------------------------------------------------------------------*/
static obj_t
bgl_bstring_to_symbol( obj_t name ) {
   long hash_number;
   obj_t bucket;
   char *cname = BSTRING_TO_STRING( name );
   hash_number = ____get_hash_power_number( cname, SYMBOL_HASH_TABLE_SIZE_SHIFT );
   
   bucket = VECTOR_REF( ____bgl_get_symtab(), hash_number );
   
   if( NULLP( bucket ) ) {
      obj_t symbol = make_symbol( name );
      obj_t pair = MAKE_PAIR( symbol, BNIL );

      VECTOR_SET( ____bgl_get_symtab(), hash_number, pair );
      
      return symbol;
   } else {
      obj_t run = bucket, back = bucket;
      
      while( !NULLP( run ) &&
	     SYMBOL( CAR( run ) ).string &&
	     !bigloo_strcmp( SYMBOL( CAR( run ) ).string, name ) )
         back = run, run = CDR( run );
      
      if( !NULLP( run ) ) {
         return CAR( run );
      }
      else {
         obj_t symbol = make_symbol( name );
	 obj_t pair = MAKE_PAIR( symbol, BNIL );
	 
         SET_CDR( back, pair );

         return symbol;
      }
   }
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bstring_to_symbol ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bstring_to_symbol( obj_t name ) {
   return bgl_bstring_to_symbol(
      ____string_to_bstring_len(
	 BSTRING_TO_STRING( name ), STRING_LENGTH( name ) ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_string_to_symbol_len ...                                     */
/*---------------------------------------------------------------------*/
obj_t
bgl_string_to_symbol_len( char *cname, long len ) {
   return bgl_bstring_to_symbol( ____string_to_bstring_len( cname, len ) );
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    string_to_symbol ...                                             */
/*---------------------------------------------------------------------*/
obj_t
string_to_symbol( char *cname ) {
   return bgl_bstring_to_symbol( ____string_to_bstring( cname ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gensym ...                                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_gensym( obj_t name ) {
   obj_t o = make_symbol( 0L );
   
   if( name == BFALSE ) {
      return o;
   } else {
      bgl_symbol_genname( o, BSTRING_TO_STRING( name ) );
      return o;
   }
}
