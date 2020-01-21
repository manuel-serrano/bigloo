/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/symbol.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 14 14:48:11 2003                          */
/*    Last change :  Tue Apr 17 09:01:52 2018 (serrano)                */
/*    Copyright   :  2003-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Custom symbol implementation                                     */
/*=====================================================================*/
#include <bigloo.h>
#include <esymbol.h>
#include <bmem.h>

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
   bmem_set_alloc_type( SYMBOL_TYPE_NUM, 0 );
   
   symbol = GC_MALLOC( ESYMBOL_SIZE );

#if BMEMDEBUG
   if( bmem_debug > 1 ) {
      fprintf( stderr, "make_symbol: %s %p\n", BSTRING_TO_STRING( name ), symbol );
   }
#endif
#if( !defined( TAG_SYMBOL ) )   
   symbol->symbol.header = MAKE_HEADER( SYMBOL_TYPE, SYMBOL_SIZE );
#endif
   
   symbol->symbol.string = name;
   symbol->symbol.cval   = BNIL;

   ((esymbol_t *)(symbol))->alloc_info = 0;
   ((esymbol_t *)(symbol))->class_alloc = -1;
   ((esymbol_t *)(symbol))->class_offset = 0;
   ((esymbol_t *)(symbol))->stamp = -3;

   return BSYMBOL( symbol );
}
   
/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    symbol_strcmp ...                                                */
/*---------------------------------------------------------------------*/
static bool_t
symbol_strcmp( obj_t o1, char *o2, long l2 ) {
   if( STRING_LENGTH( o1 ) == l2 ) {
      return !memcmp( (void *)BSTRING_TO_STRING( o1 ), o2, l2 );
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_bstring_to_symbol ...                                        */
/*---------------------------------------------------------------------*/
static obj_t
bgl_bstring_to_symbol( char *cname, long len ) {
   long hash_number;
   obj_t bucket;
   hash_number = ____get_hash_power_number_len( cname, SYMBOL_HASH_TABLE_SIZE_SHIFT, len );
   
   bucket = VECTOR_REF( ____bgl_get_symtab(), hash_number );
   
   if( NULLP( bucket ) ) {
      obj_t symbol = make_symbol( ____string_to_bstring_len( cname, len) );
      obj_t pair = MAKE_PAIR( symbol, BNIL );

      VECTOR_SET( ____bgl_get_symtab(), hash_number, pair );
      
      return symbol;
   } else {
      obj_t run = bucket, back = bucket;
      
      while( !NULLP( run ) &&
	     SYMBOL( CAR( run ) ).string &&
	     !symbol_strcmp( SYMBOL( CAR( run ) ).string, cname, len ) )
         back = run, run = CDR( run );
      
      if( !NULLP( run ) ) {
         return CAR( run );
      } else {
         obj_t symbol = make_symbol( ____string_to_bstring_len( cname, len) );
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
      BSTRING_TO_STRING( name ), STRING_LENGTH( name ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_string_to_symbol_len ...                                     */
/*---------------------------------------------------------------------*/
obj_t
bgl_string_to_symbol_len( char *cname, long len ) {
   return bgl_bstring_to_symbol( cname, len );
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    string_to_symbol ...                                             */
/*---------------------------------------------------------------------*/
obj_t
string_to_symbol( char *cname ) {
   return bgl_bstring_to_symbol( cname, strlen( cname ) );
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
