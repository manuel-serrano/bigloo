/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cdsssl.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  SERRANO Manuel                                    */
/*    Creation    :  Thu Apr  3 11:37:14 1997                          */
/*    Last change :  Thu Aug 28 15:45:56 2008 (serrano)                */
/*    -------------------------------------------------------------    */
/*    C Dsssl support.                                                 */
/*=====================================================================*/
#include <string.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Some prototypes                                                  */
/*---------------------------------------------------------------------*/
extern obj_t make_vector( int, obj_t );
extern long get_hash_power_number( char *, unsigned long );

/*---------------------------------------------------------------------*/
/*    Global Keyword table                                             */
/*---------------------------------------------------------------------*/
static obj_t c_keytab;

/*---------------------------------------------------------------------*/
/*    Keyword mutex                                                    */
/*---------------------------------------------------------------------*/
static obj_t keyword_mutex = BUNSPEC;
DEFINE_STRING( keyword_mutex_name, _1, "keyword-mutex", 12 );

/*---------------------------------------------------------------------*/
/*    bgl_init_keyword_table ...                                       */
/*---------------------------------------------------------------------*/
void bgl_init_keyword_table() {
   if( !VECTORP( c_keytab ) ) {
      c_keytab = make_vector( KEYWORD_HASH_TABLE_SIZE, BNIL );
      keyword_mutex = bgl_make_mutex( keyword_mutex_name );
   }
}
           
/*---------------------------------------------------------------------*/
/*    make_keyword ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
make_keyword( obj_t name ) {
   obj_t keyword;

   keyword = GC_MALLOC( KEYWORD_SIZE );
   
   keyword->symbol_t.header = MAKE_HEADER( KEYWORD_TYPE, KEYWORD_SIZE );
   keyword->symbol_t.string = name;
   keyword->symbol_t.cval   = BNIL;
   
   return BREF( keyword );
}
   
/*---------------------------------------------------------------------*/
/*    bstring_to_keyword ...                                           */
/*    char * --> obj_t                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bstring_to_keyword( obj_t name ) {
   long hash_number;
   obj_t bucket;
   char *cname = BSTRING_TO_STRING( name );

   hash_number = get_hash_power_number( cname, KEYWORD_HASH_TABLE_SIZE_SHIFT );

   bgl_mutex_lock( keyword_mutex );
   bucket = VECTOR_REF( c_keytab, hash_number );
   
   if( NULLP( bucket ) ) {
      obj_t keyword = make_keyword( name );
      obj_t pair = MAKE_PAIR( keyword, BNIL );
      
      VECTOR_SET( c_keytab, hash_number, pair );
      
      bgl_mutex_unlock( keyword_mutex );
      return keyword;
   } else {
      obj_t run = bucket, back = bucket;
      
      while( !NULLP( run ) &&
	     strcmp( (const char *)BSTRING_TO_STRING( KEYWORD(CAR( run )).string ),
		     (const char *)cname ) )
         back = run, run = CDR( run );
      
      if( !NULLP( run ) ) {
	 bgl_mutex_unlock( keyword_mutex );
         return CAR( run );
      }
      else {
         obj_t keyword = make_keyword( name );
	 obj_t pair    = MAKE_PAIR( keyword, BNIL );
	 
         SET_CDR( back, pair );

	 bgl_mutex_unlock( keyword_mutex );
         return keyword;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    string_to_keyword ...                                            */
/*    char * --> obj_t                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
string_to_keyword( char *cname ) {
   long hash_number;
   obj_t bucket;

   hash_number = get_hash_power_number( cname, KEYWORD_HASH_TABLE_SIZE_SHIFT );
   
   bgl_mutex_lock( keyword_mutex );
   bucket = VECTOR_REF( c_keytab, hash_number );
   
   if( NULLP( bucket ) ) {
      obj_t keyword = make_keyword( string_to_bstring( cname ) );
      obj_t pair = MAKE_PAIR( keyword, BNIL );
      
      VECTOR_SET( c_keytab, hash_number, pair );
      
      bgl_mutex_unlock( keyword_mutex );
      return keyword;
   } else {
      obj_t run = bucket, back = bucket;
      
      while( !NULLP( run ) &&
	     strcmp( (const char *)BSTRING_TO_STRING( KEYWORD(CAR( run )).string ),
		     (const char *)cname ) )
         back = run, run = CDR( run );
      
      if( !NULLP( run ) ) {
	 bgl_mutex_unlock( keyword_mutex );
         return CAR( run );
      }
      else {
         obj_t keyword = make_keyword( string_to_bstring( cname ) );
	 obj_t pair = MAKE_PAIR( keyword, BNIL );
	 
         SET_CDR( back, pair );

	 bgl_mutex_unlock( keyword_mutex );
         return keyword;
      }
   }
}
