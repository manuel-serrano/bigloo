/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/trace.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 14 15:15:48 2003                          */
/*    Last change :  Fri Jan 10 08:27:34 2020 (serrano)                */
/*    Copyright   :  2003-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Debug trace handling                                             */
/*=====================================================================*/
#include <bigloo.h>
#ifdef BMEMDEBUG
#include <pthread.h>
#endif

#include <esymbol.h>

/*---------------------------------------------------------------------*/
/*    Importations                                                     */
/*---------------------------------------------------------------------*/
extern int bmem_debug;
extern int bmem_thread;
extern void *(*____bglthread_id_get )();

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    bgl_debug_trace ...                                              */
/*---------------------------------------------------------------------*/
void *
bgl_debug_trace() {
   return  BGL_ENV_GET_TOP_OF_FRAME( BGL_CURRENT_DYNAMIC_ENV() );
}

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    bgl_debug_trace_top ...                                          */
/*---------------------------------------------------------------------*/
void *
bgl_debug_trace_top( int offset ) {
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   
   if( !env ) {
      goto unknown;
   } else {
      struct bgl_dframe *top = BGL_ENV_GET_TOP_OF_FRAME( env );

loop:
      if( !top ) goto unknown;
      if( !SYMBOLP( top->name ) ) goto unknown;

      if( ((esymbol_t *)(CSYMBOL( top->name )))->class_alloc >= 0 ) {
	 return top->name;
      }
      
      if( offset > 0 ) {
	 top = top->link;
	 offset--;
	 goto loop;
      }
      
      return top->name;
   }

 unknown:
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_debug_trace_symbol_name ...                                  */
/*---------------------------------------------------------------------*/
char *
bgl_debug_trace_symbol_name( obj_t sym ) {
   if( SYMBOLP( sym ) ) {
      return BSTRING_TO_STRING( SYMBOL_TO_STRING( sym ) );
   } else {
      return "unknown";
   }
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_debug_trace_symbol_name ...                                  */
/*---------------------------------------------------------------------*/
char *
bgl_debug_trace_symbol_name_json( obj_t sym ) {
   if( SYMBOLP( sym ) ) {
      obj_t s = SYMBOL_TO_STRING( sym );
      int len = STRING_LENGTH( s );
      char *name = BSTRING_TO_STRING( s );
      char *beg = strstr( name, "<@" );
      char *buf;

      fprintf( stderr, "name=[%s] -> [%s]\n", name, beg );
	    
      if( beg ) {
	 char *end = strstr( beg, "@>" );

	 if( end ) {
	    char *res = malloc( end - beg - 1 );
	    char *sep;
	    strncpy( res, beg + 2, end - beg );
	    res[ end - beg - 2 ] = 0;

	    sep = strchr( res, ':' );

	    if( sep ) {
	       char *buf = malloc( strlen( name ) + 27 );
	       *sep = 0;
	       
	    fprintf( stderr, "name=[%s] => [%s:%s]\n", name, sep + 1, res );
	       sprintf( buf, "{ \"point\": %s, \"file\": \"%s\" }",
			sep + 1, res );
	       return buf;
	    }
	 }
      }

      buf = malloc( len + 2 );
      buf[ 0 ] = '"';
      strcpy( buf + 1, name );
      buf[ len ] = '"';
      buf[ len + 1 ] = 0;

      fprintf( stderr, "name=[%s] ~> [%s]\n", name, buf );
      return buf;
   } else {
      return "\"unknown\"";
   }
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_debug_trace_top_name ...                                     */
/*---------------------------------------------------------------------*/
char *
bgl_debug_trace_top_name( int offset ) {
   return bgl_debug_trace_symbol_name( bgl_debug_trace_top( offset ) );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    for_each_trace ...                                               */
/*---------------------------------------------------------------------*/
void
for_each_trace( void (*fun)( obj_t, void * ), int start, int stop, void *a ) {
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   
   if( env ) {
      struct bgl_dframe *frame = BGL_ENV_GET_TOP_OF_FRAME( env );
      int depth = 0;

      while( frame && (depth < start) ) {
	 depth++;
	 frame = frame->link;
      }
	 
      while( frame && (depth < stop) ) {
	 depth++;
	 fun( frame->name, a );
	 frame = frame->link;
      }
   }
}
