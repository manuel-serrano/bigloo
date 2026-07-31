/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem-ng/lib/trace.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 14 15:15:48 2003                          */
/*    Last change :  Wed Oct  6 14:25:05 2021 (serrano)                */
/*    Copyright   :  2003-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Debug trace handling                                             */
/*=====================================================================*/
#include <bigloo.h>
#ifdef BMEMDEBUG
#include <pthread.h>
#endif

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
