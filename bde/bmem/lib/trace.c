/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bde/bmem/lib/trace.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 14 15:15:48 2003                          */
/*    Last change :  Mon Jan 21 11:38:00 2008 (serrano)                */
/*    Copyright   :  2003-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Debug trace handling                                             */
/*=====================================================================*/
#include <bigloo.h>

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
bgl_debug_trace_top() {
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   
#if !BMEMDEBUG
   if( !env ) {
      goto unknown;
   } else {
      struct bgl_dframe *top = BGL_ENV_GET_TOP_OF_FRAME( env );

      if( !top ) goto unknown;
      if( !SYMBOLP( top->symbol ) ) goto unknown;
      
      return top->symbol;
   }

 unknown:
   {
      /* if we see no trace in a stack (or no stack at all) we */
      /* check we are running a asynchronous fair-thread.      */
      void *th = bmem_thread ? ____bglthread_id_get() : 0;
      
      if( SYMBOLP( th ) )
	 return th;
      else
	 return BUNSPEC;
   }
}
#else
   if( !env ) goto unknown;
   else {
      struct bgl_dframe *top = BGL_ENV_GET_TOP_OF_FRAME( env );

      if( bmem_debug >= 20 ) {
	 fprintf( stderr, "                env=%p top=%p\n", env, top );
      }
      
      if( !top ) goto unknown;

      if( bmem_debug >= 20 ) {
	 fprintf( stderr, "                  top->symbol=%p\n", top->symbol );
	 if( top->symbol ) {
	    if( STRINGP( top->symbol ) ) {
	       fprintf( stderr, "                  top->symbol=STRING %p\n", top->symbol );
	    } else {
	       if( KEYWORDP( top->symbol ) ) {
		  fprintf( stderr, "                  top->symbol=KEYWORD %p\n", top->symbol );
	       } else {
		  if( !POINTERP( top->symbol ) ) {
		     fprintf( stderr, "                  top->symbol=pas pointer %d\n", top->symbol );
		  } else {
		     fprintf( stderr, "                  top->symbol=pointer %p\n", TYPE( top->symbol  ) );
		  }
	       }
	    }
	 }
      }

      if( !SYMBOLP( top->symbol ) ) goto unknown;
      
      return top->symbol;
   }

 unknown:
   {
      /* if we see no trace in a stack (or no stack at all) we */
      /* check we are running a asynchronous fair-thread.      */
      void *th = bmem_thread == 1 ? ____bglthread_id_get() : 0;

      if( bmem_debug >= 20 ) {
	 fprintf( stderr, "                unknown\n" );
	 fprintf( stderr, "                  id=%p pthread_self=%p\n", th, pthread_self() );

	 if( SYMBOLP( th ) )
	    fprintf( stderr, "                  id->sym=%s\n", BSTRING_TO_STRING( SYMBOL_TO_STRING( th ) ) );
      }
      
      if( SYMBOLP( th ) )
	 return th;
      else {
	 if( bmem_debug >= 20 ) {
	    fprintf( stderr, "                  unknown (th=%p)\n", th );
	 }
	 return BUNSPEC;
      }
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_debug_trace_top_name ...                                     */
/*---------------------------------------------------------------------*/
char *
bgl_debug_trace_top_name() {
   obj_t sym = bgl_debug_trace_top();

   if( SYMBOLP( sym ) ) {
      return BSTRING_TO_STRING( SYMBOL_TO_STRING( sym ) );
   } else {
      return "unknown";
   }
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
	 fun( frame->symbol, a );
	 frame = frame->link;
      }
   }
}
