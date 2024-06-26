/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/gc_intf.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec 10 11:15:20 2013                          */
/*    Last change :  Mon Jul 10 15:53:22 2023 (serrano)                */
/*    Copyright   :  2013-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The GC interface                                                 */
/*=====================================================================*/
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>

/*---------------------------------------------------------------------*/
/*    bgl_gc_init                                                      */
/*---------------------------------------------------------------------*/
GC_API void bgl_gc_init() {
   static char init = 0;

   if (!init) {
 #ifdef GC_THREADS
      char *gcthreads = getenv("BIGLOOGCTHREADS");

      if (gcthreads) {
	 int gcn = atoi(gcthreads);
	 GC_set_markers_count(gcn);
      }
#endif      
      GC_INIT();
#ifdef GC_THREADS
      GC_start_mark_threads();
#endif      
      GC_set_finalize_on_demand(1);
      init = 1;
   }
}
