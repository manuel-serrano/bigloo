/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/gchook.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:44:45 2003                          */
/*    Last change :  Fri May  1 16:06:28 2020 (serrano)                */
/*    Copyright   :  2003-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Hook to be ran after each gc                                     */
/*=====================================================================*/
#include <stdlib.h>
#include <bigloo.h>
#include <bmem.h>

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
static pa_pair_t *gcs_info = 0;
static unsigned long gc_alloc_size = 0;
unsigned long gc_number = 0;
extern int bmem_verbose;

/*---------------------------------------------------------------------*/
/*    static gc_info_t *                                               */
/*    make_gc_info ...                                                 */
/*---------------------------------------------------------------------*/
static gc_info_t *
make_gc_info( long num, long asize, long hsize, long lsize, void *lastfun ) {
   gc_info_t *info = (gc_info_t *)malloc( sizeof( gc_info_t ) );
   
   info->number = num;
   info->alloc_size = asize;
   info->heap_size = hsize;
   info->live_size = lsize;
   info->lastfun = bgl_debug_trace_top( 0 );
   info->time = ____bgl_current_nanoseconds();

   return info;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    GC_collect_hook ...                                              */
/*---------------------------------------------------------------------*/
void
GC_collect_hook( int heapsz, long livesz ) {
   void *lf = bgl_debug_trace_top( 0 );
   gc_info_t *info = make_gc_info( gc_number, gc_alloc_size, heapsz, livesz, lf );

   gc_number++;

   if( bmem_verbose >= 1 ) {
      if( heapsz > (1024 * 1024) ) {
	 fprintf( stderr, "gc %3lu: alloc size=%.2fMB, heap size=%.2fMB, live size=%.2fMB\n",
		  gc_number,
		  ((double)gc_alloc_size / (1024. * 1024.)),
		  ((double)heapsz / (1024. * 1024.)),
		  ((double)livesz / (1024. * 1024.)) );
      } else {
	 fprintf( stderr, "gc %3lu: alloc size=%luKB, heap size=%dKB, live size=%ldKB, fun=%s\n",
		  gc_number,
		  gc_alloc_size / 1024,
		  heapsz / 1024,
		  livesz / 1024,
		  bgl_debug_trace_top_name( 0 ) );
      }
   }
      
   gc_alloc_size = 0;
   
   gcs_info = pa_cons( info, gcs_info );

}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    gc_alloc_size_add ...                                            */
/*---------------------------------------------------------------------*/
void
gc_alloc_size_add( int size ) {
   if( bmem_thread ) pthread_mutex_lock( &bmem_mutex );
   gc_alloc_size += size;
   if( bmem_thread ) pthread_mutex_unlock( &bmem_mutex );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    GC_dump_gc_sexp ...                                              */
/*---------------------------------------------------------------------*/
static void
GC_dump_gc_sexp( gc_info_t *i, FILE *f ) {
   fprintf( f, "    (%lu #l%lu #l%lu #l%lu \"%s\" #l%lld)\n",
	    i->number,
	    BMEMSIZE( i->alloc_size ),
	    i->heap_size,
	    BMEMSIZE( i->live_size ),
	    bgl_debug_trace_symbol_name( i->lastfun ),
	    i->time );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    GC_dump_gc_json ...                                              */
/*---------------------------------------------------------------------*/
static void
GC_dump_gc_json( gc_info_t *i, FILE *f ) {
   fprintf( f, "    { \"number\": %lu, \"alloc\": %lu, \"heap\": %lu, \"live\": %lu, \"lastfun\": %s, \"time\": %lld }",
	    i->number,
	    BMEMSIZE( i->alloc_size ),
	    i->heap_size,
	    BMEMSIZE( i->live_size ),
	    bgl_debug_trace_symbol_name_json( i->lastfun ),
	    i->time );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    GC_dump_statistics ...                                           */
/*---------------------------------------------------------------------*/
void
GC_dump_statistics( FILE *f ) {
   fprintf( f, "  (gc\n" );
   for_each( (void (*)(void *, void *))GC_dump_gc_sexp,
	     pa_reverse( gcs_info ),
	     (void *)f );
   fprintf( f, "    )\n" );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    GC_dump_statistics_json ...                                      */
/*---------------------------------------------------------------------*/
void
GC_dump_statistics_json( FILE *f ) {
   fprintf( f, "   \"gc\": " );
   for_each_json( (void (*)(void *, void *))GC_dump_gc_json,
		  pa_reverse( gcs_info ),
		  (void *)f );
   fprintf( f, "\n" );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    GC_reset_statistics ...                                          */
/*---------------------------------------------------------------------*/
void
GC_reset_statistics() {
   gcs_info = 0;
   gc_alloc_size = 0;
   gc_number = 0;

   ____GC_reset_allocated_bytes();
}

/*---------------------------------------------------------------------*/
/*    long long                                                        */
/*    GC_alloc_total ...                                               */
/*---------------------------------------------------------------------*/
long long
GC_alloc_total() {
   long long sz = 0;
   pa_pair_t *lst = gcs_info;
   
   while( PA_PAIRP( lst ) ) {
      gc_info_t *i = PA_CAR( lst );
      sz += (long long)BMEMSIZE( i->alloc_size );
      lst = PA_CDR( lst );
   }

   return sz;
}
