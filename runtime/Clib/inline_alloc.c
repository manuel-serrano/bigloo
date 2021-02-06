/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/inline_alloc.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Sep 21 15:33:10 1994                          */
/*    Last change :  Sun Oct 29 09:01:10 2017 (serrano)                */
/*    -------------------------------------------------------------    */
/*    On fait des fonctions d'allocations specialisees pour les cons   */
/*    et les flottants.                                                */
/*=====================================================================*/
#ifndef GC_PRIVATE_H
#  include <private/gc_priv.h>
#endif
#undef abs

#include <bigloo.h>
#include <bigloo_static.h>

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    gcnum ...                                                        */
/*---------------------------------------------------------------------*/
static long gcnum = 0;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    gcollect_verbose ...                                             */
/*---------------------------------------------------------------------*/
static void
gcollect_verbose( unsigned long heapsz, unsigned long use ) {
   fprintf( stderr, "gc %2ld: %lu %lu\n", gcnum++, heapsz, use );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_gc_verbose_set ...                                           */
/*---------------------------------------------------------------------*/
GC_API void
bgl_gc_verbose_set( bool_t verbose ) {
#if( (BGL_GC == BGL_BOEHM_GC) && BGL_GC_CUSTOM )
   extern void GC_add_gc_hook( void (*f)() );
   
   if( verbose ) {
      fprintf( stderr, "bgl_gc_verbose on...\n" );
      gcnum = 1;
      GC_add_gc_hook( &gcollect_verbose );
   } else {
      fprintf( stderr, "bgl_gc_verbose off...\n" );
      GC_add_gc_hook( 0 );
   }
#endif   
}

/*---------------------------------------------------------------------*/
/*    configured allocators                                            */
/*---------------------------------------------------------------------*/
#if( BGL_GC_BUMP_ALLOC )
#  include "inline_alloc_bump.h"
#endif

#if( (BGL_GC == BGL_BOEHM_GC) && BGL_GC_CUSTOM && !defined( BGL_GC_THREADS ))
#  include "inline_alloc.h"
#else
#  include "inline_alloc_thread.h"
#endif


