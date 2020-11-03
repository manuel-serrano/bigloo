/*=====================================================================*/
/*    .../project/bigloo/bigloo/api/pthread/src/Posix/bglsetup.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 21 15:57:25 2004                          */
/*    Last change :  Sun May 10 12:22:21 2020 (serrano)                */
/*    Copyright   :  2004-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The pthread setup                                                */
/*=====================================================================*/
#include <pthread.h>

#define GC_PRIVATE_H
#include <gc.h>
#include <bigloo.h>
#include <bglpthread.h>
#include <signal.h>

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern void bglpth_setup_mutex();
extern void bglpth_setup_condvar();
extern void bglpth_setup_thread();
extern void bglpth_setup_bmem();

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_signal ...                                          */
/*---------------------------------------------------------------------*/
static void
bglpth_setup_signal() {
#if HAVE_SIGPROCMASK
   extern void bgl_sigprocmask_register( int (*)(int, const sigset_t *, sigset_t *) );
   extern int GC_pthread_sigmask();
#if defined( GC_NO_PTHREAD_SIGMASK )
   bgl_sigprocmask_register( &pthread_sigmask );
#else   
   bgl_sigprocmask_register( &GC_pthread_sigmask );
#endif   
#endif
}

   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_gc ...                                              */
/*---------------------------------------------------------------------*/
static void
bglpth_setup_gc() {
#if( BGL_GC == BGL_BOEHM_GC && BGL_GC_HAVE_BLOCKING )
   extern void GC_start_blocking();
   extern void GC_end_blocking();

   bgl_gc_start_blocking = &GC_start_blocking;
   bgl_gc_stop_blocking = &GC_end_blocking;
#endif

#if( BGL_GC == BGL_BOEHM_GC && BGL_GC_HAVE_DO_BLOCKING )
#if( BGL_GC_VERSION == 731 \
   || BGL_GC_VERSION == 722 \
   || BGL_GC_VERSION == 710 \
   || BGL_GC_VERSION == 700 || BGL_GC_VERSION == 707 )
   extern void *GC_do_blocking();
#endif
   
   bgl_gc_do_blocking = (void *(*)())&GC_do_blocking;
#endif

   GC_init();
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup ...                                                 */
/*---------------------------------------------------------------------*/
BGL_EXPORTED_DEF
void
bglpth_setup( int argc, char *argv, char **env ) {
   static int pth_init = 0;

   if( !pth_init ) {
      pth_init = 1;
      
#ifdef PTW32_VERSION
      /* Pthreads-win32 initialization */
      pthread_win32_process_attach_np();
#endif

      bglpth_setup_signal();
      bglpth_setup_gc();
      bglpth_setup_bmem();
      bglpth_setup_mutex();
      bglpth_setup_condvar();
      bglpth_setup_thread();
   }
}
