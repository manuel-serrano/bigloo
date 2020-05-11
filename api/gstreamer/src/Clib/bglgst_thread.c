/*=====================================================================*/
/*    .../project/bigloo/api/gstreamer/src/Clib/bglgst_thread.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jul 25 15:39:16 2008                          */
/*    Last change :  Wed Feb 13 15:05:03 2013 (serrano)                */
/*    Copyright   :  2008-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo implementation for Glib threads                           */
/*=====================================================================*/
#include <sys/time.h>
#if( BGL_GC == BGL_BOEHM_GC )
#  include <glib.h>
#endif
#include <gst/gst.h>
#include <string.h>
#include <unistd.h>
#include "bglgst_config.h"
#include <pthread.h>
#if( defined( BGL_GSTREAMER_USE_THREADS ) )
#  define GC_PRIVATE_H
#  include <gc.h>
#  include <bigloo.h>
#endif

/*---------------------------------------------------------------------*/
/*    imports                                                          */
/*---------------------------------------------------------------------*/
extern void bglpth_thread_cleanup( void *arg );
extern void bglpth_thread_env_create( void *thread, obj_t bglthread );
extern void bglpth_thread_init( void *self, char *stack_bottom );
extern obj_t bglpth_thread_thunk();
void *bglpth_thread_new();

/*---------------------------------------------------------------------*/
/*    GRealThread                                                      */
/*---------------------------------------------------------------------*/
typedef struct _GRealThread GRealThread;
struct  _GRealThread
{
  GThread thread;

  gint ref_count;
  gboolean ours;
  gchar *name;
  gpointer retval;
};

/*---------------------------------------------------------------------*/
/*    GThreadPosix                                                     */
/*---------------------------------------------------------------------*/
typedef struct
{
  GRealThread thread;

  pthread_t system_thread;
  gboolean  joined;
  GMutex    lock;
} GThreadPosix;

/*---------------------------------------------------------------------*/
/*    static void *                                                    */
/*    bglgst_thread_run ...                                            */
/*---------------------------------------------------------------------*/
static void *
bglgst_thread_run( void *self ) {
   obj_t thunk = bglpth_thread_thunk( self );
   GThreadFunc thread_func = (GThreadFunc)CAR( thunk );
   gpointer arg = CDR( thunk );

   /* The environment is stored in a specific variable for dynamic   */
   /* access but it is pointed to by the thread structure for the GC */
   bglpth_thread_init( self, (char *)&self );

   /* Start the gstreamer thread */
   thread_func( arg );

   /* Cleanup the Bigloo thread */
   bglpth_thread_cleanup( self );

   return NULL;
}
		      
/*---------------------------------------------------------------------*/
/*    GRealThread *                                                    */
/*    g_system_thread_new ...                                          */
/*---------------------------------------------------------------------*/
GRealThread *
g_system_thread_new (GThreadFunc   proxy,
		     gulong        stack_size,
		     const char   *name,
		     GThreadFunc   func,
		     gpointer      data,
		     GError      **error)
{
   void *self;
   GThreadPosix *thread;
   GRealThread *base_thread;
   pthread_attr_t attr;
   gint ret;

   thread = g_slice_new0( GThreadPosix );
   base_thread = (GRealThread*)thread;
   base_thread->ref_count = 2;
   base_thread->ours = TRUE;
   base_thread->thread.joinable = TRUE;
   base_thread->thread.func = func;
   base_thread->thread.data = data;
   base_thread->name = g_strdup( name );

   pthread_attr_init( &attr );

#ifdef HAVE_PTHREAD_ATTR_SETSTACKSIZE
   if ( stack_size ) {
#ifdef _SC_THREAD_STACK_MIN
      long min_stack_size = sysconf( _SC_THREAD_STACK_MIN );
      if ( min_stack_size >= 0 )
         stack_size = MAX( (gulong) min_stack_size, stack_size );
#endif /* _SC_THREAD_STACK_MIN */
      /* No error check here, because some systems can't do it and
       * we simply don't want threads to fail because of that. */
      pthread_attr_setstacksize( &attr, stack_size );
   }
#endif /* HAVE_PTHREAD_ATTR_SETSTACKSIZE */

   self = bglpth_thread_new( MAKE_PAIR( (obj_t)proxy, (obj_t)thread ) );
   bglpth_thread_env_create( self, BFALSE );

   ret = pthread_create( &thread->system_thread, &attr, bglgst_thread_run, self );

   pthread_attr_destroy( &attr );

   if ( ret == EAGAIN ) {
      g_set_error( error, G_THREAD_ERROR, G_THREAD_ERROR_AGAIN,
		   "Error creating thread: %s", g_strerror( ret ));
      g_slice_free( GThreadPosix, thread );
      return NULL;
   }

   g_mutex_init( &thread->lock );

   return (GRealThread *)thread;
}
