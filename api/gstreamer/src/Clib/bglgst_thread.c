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
/*    static GMutex *                                                  */
/*    bglgst_mutex_new ...                                             */
/*---------------------------------------------------------------------*/
static GMutex *
bglgst_mutex_new() {
   GMutex *result = (GMutex *)g_new( pthread_mutex_t, 1 );
   pthread_mutex_init( (pthread_mutex_t *)result, 0L );
   
   return result;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bglgst_mutex_lock ...                                            */
/*---------------------------------------------------------------------*/
static int
bglgst_mutex_lock( pthread_mutex_t *mutex ) {
   return pthread_mutex_lock( mutex );
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bglgst_mutex_trylock ...                                         */
/*---------------------------------------------------------------------*/
static gboolean
bglgst_mutex_trylock( GMutex *mutex ) {
  int result = pthread_mutex_trylock( (pthread_mutex_t *)mutex );

  if (result == EBUSY)
    return FALSE;

  return TRUE;
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bglgst_mutex_unlock ...                                          */
/*---------------------------------------------------------------------*/
static int
bglgst_mutex_unlock( pthread_mutex_t *mutex ) {
   return pthread_mutex_unlock( mutex );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglgst_mutex_free ...                                            */
/*---------------------------------------------------------------------*/
static void
bglgst_mutex_free( GMutex *mutex ) {
   pthread_mutex_destroy( (pthread_mutex_t *)mutex );
   g_free( mutex );
}


/*---------------------------------------------------------------------*/
/*    static GCond *                                                   */
/*    bglgst_cond_new ...                                              */
/*---------------------------------------------------------------------*/
static GCond *
bglgst_cond_new() {
   GCond *result = (GCond *)g_new( pthread_cond_t, 1 );
   pthread_cond_init( (pthread_cond_t *)result, 0L );
   
   return result;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bglgst_pthread_cond_signal ...                                   */
/*---------------------------------------------------------------------*/
static int
bglgst_pthread_cond_signal( pthread_cond_t *cond ) {
   return pthread_cond_signal( cond );
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bglgst_pthread_cond_broadcast ...                                */
/*---------------------------------------------------------------------*/
static int
bglgst_pthread_cond_broadcast( pthread_cond_t *cond ) {
   return pthread_cond_broadcast( cond );
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bglgst_pthread_cond_wait ...                                     */
/*---------------------------------------------------------------------*/
static int
bglgst_pthread_cond_wait( pthread_cond_t *cond, pthread_mutex_t *mutex ) {
   return pthread_cond_wait( cond, mutex );
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bglgst_cond_timed_wait ...                                       */
/*---------------------------------------------------------------------*/
static gboolean
bglgst_cond_timed_wait( GCond *cond,
			GMutex *entered_mutex,
			GTimeVal *abs_time ) {
#define G_NSEC_PER_SEC 1000000000
   int result;
   struct timespec end_time;
   gboolean timed_out;

   g_return_val_if_fail( cond != NULL, FALSE );
   g_return_val_if_fail( entered_mutex != NULL, FALSE );

   if( !abs_time ) {
      result = pthread_cond_wait( (pthread_cond_t *)cond,
                                  (pthread_mutex_t *)entered_mutex );
      timed_out = FALSE;
   } else {
      end_time.tv_sec = abs_time->tv_sec;
      end_time.tv_nsec = abs_time->tv_usec * (G_NSEC_PER_SEC / G_USEC_PER_SEC);

      g_return_val_if_fail( end_time.tv_nsec < G_NSEC_PER_SEC, TRUE );

      result = pthread_cond_timedwait( (pthread_cond_t *) cond,
				       (pthread_mutex_t *) entered_mutex,
				       &end_time );
      timed_out = (result == ETIMEDOUT);
   }
  
   return !timed_out;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglgst_cond_free ...                                             */
/*---------------------------------------------------------------------*/
static void
bglgst_cond_free( GCond *cond ) {
   pthread_cond_destroy( (pthread_cond_t *)cond );
   g_free( cond );
}

/*---------------------------------------------------------------------*/
/*    static GPrivate *                                                */
/*    bglgst_private_new ...                                           */
/*---------------------------------------------------------------------*/
static GPrivate *
bglgst_private_new( GDestroyNotify destructor ) {
  GPrivate *result = (GPrivate *)g_new( pthread_key_t, 1 );
  pthread_key_create( (pthread_key_t *) result, destructor );

  return result;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglgst_private_set ...                                           */
/*---------------------------------------------------------------------*/
static void
bglgst_private_set( GPrivate *private_key, gpointer value ) {
   if( !private_key )
      return;
   
   pthread_setspecific( *(pthread_key_t *)private_key, value );
}

/*---------------------------------------------------------------------*/
/*    static gpointer                                                  */
/*    bglgst_private_get ...                                           */
/*---------------------------------------------------------------------*/
static gpointer
bglgst_private_get( GPrivate *private_key ) {
   if( !private_key )
      return NULL;
   
   return (gpointer)pthread_getspecific( *(pthread_key_t *)private_key );
}

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
/*    static void                                                      */
/*    bglgst_thread_create ...                                         */
/*---------------------------------------------------------------------*/
static void
bglgst_thread_create( GThreadFunc thread_func,
		      gpointer arg,
		      gulong stack_size,
		      gboolean joinable,
		      gboolean bound,
		      GThreadPriority priority,
		      gpointer thread,
		      GError **error ) {
  pthread_attr_t attr;
  gint ret;
  void *self = bglpth_thread_new( MAKE_PAIR( (obj_t)thread_func, (obj_t)arg ) );

  bglpth_thread_env_create( self, BFALSE );

  g_return_if_fail( thread_func );
  g_return_if_fail( priority >= G_THREAD_PRIORITY_LOW );
  g_return_if_fail( priority <= G_THREAD_PRIORITY_URGENT );

  pthread_attr_init( &attr );

  if( stack_size ) {
     /* No error check here, because some systems can't do it and
      * we simply don't want threads to fail because of that. */
     pthread_attr_setstacksize( &attr, stack_size );
  }

  if( bound )
    /* No error check here, because some systems can't do it and we
     * simply don't want threads to fail because of that. */
    pthread_attr_setscope( &attr, PTHREAD_SCOPE_SYSTEM );

  pthread_attr_setdetachstate( &attr,
			       joinable ?
			       PTHREAD_CREATE_JOINABLE
			       : PTHREAD_CREATE_DETACHED );

  ret = pthread_create( thread, &attr, bglgst_thread_run, self );

  pthread_attr_destroy( &attr );

  if( ret == EAGAIN ) {
     g_set_error (error, G_THREAD_ERROR, G_THREAD_ERROR_AGAIN, 
		  "Error creating thread: %s", g_strerror( ret ) );
      return;
  }

  return;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglgst_thread_yield ...                                          */
/*---------------------------------------------------------------------*/
static void
bglgst_thread_yield() {
   sched_yield();
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglgst_thread_join ...                                           */
/*---------------------------------------------------------------------*/
static void
bglgst_thread_join( gpointer thread ) {
   gpointer ignore;
   pthread_join( *(pthread_t*)thread, &ignore );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglgst_thread_exit ...                                           */
/*---------------------------------------------------------------------*/
static void
bglgst_thread_exit() {
  pthread_exit( NULL );
}


/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglgst_thread_set_priority ...                                   */
/*---------------------------------------------------------------------*/
static void
bglgst_thread_set_priority( gpointer thread, GThreadPriority priority ) {
   return;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglgst_thread_self ...                                           */
/*---------------------------------------------------------------------*/
static void
bglgst_thread_self( gpointer thread ) {
   *(pthread_t *)thread = pthread_self();
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bglgst_thread_equal ...                                          */
/*---------------------------------------------------------------------*/
static gboolean
bglgst_thread_equal( gpointer thread1, gpointer thread2 ) {
  return (pthread_equal( *(pthread_t *)thread1, *(pthread_t *)thread2 ) != 0);
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglgst_thread_init ...                                           */
/*---------------------------------------------------------------------*/
void
bglgst_thread_init() {
   static GThreadFunctions g_thread_functions_for_bigloo = {
      &bglgst_mutex_new,
      (void (*)(GMutex *))&bglgst_mutex_lock,
      &bglgst_mutex_trylock,
      (void (*)(GMutex *))&bglgst_mutex_unlock,
      &bglgst_mutex_free,
      &bglgst_cond_new,
      (void (*)(GCond *))&bglgst_pthread_cond_signal,
      (void (*)(GCond *))&bglgst_pthread_cond_broadcast,
      (void (*)(GCond *, GMutex *))&bglgst_pthread_cond_wait,
      &bglgst_cond_timed_wait,
      &bglgst_cond_free,
      &bglgst_private_new,
      &bglgst_private_get,
      &bglgst_private_set,
      &bglgst_thread_create,
      &bglgst_thread_yield,
      &bglgst_thread_join,
      &bglgst_thread_exit,
      &bglgst_thread_set_priority,
      &bglgst_thread_self,
      &bglgst_thread_equal
   };

   g_thread_init( &g_thread_functions_for_bigloo );
}
