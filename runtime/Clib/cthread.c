/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cthread.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Oct  6 11:49:21 2004                          */
/*    Last change :  Tue Apr 17 08:01:05 2018 (serrano)                */
/*    Copyright   :  2004-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Thread tools (mutex, condition-variable, ...).                   */
/*    -------------------------------------------------------------    */
/*    This file does not presuppose any thread implementation.         */
/*    In particular, it does not presuppose that the underlying        */
/*    implementation is pthread nor pth. This implementation looks     */
/*    like an dummy implementation. It is just a kind of placeholder   */
/*    for Bigloo threads implementations.                              */
/*=====================================================================*/
#include <bigloo.h>
#include <signal.h>

/*---------------------------------------------------------------------*/
/*    Default functions ...                                            */
/*---------------------------------------------------------------------*/
static obj_t bgl_mutex_init_default( obj_t o );
static obj_t bgl_condvar_init_default( obj_t o );

static obj_t bgl_init_default( obj_t o ) { return o; }

static void bgl_act0_default( void ) { return; }
static int bgl_act_default( void *o ) { return 0; }
static bool_t bgl_act2_default( obj_t o1, obj_t o2 ) { return 1; }
static bool_t bgl_act2long_default( obj_t o1, long o2 ) { return 1; }
static bool_t bgl_act3long_default( obj_t o1, obj_t o2, long o3 ) { return 1; }

static void *bgl_gc_do_blocking_default( void (*fun)(), void *o2 ) {
   fun( o2 );

   return 0L;
}

static obj_t bgl_mutex_state_default( obj_t mutex ) { return BUNSPEC; }

static obj_t bgl_create_mutex_default( obj_t );

/*---------------------------------------------------------------------*/
/*    Single threaded dynamic environment ...                          */
/*---------------------------------------------------------------------*/
BGL_THREAD_DECL obj_t single_thread_denv = 0L;

static obj_t denv_get() { return 0; }

static obj_t nothread_backend;

DEFINE_STRING( nothread_backend, ___0, "nothread", sizeof( "nothread" ) );

/*---------------------------------------------------------------------*/
/*    Thread registers                                                 */
/*---------------------------------------------------------------------*/
#if HAVE_SIGPROCMASK
int (*bgl_sigprocmask)( int, const sigset_t *, sigset_t * ) = &sigprocmask;
#endif

static obj_t (*bgl_mutex_init)( obj_t ) = &bgl_mutex_init_default;
static obj_t (*bgl_spinlock_init)( obj_t ) = &bgl_mutex_init_default;
static obj_t (*bgl_condvar_init)( obj_t ) = &bgl_condvar_init_default;

BGL_RUNTIME_DEF obj_t (*bgl_create_mutex)( obj_t ) = &bgl_create_mutex_default;
BGL_RUNTIME_DEF obj_t (*bgl_create_spinlock)( obj_t ) = &bgl_create_mutex_default;

BGL_RUNTIME_DEF void (*bgl_gc_start_blocking)( void ) = &bgl_act0_default;
BGL_RUNTIME_DEF void (*bgl_gc_stop_blocking)( void ) = &bgl_act0_default;

BGL_RUNTIME_DEF void *(*bgl_gc_do_blocking)( void (*fun)(), void * ) = &bgl_gc_do_blocking_default;

BGL_RUNTIME_DEF obj_t (*bgl_multithread_dynamic_denv)() = &denv_get;

/*---------------------------------------------------------------------*/
/*    Register functions ...                                           */
/*---------------------------------------------------------------------*/
#define REGISTER_FUNCTION( id, res, proto ) \
  BGL_RUNTIME_DEF void id##_register( res (*f)proto ) { id = f; }

#if HAVE_SIGPROCMASK							 
REGISTER_FUNCTION( bgl_sigprocmask, int, (int, const sigset_t *, sigset_t *) )
#endif

REGISTER_FUNCTION( bgl_create_mutex, obj_t, (obj_t) )
REGISTER_FUNCTION( bgl_create_spinlock, obj_t, (obj_t) )
REGISTER_FUNCTION( bgl_mutex_init, obj_t, (obj_t) )
REGISTER_FUNCTION( bgl_spinlock_init, obj_t, (obj_t) )
REGISTER_FUNCTION( bgl_condvar_init, obj_t, (obj_t) )
REGISTER_FUNCTION( bgl_multithread_dynamic_denv, obj_t, (void) );

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_create_mutex ...                                             */
/*---------------------------------------------------------------------*/
static obj_t
bgl_create_mutex_default( obj_t name ) {
   obj_t m = GC_MALLOC( BGL_MUTEX_SIZE );

   m->mutex.header = MAKE_HEADER( MUTEX_TYPE, BGL_MUTEX_SIZE );
   m->mutex.name = name;
   m->mutex.sysmutex = 0L;

   return BREF( m );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_mutex_init_default ...                                       */
/*---------------------------------------------------------------------*/
static obj_t
bgl_mutex_init_default( obj_t m ) {
   BGL_MUTEX( m ).syslock = &bgl_act_default;
   BGL_MUTEX( m ).systrylock = &bgl_act_default;
   BGL_MUTEX( m ).syslockprelock = &bgl_act2_default;
   BGL_MUTEX( m ).systimedlock = &bgl_act2long_default;
   BGL_MUTEX( m ).sysunlock = &bgl_act_default;
   BGL_MUTEX( m ).sysstate = &bgl_mutex_state_default;
   BGL_MUTEX( m ).backend = nothread_backend;

   return m;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_mutex ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_mutex( obj_t name ) {
   return bgl_mutex_init( bgl_create_mutex( name ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_spinlock ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_spinlock( obj_t name ) {
   return bgl_spinlock_init( bgl_create_mutex( name ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_nil_mutex ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_nil_mutex() {
   obj_t m = GC_MALLOC( BGL_MUTEX_SIZE );

   m->mutex.header = MAKE_HEADER( MUTEX_TYPE, BGL_MUTEX_SIZE );
   m->mutex.name = BUNSPEC;
   m->mutex.sysmutex = 0L;

   return bgl_mutex_init( BREF( m ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_condvar_create ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_create_condvar( obj_t name ) {
   obj_t cv = GC_MALLOC( BGL_CONDVAR_SIZE );

   cv->condvar.header = MAKE_HEADER( CONDVAR_TYPE, BGL_CONDVAR_SIZE );
   cv->condvar.name = name;
   cv->condvar.condvar = 0L;

   return BREF( cv );
}
   
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_condvar_init_default ...                                     */
/*---------------------------------------------------------------------*/
static obj_t
bgl_condvar_init_default( obj_t cv ) {
   BGL_CONDVAR( cv ).syswait = &bgl_act2_default;
   BGL_CONDVAR( cv ).systimedwait = &bgl_act3long_default;
   BGL_CONDVAR( cv ).syssignal = &bgl_act_default;
   BGL_CONDVAR( cv ).sysbroadcast = &bgl_act_default;

   return cv;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_condvar ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_condvar( obj_t name ) {
   return bgl_condvar_init( bgl_create_condvar( name ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_nil_condvar ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_nil_condvar() {
   obj_t m = GC_MALLOC( BGL_CONDVAR_SIZE );

   m->condvar.header = MAKE_HEADER( CONDVAR_TYPE, BGL_CONDVAR_SIZE );
   m->condvar.name = BUNSPEC;
   m->condvar.condvar = 0L;

   return bgl_condvar_init( BREF( m ) );
}

/*---------------------------------------------------------------------*/
/*    struct exitd *                                                   */
/*    make_exitd_bottom ...                                            */
/*---------------------------------------------------------------------*/
struct exitd *
make_exitd_bottom() {
   struct exitd *bottom =
      (struct exitd *)GC_MALLOC_UNCOLLECTABLE( sizeof( struct exitd ) );

   bottom->protect0 = BFALSE;
   bottom->protect1 = BFALSE;
   bottom->protectn = BNIL;

   return bottom;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_dynamic_env ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_dynamic_env() {
   int i;
   
   obj_t env = GC_MALLOC( sizeof( struct bgl_dynamic_env ) );

   env->dynamic_env.header = MAKE_HEADER( DYNAMIC_ENV_TYPE, 0 );
   
   env->dynamic_env.current_output_port = BUNSPEC;
   env->dynamic_env.current_error_port = BUNSPEC;
   env->dynamic_env.current_input_port = BUNSPEC;
   
   env->dynamic_env.current_display = BUNSPEC;

   env->dynamic_env.exit_value = BUNSPEC;
   env->dynamic_env.exitd_bottom = make_exitd_bottom();
   env->dynamic_env.exitd_top = env->dynamic_env.exitd_bottom;
   env->dynamic_env.exitd_val = MAKE_PAIR( BUNSPEC, BUNSPEC );
   SET_CAR( env->dynamic_env.exitd_val, MAKE_PAIR( BUNSPEC, BUNSPEC ) );
   env->dynamic_env.exitd_stamp = BINT( 0 );
   env->dynamic_env.befored_top = 0L;

   env->dynamic_env.mvalues_number = 1;
   for( i = 0; i < 16; i++ ) {
      env->dynamic_env.mvalues[ i ] = BUNSPEC;
   }

   env->dynamic_env.error_handler = BNIL;
   env->dynamic_env.uncaught_exception_handler = BNIL;
   env->dynamic_env.error_notifiers = BNIL;
   
   env->dynamic_env.interrupt_notifier = BNIL;
   
   env->dynamic_env.top_of_frame = 0L;
   env->dynamic_env.exit_traces = BNIL;
   env->dynamic_env.top.name = BUNSPEC;
   env->dynamic_env.top.location = BUNSPEC;
   env->dynamic_env.top.link = 0;

   env->dynamic_env.debug_alist = BNIL;

   env->dynamic_env.thread_backend = BUNSPEC;
   env->dynamic_env.current_thread = 0L;

   env->dynamic_env.lexical_stack = BNIL;
 
   env->dynamic_env.evstate = BUNSPEC;
   env->dynamic_env.module = BUNSPEC;
   env->dynamic_env.abase = BUNSPEC;

#if( BGL_SAW == 1 ) 
   env->dynamic_env.saw_sp = 0L;
#endif
   
   env->dynamic_env.parameters = BNIL;
   
   for( i = 0; i < 32; i++ ) {
      env->dynamic_env.sig_handlers[ i ] = BFALSE;
   }

   env->dynamic_env.user_data = BNIL;
   
   return BREF( env );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_dup_dynamic_env ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_dup_dynamic_env( obj_t o ) {
   int i;
   
   obj_t env = make_dynamic_env();
   struct bgl_dynamic_env *dst =
      (struct bgl_dynamic_env *)&(CREF( env )->dynamic_env);
   struct bgl_dynamic_env *src =
      (struct bgl_dynamic_env *)&(CREF( o )->dynamic_env);

   dst->current_output_port = src->current_output_port;
   dst->current_error_port = src->current_error_port;
   dst->current_input_port = src->current_input_port;

   dst->current_display = src->current_display;

   dst->interrupt_notifier = src->interrupt_notifier;
   
   dst->thread_backend = src->thread_backend;
   dst->current_thread = src->current_thread;
   
   dst->module = src->module;
   dst->abase = src->abase;

   for( i = 0; i < 32; i++ ) {
      dst->sig_handlers[ i ] = src->sig_handlers[ i ];
   }
   return env;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_current_dynamic_env ...                                      */
/*    -------------------------------------------------------------    */
/*    This function is used by callcc when the stack is being          */
/*    restored.                                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_current_dynamic_env() {
   return BGL_CURRENT_DYNAMIC_ENV();
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_dynamic_env ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_init_dynamic_env() {
   /* the variable bgl_dynamic_env_init role is twofold: first */
   /* it is used to avoid duplicate initialization, second it  */
   /* prevents the GC to collected the dynamic environment.    */
   static obj_t bgl_dynamic_env_init = 0;

   if( bgl_dynamic_env_init == 0 ) {
      bgl_dynamic_env_init = single_thread_denv = make_dynamic_env();
   }
}
