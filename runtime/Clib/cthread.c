/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cthread.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Oct  6 11:49:21 2004                          */
/*    Last change :  Fri Dec 31 12:13:27 2010 (serrano)                */
/*    Copyright   :  2004-10 Manuel Serrano                            */
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

/*---------------------------------------------------------------------*/
/*    Default functions ...                                            */
/*---------------------------------------------------------------------*/
static obj_t bgl_init_default( obj_t o ) { return o; }

static void bgl_act0_default( void ) { return; }
static bool_t bgl_act_default( obj_t o ) { return 1; }
static bool_t bgl_act2_default( obj_t o1, obj_t o2 ) { return 1; }
static bool_t bgl_act2long_default( obj_t o1, long o2 ) { return 1; }
static bool_t bgl_act3long_default( obj_t o1, obj_t o2, long o3 ) { return 1; }

static void *bgl_gc_do_blocking_default( void (*fun)(), void *o2 ) {
   fun( o2 );
}

static obj_t bgl_mutex_state_default( obj_t mutex ) { return BUNSPEC; }

/*---------------------------------------------------------------------*/
/*    Single threaded dynamic environment ...                          */
/*---------------------------------------------------------------------*/
BGL_THREAD_DECL obj_t single_thread_denv = 0L;

static obj_t denv_get() { return 0; }

/*---------------------------------------------------------------------*/
/*    Thread registers                                                 */
/*---------------------------------------------------------------------*/
static obj_t (*bgl_mutex_init)( obj_t ) = &bgl_init_default;
static obj_t (*bgl_condvar_init)( obj_t ) = &bgl_init_default;

BGL_RUNTIME_DEF void (*bgl_gc_start_blocking)( void ) = &bgl_act0_default;
BGL_RUNTIME_DEF void (*bgl_gc_stop_blocking)( void ) = &bgl_act0_default;

BGL_RUNTIME_DEF void *(*bgl_gc_do_blocking)( void (*fun)(), void * ) = &bgl_gc_do_blocking_default;

BGL_RUNTIME_DEF bool_t (*bgl_mutex_lock)( obj_t ) = &bgl_act_default;
BGL_RUNTIME_DEF bool_t (*bgl_mutex_timed_lock)( obj_t, long ) = &bgl_act2long_default;
BGL_RUNTIME_DEF bool_t (*bgl_mutex_unlock)( obj_t ) = &bgl_act_default;
BGL_RUNTIME_DEF obj_t (*bgl_mutex_state)( obj_t ) = &bgl_mutex_state_default;

BGL_RUNTIME_DEF bool_t (*bgl_condvar_wait)( obj_t, obj_t ) = &bgl_act2_default;
BGL_RUNTIME_DEF bool_t (*bgl_condvar_timed_wait)( obj_t, obj_t, long ) = &bgl_act3long_default;
BGL_RUNTIME_DEF bool_t (*bgl_condvar_signal)( obj_t ) = &bgl_act_default;
BGL_RUNTIME_DEF bool_t (*bgl_condvar_broadcast)( obj_t ) = &bgl_act_default;

BGL_RUNTIME_DEF obj_t (*bgl_multithread_dynamic_denv)() = &denv_get;

/*---------------------------------------------------------------------*/
/*    Register functions ...                                           */
/*---------------------------------------------------------------------*/
#define REGISTER_FUNCTION( id, res, proto ) \
  BGL_RUNTIME_DEF void id##_register( res (*f)proto ) { id = f; }

REGISTER_FUNCTION( bgl_mutex_init, obj_t, (obj_t) )
REGISTER_FUNCTION( bgl_mutex_lock, bool_t, (obj_t) )
REGISTER_FUNCTION( bgl_mutex_timed_lock, bool_t, (obj_t, long) )
REGISTER_FUNCTION( bgl_mutex_unlock, bool_t, (obj_t) )
REGISTER_FUNCTION( bgl_mutex_state, obj_t, (obj_t) )

REGISTER_FUNCTION( bgl_condvar_init, obj_t, (obj_t) )
REGISTER_FUNCTION( bgl_condvar_wait, bool_t, (obj_t, obj_t) )
REGISTER_FUNCTION( bgl_condvar_timed_wait, bool_t, (obj_t, obj_t, long) )
REGISTER_FUNCTION( bgl_condvar_signal, bool_t, (obj_t) )
REGISTER_FUNCTION( bgl_condvar_broadcast, bool_t, (obj_t) )

REGISTER_FUNCTION( bgl_multithread_dynamic_denv, obj_t, (void) );
		   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_mutex ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_mutex( obj_t name ) {
   obj_t m = GC_MALLOC( BGL_MUTEX_SIZE );

   m->mutex_t.header = MAKE_HEADER( MUTEX_TYPE, BGL_MUTEX_SIZE );
   m->mutex_t.name = name;
   m->mutex_t.mutex = 0L;

   bgl_mutex_init( m );

   return BREF( m );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_nil_mutex ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_nil_mutex() {
   obj_t m = GC_MALLOC( BGL_MUTEX_SIZE );

   m->mutex_t.header = MAKE_HEADER( MUTEX_TYPE, BGL_MUTEX_SIZE );
   m->mutex_t.name = BUNSPEC;
   m->mutex_t.mutex = 0L;

   return BREF( m );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_condvar ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_condvar( obj_t name ) {
   obj_t m = GC_MALLOC( BGL_CONDVAR_SIZE );

   m->condvar_t.header = MAKE_HEADER( CONDVAR_TYPE, BGL_CONDVAR_SIZE );
   m->condvar_t.name = name;
   m->condvar_t.condvar = 0L;

   bgl_condvar_init( m );

   return BREF( m );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_nil_condvar ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_nil_condvar() {
   obj_t m = GC_MALLOC( BGL_CONDVAR_SIZE );

   m->condvar_t.header = MAKE_HEADER( CONDVAR_TYPE, BGL_CONDVAR_SIZE );
   m->condvar_t.name = BUNSPEC;
   m->condvar_t.condvar = 0L;

   return BREF( m );
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

   env->dynamic_env_t.header = MAKE_HEADER( DYNAMIC_ENV_TYPE, 0 );
   
   env->dynamic_env_t.current_output_port = BUNSPEC;
   env->dynamic_env_t.current_error_port = BUNSPEC;
   env->dynamic_env_t.current_input_port = BUNSPEC;
   
   env->dynamic_env_t.current_display = BUNSPEC;

   env->dynamic_env_t.exit_value = BUNSPEC;
   env->dynamic_env_t.exitd_top = 0L;
   env->dynamic_env_t.exitd_val = MAKE_PAIR( BUNSPEC, BUNSPEC );
   SET_CAR( env->dynamic_env_t.exitd_val, MAKE_PAIR( BUNSPEC, BUNSPEC ) );
   env->dynamic_env_t.exitd_stamp = BINT( 0 );
   env->dynamic_env_t.befored_top = 0L;

   env->dynamic_env_t.mvalues_number = 1;
   for( i = 0; i < 16; i++ ) {
      env->dynamic_env_t.mvalues[ i ] = BUNSPEC;
   }

   env->dynamic_env_t.error_handler = BNIL;
   env->dynamic_env_t.uncaught_exception_handler = BNIL;
   env->dynamic_env_t.error_notifiers = BNIL;
   
   env->dynamic_env_t.interrupt_notifier = BNIL;
   
   env->dynamic_env_t.top_of_frame = 0L;
   env->dynamic_env_t.exit_traces = BNIL;
   env->dynamic_env_t.top.name = BUNSPEC;
   env->dynamic_env_t.top.location = BUNSPEC;
   env->dynamic_env_t.top.link = 0;

   env->dynamic_env_t.debug_alist = BNIL;

   env->dynamic_env_t.thread_backend = BUNSPEC;
   env->dynamic_env_t.current_thread = 0L;

   env->dynamic_env_t.lexical_stack = BNIL;
 
   env->dynamic_env_t.bytecode = BUNSPEC;
   env->dynamic_env_t.module = BUNSPEC;
   env->dynamic_env_t.abase = BUNSPEC;

   env->dynamic_env_t.parameters = BNIL;
   
   for( i = 0; i < 32; i++ ) {
      env->dynamic_env_t.sig_handlers[ i ] = BFALSE;
   }

   env->dynamic_env_t.user_data = BNIL;
   
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
   struct bgl_dynamic_env *dst = (struct bgl_dynamic_env *)CREF( env );
   struct bgl_dynamic_env *src = (struct bgl_dynamic_env *)CREF( o );

   env->dynamic_env_t.current_output_port = o->dynamic_env_t.current_output_port;
   env->dynamic_env_t.current_error_port = o->dynamic_env_t.current_error_port;
   env->dynamic_env_t.current_input_port = o->dynamic_env_t.current_input_port;

   env->dynamic_env_t.current_display = o->dynamic_env_t.current_display;

   env->dynamic_env_t.interrupt_notifier = o->dynamic_env_t.interrupt_notifier;
   
   env->dynamic_env_t.thread_backend = o->dynamic_env_t.thread_backend;
   env->dynamic_env_t.current_thread = o->dynamic_env_t.current_thread;
   
   env->dynamic_env_t.module = o->dynamic_env_t.module;
   env->dynamic_env_t.abase = o->dynamic_env_t.abase;
   
   for( i = 0; i < 32; i++ ) {
      env->dynamic_env_t.sig_handlers[ i ] = o->dynamic_env_t.sig_handlers[ i ];
   }

   return env;
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
