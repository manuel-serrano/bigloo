/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/callcc.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Sep 14 09:03:27 1992                          */
/*    Last change :  Sat Dec  7 18:55:45 2019 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Implementing call/cc                                             */
/*=====================================================================*/
#include <string.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    SPARC settings ...                                               */
/*---------------------------------------------------------------------*/
#if( defined( sparc ) )
extern int flush_regs_in_stack();
#else
#define flush_regs_in_stack()
#endif

/*---------------------------------------------------------------------*/
/*    Common importations ...                                          */
/*---------------------------------------------------------------------*/
extern long glob_dummy;

extern obj_t make_fx_procedure( obj_t (*)(), int, int );
extern obj_t c_constant_string_to_string( char * );

static obj_t callcc_restore_stack( obj_t, obj_t, char ** );
extern obj_t unwind_stack_until( obj_t, obj_t, obj_t, obj_t );
extern bool_t unwind_stack_value_p( obj_t );
extern void *bgl_get_top_of_stack();
extern obj_t  bgl_current_dynamic_env();

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    bgl_callcc_get_top_of_stack ...                                  */
/*---------------------------------------------------------------------*/
BGL_NOINLINE
void *
bgl_callcc_get_top_of_stack( void *dummy ) {
   return dummy;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    wind_stack ...                                                   */
/*    -------------------------------------------------------------    */
/*    We `wind' a stack. That is, we succesively invoke all the        */
/*    before thunk that have been pushed by `dynamic-wind'. This       */
/*    operation is the contrary of a `unwind-protect' that's why       */
/*    this function is called `wind-stack'.                            */
/*    -------------------------------------------------------------    */
/*    The global variable befored_top that has been restored when      */
/*    the old stack has been re-installed is the head of the list.     */
/*    Unfortunately we have to wind the stack from bottom to top.      */
/*    That is we have to inverse the list. We do this with a simple    */
/*    recursion.                                                       */
/*---------------------------------------------------------------------*/
void
wind_stack( struct befored *bfl ) {
   if( bfl ) {
      obj_t proc = bfl->before;
      
      wind_stack( bfl->prev );
      
      if( !PROCEDURE_CORRECT_ARITYP( proc, 0 ) )
         the_failure( c_constant_string_to_string( "dynamic-wind" ),
                      c_constant_string_to_string( "illegal arity" ),
                      BINT( PROCEDURE_ARITY( proc ) ) );
      else
	 PROCEDURE_ENTRY( proc )( proc, BEOA );
   }
}
      
/*---------------------------------------------------------------------*/
/*    apply_continuation ...                                           */
/*    -------------------------------------------------------------    */
/*    When applying a continuation, we first unwind the stack.         */
/*    Either we reached the stack bottom and we have to restore        */
/*    the whole stack. Either, we find the escape procedure            */
/*    and we stop.                                                     */
/*---------------------------------------------------------------------*/
obj_t
apply_continuation( obj_t kont, obj_t value ) {
   obj_t stack;
   obj_t restore;
   struct exitd *etop;
   obj_t estamp;
   const obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   struct exitd ctop;

   if( !PROCEDUREP( kont ) ||
       ((obj_t)(PROCEDURE_ENTRY( kont )) != ((obj_t)&apply_continuation)) )
      /* We check if a kont is a legal continution by checking if */
      /* it is first a continuation and then if its entry is is   */
      /* apply continuation.                                      */
      the_failure( c_constant_string_to_string( "apply_continuation" ),
		   c_constant_string_to_string( "continuation" ),
		   kont );
   
   stack = PROCEDURE_REF( kont, 0 );
   etop = STACK( stack ).exitd_top;
   estamp = STACK( stack ).stamp;
   
   restore = make_fx_procedure( callcc_restore_stack, 1, 1 );
   PROCEDURE_SET( restore, 0, kont );

   /* We check that the continuation is applied on the same thread */
   if( STACK( stack ).stack_bot != BGL_ENV_STACK_BOTTOM( env ) )
      C_FAILURE( "apply_continuation",
		 "attempted to apply foreign continuation (created in another thread)",
		 kont ); 
	 
   return unwind_stack_until( (obj_t)(etop), estamp, value, restore );
}

/*---------------------------------------------------------------------*/
/*    callcc activation frame                                          */
/*---------------------------------------------------------------------*/
static obj_t  stack;
static char  *stack_top;
static long   stack_size;
static obj_t  s_value;
static obj_t  stamp;
static void (*memorycpy)( void*, void*, size_t );

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    callcc_init_stack ...                                            */
/*    -------------------------------------------------------------    */
/*    This function has to be separated from callcc_install_stack      */
/*    in order to ensure that it allocates its own large enough        */
/*    activation frame.                                                */
/*---------------------------------------------------------------------*/
static void 
callcc_init_stack() {
   const obj_t env = bgl_current_dynamic_env();

   /* Restore the global before link pointer */
   BGL_ENV_BEFORED_TOP_SET( env, STACK( stack ).before_top );

   /* And now evaluate all the DYNAMIC-WIND's before thunks */
   wind_stack( BGL_ENV_BEFORED_TOP( env ) );
      
   /* restore bexit linking */
   BGL_ENV_EXITD_TOP_SET( env, STACK( stack ).exitd_top );

   /* jump to the continuation, evaluting the DYNAMIC-WIND's after thunks */
   unwind_stack_until( (obj_t)(BGL_ENV_EXITD_TOP( env )), stamp, s_value, BFALSE );
}

void (*__callcc_init_stack)() = &callcc_init_stack;
 
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    callcc_install_stack ...                                         */
/*---------------------------------------------------------------------*/
static void
callcc_install_stack( obj_t kont, obj_t value ) {
   /* from now on we cannot use local variables because */
   /* the stack will be erased.                         */
   const obj_t env = bgl_current_dynamic_env();

   s_value    = value;
   stack      = PROCEDURE_REF( kont, 0 );
   stack_top  = STACK( stack ).stack_top;
   stack_size = STACK( stack ).size;
   stamp      = STACK( stack ).stamp;
   memorycpy  = (void (*)( void*, void*, size_t ))PROCEDURE_REF( kont, 1 );

   /* Check the stack before restore */
   if( (!STACKP( stack )) || (CREF( stack ) != STACK( stack ).self) )
      C_FAILURE( "apply_continuation",
		 "not a C stack",
		 stack );
   else {
      /* Flush Sparc windows registers */
      flush_regs_in_stack();

      /* Restore the stack. We don't directly use memcpy because   */
      /* this function can be inlined by the C compiler. Hence,    */
      /* we tric the C compiler by using the copy function         */
      /* (i.e., memcpy) that has been stored in the procedure when */
      /* the stack has been allocated.                             */

#if( defined( STACK_GROWS_DOWN ) )
      memorycpy( stack_top, &(STACK( stack ).stack), stack_size );
#else
      memorycpy( BGL_ENV_STACK_BOTTOM( env ), &(STACK( stack ).stack), stack_size );
#endif

      __callcc_init_stack();
   }
}

void (*__callcc_install_stack)(obj_t, obj_t) = &callcc_install_stack;

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    callcc_restore_stack ...                                         */
/*---------------------------------------------------------------------*/
static obj_t
callcc_restore_stack( obj_t env, obj_t value, char **_dummy ) {
   char *stack_top;
   char *actual_stack_top;
   obj_t stack;
   obj_t kont = PROCEDURE_REF( env, 0 );
   char *dummy[ 1024 ];
   
   actual_stack_top = (char *)bgl_get_top_of_stack();
   stack = PROCEDURE_REF( kont, 0 );
   stack_top = STACK( stack ).stack_top;

   /* on fait grandire la pile jusqu'a ce qu'elle depasse stack_top */
#if( defined( STACK_GROWS_DOWN ) )
   if( ((unsigned long)stack_top) <= (unsigned long)actual_stack_top)
#else 
   if( ((unsigned long)stack_top) >= (unsigned long)actual_stack_top )
#endif
   {
      /* Je fais un appel recursif pour faire grandir la pile.        */
      /* C'est absolument atroce, mais je passe un arg supplementaire */
      /* a la fonction: dummy. La raison est que j'ai peur qu'un      */
      /* compilo tres intelligent s'apercoive que je n'utilise pas    */
      /* cette variable et donc qu'il me la supprime oubien que ce    */
      /* meme compilo fasse une optimisation sur un appel qui est     */
      /* recursif terminal et donc qu'il transforme cela en un goto,  */
      /* sans faire grandir la pile. D'autre part, comme la fonction  */
      /* `alloca' n'est pas standard et donc qu'il ne faut pas        */
      /* pour allouer sur la pile.                                    */
      /* En plus, afin, d'etre sur que dummy ne va pas etre mangee    */
      /* par un compilo trop intelligent, on la range dans une        */
      /* variable globale.                                            */

      /* if we have alloca, use it to increase the size of the stack. */
      /* This helps guard against the case where the recursive call   */
      /* is optimized to a goto. This can happen when linktime        */
      /* optimization is used.                                        */

#if( defined( BGL_HAVE_ALLOCA ) ) 
#if( defined( STACK_GROWS_DOWN ) )
     unsigned long stack_incr = (unsigned long)actual_stack_top -
       (unsigned long)stack_top;
#else
     unsigned long stack_incr = (unsigned long)stack_top -
       (unsigned long)actual_stack_top;
#endif
     alloca(stack_incr+1);
#endif     
     
     glob_dummy = (long)dummy;
     callcc_restore_stack( env, value, &dummy[ 1 ] );
   } else {
      __callcc_install_stack( kont, value );
   }

   return (obj_t)_dummy;
}

/*---------------------------------------------------------------------*/
/*    call_cc ...                                                      */
/*---------------------------------------------------------------------*/
obj_t
call_cc( obj_t proc ) {
   /* this variable _must_ be named jmpbuf because of SET_EXIT */
   /* that uses this name.                                     */
   callcc_jmp_buf jmpbuf; 
   const obj_t env = BGL_CURRENT_DYNAMIC_ENV();

   /* Warning CALLCC_SET_EXIT is a magic macro. It handles IA64 arch */
   if( !CALLCC_SET_EXIT( BUNSPEC ) ) {
      obj_t continuation;
      obj_t stack;
      char *stack_top;
      unsigned long stack_size;
      obj_t aux;
      /* We push the exit taking care that it is a callcc exit. */
      PUSH_ENV_EXIT( env, (obj_t)(&jmpbuf), EXITD_CALLCC );
      
      /* sur sparc, il est indispensables de flusher les registres. */
      flush_regs_in_stack();

      /* on recupere l'adresse du sommet de pile */
      stack_top = (char *)bgl_get_top_of_stack();
      
      /* on calcule la taille de la pile, en prevoyant que le GC peut */
      /* flusher les registres dans la pile (REGISTER_SAVE_BUFFER)    */
#if( defined( STACK_GROWS_DOWN ) )
      stack_size = (unsigned long)BGL_ENV_STACK_BOTTOM( env )-(unsigned long)stack_top;
#else
      stack_size = (unsigned long)stack_top-(unsigned long)BGL_ENV_STACK_BOTTOM( env );
#endif
      /* on alloue un espace pour la sauvegarder de la pile  */
      stack = MAKE_STACK( stack_size + sizeof(char *), aux );

      STACK( stack ).size       = (long)stack_size;
      STACK( stack ).self       = CREF( stack );
      STACK( stack ).exitd_top  = BGL_ENV_EXITD_TOP( env );
      STACK( stack ).stamp      = BGL_ENV_EXITD_TOP( env )->stamp;
      STACK( stack ).before_top = BGL_ENV_BEFORED_TOP( env );
      STACK( stack ).stack_top  = stack_top;
      STACK( stack ).stack_bot  = BGL_ENV_STACK_BOTTOM( env );
      
      /* on construit la continuation */
      continuation = make_fx_procedure( &apply_continuation, 1, 2 );
      PROCEDURE_SET( continuation, 0, stack );
      PROCEDURE_SET( continuation, 1, (obj_t)&memcpy );

      /* on duplique la pile */
#if( defined( STACK_GROWS_DOWN ) )
      memcpy( &(STACK( stack ).stack), (char *)stack_top, stack_size );
#else
      memcpy( &(STACK( stack ).stack), (char *)BGL_ENV_STACK_BOTTOM(), stack_size);
#endif
 
      /* on va faire l'application mais avant il faut qu'on test */
      /* que la fonction est correcte. Pour cela, on va verifier */
      /* que la fonction a une arite correcte.                   */
      /* Le test procedurep( proc ) a deja ete effectue dans la  */
      /* definition scheme de `call/cc'.                         */
      if( !PROCEDURE_CORRECT_ARITYP( proc, 1 ) ) {
         return the_failure( c_constant_string_to_string( "call/cc" ),
			     c_constant_string_to_string( "illegal arity" ),
			     BINT( PROCEDURE_ARITY( proc ) ) );
      } else {
	 obj_t val = PROCEDURE_ENTRY( proc )( proc, continuation, BEOA );

	 POP_ENV_EXIT( env );
	 return val;
      }
   } else {
      if( unwind_stack_value_p( BGL_ENV_EXIT_VALUE( env ) ) )
         return the_failure( c_constant_string_to_string( "call/cc" ),
			     c_constant_string_to_string( "illegal continuation" ),
			     BINT( PROCEDURE_ARITY( proc ) ) );
      else
	 return BGL_ENV_EXIT_VALUE( env );
   }
}
