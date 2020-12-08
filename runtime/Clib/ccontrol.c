/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/ccontrol.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 17 13:16:31 1995                          */
/*    Last change :  Wed Nov 20 10:55:13 2019 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Closure allocations.                                             */
/*=====================================================================*/
#include <bigloo.h>
#include <stdarg.h>

/*---------------------------------------------------------------------*/
/*    External definitions.                                            */
/*---------------------------------------------------------------------*/
extern obj_t make_string_sans_fill( long );

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_procedure ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_procedure( obj_t entry, int arity, int size ) {
   if( arity >= 0 )
      return make_fx_procedure( (obj_t (*)())entry, arity, size );
   else
      return make_va_procedure( (obj_t (*)())entry, arity, size );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_dup_procedure ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_dup_procedure( obj_t proc ) {
   int size = PROCEDURE_LENGTH( proc );
   obj_t n_proc = GC_MALLOC( BGL_PROCEDURE_BYTE_SIZE( size ) );
   obj_t o_proc = CREF( proc );
			      
   n_proc->procedure.header = o_proc->procedure.header;
   n_proc->procedure.entry = o_proc->procedure.entry;
   n_proc->procedure.va_entry = o_proc->procedure.va_entry;
   n_proc->procedure.attr = o_proc->procedure.attr;
   n_proc->procedure.arity = o_proc->procedure.arity;

   while( size-- > 0 ) {
      PROCEDURE_SET( BREF( n_proc ), size, PROCEDURE_REF( proc, size ) );
   }

   return BREF( n_proc );
}

/*---------------------------------------------------------------------*/
/*    INIT_FX_PROCEDURE                                                */
/*---------------------------------------------------------------------*/
#define INIT_FX_PROCEDURE( proc, entry, arity, size ) \
   (proc->procedure.header = MAKE_HEADER( PROCEDURE_TYPE, size ), \
    proc->procedure.entry = entry, \
    proc->procedure.va_entry = 0L, \
    proc->procedure.attr = BUNSPEC, \
    proc->procedure.arity = arity, \
    BREF( proc ))
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_init_fx_procedure ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_init_fx_procedure( obj_t proc, obj_t (*entry)(), int arity, int size ) {
   if( size > (1 << HEADER_SIZE_BIT_SIZE) ) {
      C_FAILURE( "make-fx-procedure", "Environment to large", BINT( size ) );
   } else {
      return INIT_FX_PROCEDURE( proc, entry, arity, size );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_fx_procedure ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_fx_procedure( obj_t (*entry)(), int arity, int size ) {
   if( size > (1 << HEADER_SIZE_BIT_SIZE) ) {
      C_FAILURE( "make-fx-procedure", "Environment to large", BINT( size ) );
   } else {
      obj_t a_tproc = GC_MALLOC( BGL_PROCEDURE_BYTE_SIZE( size ) );
	      
      return INIT_FX_PROCEDURE( a_tproc, entry, arity, size );
   }
}

/*---------------------------------------------------------------------*/
/*    make_va_procedure ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_va_procedure( obj_t (*entry)(), int arity, int size ) {

   if( size > (1 << HEADER_SIZE_BIT_SIZE) ) {
      C_FAILURE( "make-va-procedure", "Environment to large", BINT( size ) );
   } else {
      int byte_size = PROCEDURE_SIZE + ((size-1) * OBJ_SIZE);
      obj_t a_tproc = GC_MALLOC( byte_size );
	      
      a_tproc->procedure.header = MAKE_HEADER( PROCEDURE_TYPE, size );
      a_tproc->procedure.entry = (obj_t (*)())va_generic_entry; 
      a_tproc->procedure.va_entry = entry;
      a_tproc->procedure.attr = BUNSPEC;
      a_tproc->procedure.arity = arity;
      
      return BREF( a_tproc );
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    generic_entry ...                                                */
/*---------------------------------------------------------------------*/
static obj_t
generic_entry( obj_t proc, ... ) {
   va_list argl;
   obj_t optional;
   obj_t runner;

   va_start( argl, proc );
   
   if( (runner = va_arg( argl, obj_t )) != BEOA ) {
      obj_t tail;
      
      optional = tail = MAKE_PAIR( runner, BNIL );
      
      while( (runner = va_arg( argl, obj_t )) != BEOA ) {
         SET_CDR( tail, MAKE_PAIR( runner, BNIL ) );
         tail = CDR( tail );
      } 
   }
   else
      optional = BNIL;

   va_end( argl );
   
   return apply( PROCEDURE_REF( proc, 3 ), optional );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    generic_entry1 ...                                               */
/*---------------------------------------------------------------------*/
static obj_t
generic_entry1( obj_t proc, obj_t a1 ) {
   obj_t p = PROCEDURE_REF( proc, 3 );

   return PROCEDURE_ENTRY( p )( p, a1 );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    generic_entry2 ...                                               */
/*---------------------------------------------------------------------*/
static obj_t
generic_entry2( obj_t proc, obj_t a1, obj_t a2 ) {
   obj_t p = PROCEDURE_REF( proc, 3 );

   return PROCEDURE_ENTRY( p )( p, a1, a2 );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    generic_entry3 ...                                               */
/*---------------------------------------------------------------------*/
static obj_t
generic_entry3( obj_t proc, obj_t a1, obj_t a2, obj_t a3 ) {
   obj_t p = PROCEDURE_REF( proc, 3 );

   return PROCEDURE_ENTRY( p )( p, a1, a2, a3 );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    generic_entry4 ...                                               */
/*---------------------------------------------------------------------*/
static obj_t
generic_entry4( obj_t proc, obj_t a1, obj_t a2, obj_t a3, obj_t a4 ) {
   obj_t p = PROCEDURE_REF( proc, 3 );

   return PROCEDURE_ENTRY( p )( p, a1, a2, a3, a4 );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    generic_entry5 ...                                               */
/*---------------------------------------------------------------------*/
static obj_t
generic_entry5( obj_t proc, obj_t a1, obj_t a2, obj_t a3, obj_t a4, obj_t a5 ) {
   obj_t p = PROCEDURE_REF( proc, 3 );

   return PROCEDURE_ENTRY( p )( p, a1, a2, a3, a4, a5 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_generic ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_generic( obj_t proc ) {
   int arity = PROCEDURE_ARITY( proc );
   obj_t res; 

   switch( arity ) {
      case 1:
	 res = make_fx_procedure( (obj_t (*)())generic_entry1, arity, 4 );
	 break;
      case 2:
	 res = make_fx_procedure( (obj_t (*)())generic_entry2, arity, 4 );
	 break;
      case 3:
	 res = make_fx_procedure( (obj_t (*)())generic_entry3, arity, 4 );
	 break;
      case 4:
	 res = make_fx_procedure( (obj_t (*)())generic_entry4, arity, 4 );
	 break;
      case 5:
	 res = make_fx_procedure( (obj_t (*)())generic_entry5, arity, 4 );
	 break;
      default:
	 res = make_fx_procedure( (obj_t (*)())generic_entry, arity, 4 );
	 break;
   }

   PROCEDURE_SET( res, 3, proc );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_procedure_entry_to_string ...                                */
/*---------------------------------------------------------------------*/
obj_t
bgl_procedure_entry_to_string( obj_t proc ) {
   obj_t res = make_string_sans_fill( 17 );
   
   if( VA_PROCEDUREP( proc ) ) {
      sprintf( BSTRING_TO_STRING( res ), "%016lx", (long)PROCEDURE_VA_ENTRY( proc ) );
   } else {
      sprintf( BSTRING_TO_STRING( res ), "%016lx", (long)PROCEDURE_ENTRY( proc ) );
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_string_to_procedure_entry ...                                */
/*---------------------------------------------------------------------*/
obj_t
bgl_string_to_procedure_entry( obj_t string ) {
   return (obj_t)(strtoul( BSTRING_TO_STRING( string ), 0, 16 ) );
}

/*---------------------------------------------------------------------*/
/*    va_generic_entry ...                                             */
/*    -------------------------------------------------------------    */
/*    Tous les tests d'arite ont ete expanses `inline'. On n'a plus    */
/*    qu'a faire l'appel.                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
va_generic_entry( obj_t proc, ... ) {
   va_list argl;
   int     arity;
   int     require;
   obj_t   arg[ 16 ];
   obj_t   optional;
   obj_t   runner;
   long    i;

   va_start( argl, proc );
   
   arity  = PROCEDURE_ARITY( proc );
   require = -arity - 1;

   for( i = 0; i < require; i++ )
      arg[ i ] = va_arg( argl, obj_t );

   if( (runner = va_arg( argl, obj_t )) != BEOA ) {
      obj_t tail;
      
      optional = tail = MAKE_PAIR( runner, BNIL );
      
      while( (runner = va_arg( argl, obj_t )) != BEOA ) {
         SET_CDR( tail, MAKE_PAIR( runner, BNIL ) );
         tail = CDR( tail );
      } 
   }
   else
      optional = BNIL;

   va_end( argl );
   
#define CALL( proc ) ((obj_t (*)())PROCEDURE_VA_ENTRY( proc ))      
   switch( arity ) {
      case -1  : return CALL( proc )(proc, optional);
      case -2  : return CALL( proc )(proc, arg[ 0 ], optional);
      case -3  : return CALL( proc )(proc, arg[ 0 ], arg[ 1 ], optional);
      case -4  : return CALL( proc )(proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                      optional);
      case -5  : return CALL( proc )(proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], optional);
      case -6  : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], optional);
      case -7  : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     optional);
      case -8  : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], optional);
      case -9  : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], optional);
      case -10 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     optional);
      case -11 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], optional);
      case -12 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], optional);
      case -13 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     optional);
      case -14 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     arg[ 12 ], optional);
      case -15 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     arg[ 12 ], arg[ 13 ], optional);
      case -16 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     arg[ 12 ], arg[ 13 ], arg[ 14 ],
                                     optional);
      case -17 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     arg[ 12 ], arg[ 13 ], arg[ 14 ],
                                     arg[ 15 ], optional);
      
      default: C_FAILURE( "va_generic_entry",
			  "too many argument expected",
			  BINT( arity ) );
   }
   return BNIL;
}

/*---------------------------------------------------------------------*/
/*    bgl_va_stack_entry ...                                           */
/*    -------------------------------------------------------------    */
/*    Entry point of stack allocated varargs functions.                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_va_stack_entry( obj_t proc, ... ) {
   va_list argl;
   int     arity;
   int     require;
   obj_t   arg[ 16 ];
   obj_t   optional;
   obj_t   runner;
   long    i;

   va_start( argl, proc );
   
   arity  = PROCEDURE_ARITY( proc );
   require = -arity - 1;

   for( i = 0; i < require; i++ )
      arg[ i ] = va_arg( argl, obj_t );

   if( (runner = va_arg( argl, obj_t )) != BEOA ) {
      obj_t tail;
      obj_t __tmp;
      
      optional = tail = MAKE_STACK_PAIR_TMP( runner, BNIL, __tmp );
      
      while( (runner = va_arg( argl, obj_t )) != BEOA ) {
         SET_CDR( tail, MAKE_STACK_PAIR_TMP( runner, BNIL, __tmp ) );
         tail = CDR( tail );
      } 
   } else {
      optional = BNIL;
   }
   
   va_end( argl );
   
#define CALL( proc ) ((obj_t (*)())PROCEDURE_VA_ENTRY( proc ))      
   switch( arity ) {
      case -1  : return CALL( proc )(proc, optional);
      case -2  : return CALL( proc )(proc, arg[ 0 ], optional);
      case -3  : return CALL( proc )(proc, arg[ 0 ], arg[ 1 ], optional);
      case -4  : return CALL( proc )(proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                      optional);
      case -5  : return CALL( proc )(proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], optional);
      case -6  : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], optional);
      case -7  : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     optional);
      case -8  : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], optional);
      case -9  : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], optional);
      case -10 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     optional);
      case -11 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], optional);
      case -12 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], optional);
      case -13 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     optional);
      case -14 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     arg[ 12 ], optional);
      case -15 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     arg[ 12 ], arg[ 13 ], optional);
      case -16 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     arg[ 12 ], arg[ 13 ], arg[ 14 ],
                                     optional);
      case -17 : return CALL( proc )( proc, arg[ 0 ], arg[ 1 ], arg[ 2 ],
                                     arg[ 3 ], arg[ 4 ], arg[ 5 ],
                                     arg[ 6 ], arg[ 7 ], arg[ 8 ],
                                     arg[ 9 ], arg[ 10 ], arg[ 11 ],
                                     arg[ 12 ], arg[ 13 ], arg[ 14 ],
                                     arg[ 15 ], optional);
      
      default: C_FAILURE( "va_generic_entry",
			  "too many argument expected",
			  BINT( arity ) );
   }
   return BNIL;
}

/*---------------------------------------------------------------------*/
/*    opt_generic_entry ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
opt_generic_entry( obj_t proc, ... ) {
   va_list argl;
   int len = 0;
   obj_t args;
   obj_t runner;
   long i;
   int byte_size;
   obj_t res;
   
   /* compute the number of arguments */
   va_start( argl, proc );
   while( va_arg( argl, obj_t ) != BEOA ) len++;
   va_end( argl );
   
   /* Stack allocate the argument vector, see          */
   /* cvector.c:create_vector for regular vector alloc */
   byte_size = VECTOR_SIZE + ( (len-1) * OBJ_SIZE );

#if( __APPLE__ == 1 && __APPLE_CC__ >= 6000 )    
   args = (obj_t)malloc( byte_size );
#else   
   args = (obj_t)alloca( byte_size );
#endif   

#if( !defined( TAG_VECTOR ) )
   args->vector.header = MAKE_HEADER( VECTOR_TYPE, byte_size );
#endif		
   args->vector.length = len;

   args = BVECTOR( args );

   /* fill the vector, up to arity argument */
   va_start( argl, proc );
   for( i = 0; i < len; i++ ) VECTOR_SET( args, i, va_arg( argl, obj_t ) );
   va_end( argl );

   /* jump to the function */
#define CALL( proc ) ((obj_t (*)())PROCEDURE_VA_ENTRY( proc ))
   res = CALL( proc )( proc, args );

#if( __APPLE__ == 1 )
   free( CVECTOR( args ) );
#endif

   return res;
}

/*---------------------------------------------------------------------*/
/*    Eval procedures                                                  */
/*    -------------------------------------------------------------    */
/*    This is a very risky hack. The procedure bgl_eval_procedure,     */
/*    bgl_eval_4procedure, and bgl_eval_4vaprocedure are substitute    */
/*    for subtyping creator. The clean implementation is the Java      */
/*    and Dotnet ones. Since C does not support for subtyping, we      */
/*    have hacked. We use the property that the procedure constructed  */
/*    by the evaluator are in a very limited number and that there     */
/*    is exactly *2* procedure of arity 1, *2* procedure of arity 2,   */
/*    and so on. There is one lambda for un-traced (un-named)          */
/*    procedures and one for traced (named) procedure. tThe exception  */
/*    comes from arity -1. Arity -1 represents functions accepting a   */
/*    variable number of arguments but also functions accepting more   */
/*    than 4 parameters. Hence, we have to use various functions when  */
/*    marking these procedures.                                        */
/*---------------------------------------------------------------------*/
static obj_t (*eval_procedure[ 9 ])();
static obj_t (*eval_traced_procedure[ 9 ])();
static obj_t (*eval_4procedure)();
static obj_t (*eval_traced_4procedure)();
static obj_t (*eval_4vaprocedure)();
static obj_t (*eval_traced_4vaprocedure)();

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_eval_procedurep ...                                          */
/*---------------------------------------------------------------------*/
bool_t
bgl_eval_procedurep( obj_t proc ) {
   int arity = PROCEDURE_ARITY( proc );
   int idx = arity >= 0 ? arity : -arity + 4;
   obj_t (*entry)() = (arity >= 0) ?
      (obj_t (*)())PROCEDURE_ENTRY( proc )
      : (obj_t (*)())PROCEDURE_VA_ENTRY( proc );

   return (eval_procedure[ idx ] == entry)
      || (eval_traced_procedure[ idx ] == entry);
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_eval_4procedurep ...                                         */
/*---------------------------------------------------------------------*/
bool_t
bgl_eval_4procedurep( obj_t proc ) {
   obj_t (*entry)() = (obj_t (*)())PROCEDURE_VA_ENTRY( proc );

   return (eval_4procedure == entry) || (eval_traced_4procedure == entry);
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_eval_4vaprocedurep ...                                       */
/*---------------------------------------------------------------------*/
bool_t
bgl_eval_4vaprocedurep( obj_t proc ) {
   obj_t (*entry)() = (obj_t (*)())PROCEDURE_VA_ENTRY( proc );

   return (eval_4vaprocedure == entry) || (eval_traced_4vaprocedure == entry);
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_eval_procedure ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_eval_procedure( obj_t proc ) {
   int arity = PROCEDURE_ARITY( proc );
   int idx = (arity >= 0 ? arity : -arity + 4);
   obj_t (*entry)() = (arity >= 0) ?
      (obj_t (*)())PROCEDURE_ENTRY( proc )
      : (obj_t (*)())PROCEDURE_VA_ENTRY( proc );

   eval_procedure[ idx ] = entry;
   return proc;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_eval_4procedure ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_eval_4procedure( obj_t proc ) {
   eval_4procedure = (obj_t (*)())PROCEDURE_VA_ENTRY( proc );
   return proc;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_eval_4vaprocedure ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_eval_4vaprocedure( obj_t proc ) {
   eval_4vaprocedure = (obj_t (*)())PROCEDURE_VA_ENTRY( proc );
   return proc;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_eval_traced_procedure ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_eval_traced_procedure( obj_t proc ) {
   int arity = PROCEDURE_ARITY( proc );
   int idx = (arity >= 0 ? arity : -arity + 4);
   obj_t (*entry)() = (arity >= 0) ?
      (obj_t (*)())PROCEDURE_ENTRY( proc )
      : (obj_t (*)())PROCEDURE_VA_ENTRY( proc );

   eval_traced_procedure[ idx ] = entry;
   return proc;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_eval_traced_4procedure ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_eval_traced_4procedure( obj_t proc ) {
   eval_traced_4procedure = (obj_t (*)())PROCEDURE_VA_ENTRY( proc );
   return proc;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_eval_traced_4vaprocedure ...                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_eval_traced_4vaprocedure( obj_t proc ) {
   eval_traced_4vaprocedure = (obj_t (*)())PROCEDURE_VA_ENTRY( proc );
   return proc;
}
