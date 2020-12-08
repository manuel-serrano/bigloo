/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cinit_obj.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jan 29 09:19:48 2002                          */
/*    Last change :  Thu Apr 19 09:32:34 2018 (serrano)                */
/*    Copyright   :  2002-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bootstrap of pre-allocated objects.                              */
/*=====================================================================*/
#include <stdio.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Les objects vitaux ...                                           */
/*---------------------------------------------------------------------*/
obj_t an_object;

obj_t quote;
obj_t *c_allocated_char;

obj_t bprof_port = BUNSPEC;

BGL_RUNTIME_DEF header_t bgl_opaque_nil = MAKE_HEADER( OPAQUE_TYPE, 0 );

/*---------------------------------------------------------------------*/
/*    Global mutex                                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t bigloo_mutex = BUNSPEC;
BGL_RUNTIME_DEF obj_t bigloo_generic_mutex = BUNSPEC;
DEFINE_STRING( bigloo_mutex_name, _1, "bigloo-mutex", 12 );

/*---------------------------------------------------------------------*/
/*    Global floating point constants                                  */
/*---------------------------------------------------------------------*/
double bgl_nan(), bgl_infinity();
#if( !BGL_NAN_TAGGING )
BGL_RUNTIME_DEF obj_t bigloo_nan, bigloo_infinity, bigloo_minfinity;
#else
BGL_RUNTIME_DEF union nanobj bigloo_nan, bigloo_infinity, bigloo_minfinity;
#endif

/*---------------------------------------------------------------------*/
/*    Importations                                                     */
/*---------------------------------------------------------------------*/
extern obj_t string_to_obj( obj_t, obj_t, obj_t );
extern void bgl_init_dynamic_env();
extern void bgl_init_symbol_table();
extern void bgl_init_socket();
extern void bgl_init_date();
extern void bgl_init_signal();
extern void bgl_init_keyword_table();
extern void bgl_init_io();
extern void bgl_init_trace();
extern void bgl_init_process_table();
extern void bgl_init_dload();
extern void bgl_init_bignum();
#if( BGL_SAW == 1  && BGL_GC == BGL_SAW_GC )    
extern void bgl_saw_init();
#endif

/*---------------------------------------------------------------------*/
/*    init_objects ...                                                 */
/*---------------------------------------------------------------------*/
void bgl_init_objects() {
#if( BGL_SAW == 1  && BGL_GC == BGL_SAW_GC )    
   bgl_saw_init();
#endif   
   bgl_init_dynamic_env();
   bgl_init_trace();
   bgl_init_symbol_table();
   bgl_init_signal();
   bgl_init_io();
   bgl_init_keyword_table();
   bgl_init_process_table();
   bgl_init_dload();
   bgl_init_socket();
   bgl_init_date();
   bgl_init_bignum();

   bigloo_mutex = bgl_make_spinlock( bigloo_mutex_name );
   bigloo_generic_mutex = bgl_make_spinlock( bigloo_mutex_name );
   quote = string_to_symbol( "QUOTE" );

#if( !BGL_NAN_TAGGING )
   bigloo_nan = DOUBLE_TO_REAL( bgl_nan() );
   bigloo_infinity = DOUBLE_TO_REAL( bgl_infinity() );
   bigloo_minfinity = DOUBLE_TO_REAL( -bgl_infinity() );
#else
   bigloo_nan = (union nanobj){ real: bgl_nan() };
   bigloo_infinity = (union nanobj){ real: bgl_infinity() };
   bigloo_minfinity = (union nanobj){ real: -bgl_infinity() };
#endif
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_module_debug_level ...                                       */
/*---------------------------------------------------------------------*/
static int bgl_module_debug_level = -1;

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_module_margins ...                                           */
/*---------------------------------------------------------------------*/
char *bgl_module_margins[] = { "",
			       " ",
			       "  ",
			       "   ",
			       "    ",
			       "     ",
			       "      ",
			       "       ",
			       "        ",
			       "         ",
			       "          ",
			       "           ",
			       "            ",
			       "             ",
			       "              ",
			       "               ",
			       "                " };

/*---------------------------------------------------------------------*/
/*    static char *                                                    */
/*    bgl_init_module_debug_margin ...                                 */
/*---------------------------------------------------------------------*/
static char *
margin( int level ) {
#define MAX_MODULE_DEBUG_LEVEL 16
   return bgl_module_margins[ bgl_module_debug_level < MAX_MODULE_DEBUG_LEVEL ?
			      bgl_module_debug_level : MAX_MODULE_DEBUG_LEVEL ];
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_module_debug_start ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_init_module_debug_start( char *mod ) {
   bgl_module_debug_level++;
   fprintf( stderr, "%s>>> %s (%d)\n",
	    margin( bgl_module_debug_level ),
	    mod,
	    bgl_module_debug_level );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_module_debug_library ...                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_init_module_debug_library( char *mod ) {
   fprintf( stderr, "%s library (%s)\n",
	    margin( bgl_module_debug_level ),
	    mod );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_module_debug_object ...                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_init_module_debug_object( char *mod ) {
   fprintf( stderr, "%s object (%s)\n",
	    margin( bgl_module_debug_level ),
	    mod );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_module_debug_string ...                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_init_module_debug_string( char *str ) {
   fprintf( stderr, "%s %s\n",
	    margin( bgl_module_debug_level ),
	    str );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_module_debug_import ...                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_init_module_debug_import( char *mod, char *import ) {
   fprintf( stderr, "%s import (%s) %s\n",
	    margin( bgl_module_debug_level ),
	    mod,
	    import );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_module_debug_end ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_init_module_debug_end( char *mod ) {
   fprintf( stderr, "%s<<< %s\n", margin( bgl_module_debug_level ), mod );
   bgl_module_debug_level--;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_bmem_reset ...                                               */
/*    -------------------------------------------------------------    */
/*    This dummy function is overriden (LD_PRELOADed) by the bmem.so   */
/*    library. It is used by bmem to reset the allocation statistics   */
/*    gathering during the execution of a program, on user demand.     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_bmem_reset() {
   return BFALSE;
}

/*---------------------------------------------------------------------*/
/*    double bgl_zero                                                  */
/*    -------------------------------------------------------------    */
/*    These definitions are used when the C compiler does not support  */
/*    static divisions by 0, such as Intel's ICC.                      */
/*---------------------------------------------------------------------*/
double bgl_zero = 0.0;

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_nan ...                                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF double
bgl_nan() {
   return 0./ bgl_zero;
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_infinity ...                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF double
bgl_infinity() {
   return 1.0 / bgl_zero;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    __debug ...                                                      */
/*---------------------------------------------------------------------*/
obj_t
__debug( char *lbl, obj_t o ) {
   fprintf( stderr, "%s:%d %s o=%p\n", __FILE__, __LINE__, lbl, o );
   if( BGL_HVECTORP( o ) ) {
      fprintf( stderr, "   hvector=%lu\n",BGL_HVECTOR_LENGTH( o ) );
   } else if( REALP( o ) ) {
      fprintf( stderr, "   real\n" );
   } else if( PAIRP( o ) ) {
      fprintf( stderr, "   pair\n" );
   } else if( SYMBOLP( o ) ) {
      fprintf( stderr, "   symbol=%s\n",
	       BSTRING_TO_STRING( SYMBOL_TO_STRING( o ) ) );
   } else if( INTEGERP( o ) ) {
      fprintf( stderr, "   int=%ld\n",CINT( o ) );
   } else if( REALP( o ) ) {
      fprintf( stderr, "   real=%f\n",REAL_TO_DOUBLE( o ) );
   } else if( BGL_OBJECTP( o ) ) {
      fprintf( stderr, "   object=%ld\n", BGL_OBJECT_CLASS_NUM( o ) );
   } else if( POINTERP( o ) ) {
      fprintf( stderr, "   PTRP=%d TYPE=%ld\n", POINTERP( o ), TYPE( o ) );
   }
   return o;
}
