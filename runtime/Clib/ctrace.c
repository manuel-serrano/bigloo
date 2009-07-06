/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/ctrace.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Mar 31 18:06:36 1995                          */
/*    Last change :  Fri Mar  6 17:45:24 2009 (serrano)                */
/*    -------------------------------------------------------------    */
/*    We dump a execution trace                                        */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_trace ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
void
bgl_init_trace() {
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();

   BGL_DYNAMIC_ENV( env ).top.symbol = BUNSPEC;
   BGL_DYNAMIC_ENV( env ).top.link = 0;
 
   BGL_ENV_SET_TOP_OF_FRAME( env, &(BGL_DYNAMIC_ENV( env ).top) );
}

/*---------------------------------------------------------------------*/
/*    dump_trace_stack ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
dump_trace_stack( obj_t port, int depth ) {
   long level = 0L;
   struct bgl_dframe *runner = BGL_ENV_GET_TOP_OF_FRAME( BGL_CURRENT_DYNAMIC_ENV() );
   obj_t old = 0;
   int recursion = 0;
   char buffer[ 100 ];

   while( (level < depth) && runner ) {
      if( SYMBOLP( runner->symbol ) ) {
	 if( EQP( runner->symbol, old ) )
	    recursion++;
	 else {
	    if( recursion > 0 ) {
	       bgl_display_string( string_to_bstring( " (" ), port );
	       bgl_display_fixnum( BINT( 1 + recursion ), port );
	       bgl_display_string( string_to_bstring( " times)\n" ), port );
	    } else {
	       if( level > 0 )
		  bgl_display_string( string_to_bstring( "\n" ), port );
	    }

	    sprintf( buffer, "  %3ld.", level );
	    bgl_display_string( string_to_bstring( buffer ), port );
	    bgl_display_string( SYMBOL_TO_STRING( runner->symbol ), port );
	    
	    recursion = 0;
	 }
	 old = runner->symbol;
			
	 level++; 
      }
      
      runner = runner->link;
   }
   
   if( recursion > 0 ) {
      bgl_display_string( string_to_bstring( " (" ), port );
      bgl_display_fixnum( BINT( 1 + recursion ), port );
      bgl_display_string( string_to_bstring( " times)\n" ), port );
   }
   
   bgl_display_string( string_to_bstring( "\n" ), port );
   
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    get_trace_stack ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
get_trace_stack( int depth ) {
   long level = 0L;
   struct bgl_dframe *runner = BGL_ENV_GET_TOP_OF_FRAME( BGL_CURRENT_DYNAMIC_ENV() );
   obj_t l = BNIL;

   while( (level < depth) && runner ) {
      if( SYMBOLP( runner->symbol ) ) {
	 l = MAKE_PAIR( runner->symbol, l );
	 level++; 
      }
      
      runner = runner->link;
   }
   
   return l;
}

/*---------------------------------------------------------------------*/
/*    cref ...                                                         */
/*---------------------------------------------------------------------*/
obj_t
cref( obj_t obj ) {
   return CREF( obj );
}

/*---------------------------------------------------------------------*/
/*    car ...                                                          */
/*---------------------------------------------------------------------*/
obj_t
car( obj_t obj ) {
   return CAR( obj );
}

/*---------------------------------------------------------------------*/
/*    cdr ...                                                          */
/*---------------------------------------------------------------------*/
obj_t
cdr( obj_t obj ) {
   return CDR( obj );
}

/*---------------------------------------------------------------------*/
/*    byteshow ...                                                     */
/*---------------------------------------------------------------------*/
static void
byteshow( unsigned char *addr ) {
#define PP_CHAR( c ) (((c) >= 33) && ((c) < 127)) ? c : '.'

   printf( "  %08lx  :  %02x %02x %02x %02x  :  %c%c%c%c\n",
           (unsigned long)addr,
           addr[ 0 ],
           addr[ 1 ],
           addr[ 2 ],
           addr[ 3 ],
           PP_CHAR( addr[ 0 ] ),
           PP_CHAR( addr[ 1 ] ),
           PP_CHAR( addr[ 2 ] ),
           PP_CHAR( addr[ 3 ] ) );
}
             
/*---------------------------------------------------------------------*/
/*    memshow ...                                                      */
/*---------------------------------------------------------------------*/
void
memshow( char *from, char *to, long step ) {
   char *i;

   step *= 4;
      
   if( from > to )
      for( i = from; i > to; i -= step )
         byteshow( (unsigned char *)i );
   else
      for( i = from; i < to; i += step )
         byteshow( (unsigned char *)i );

   puts( "" );
   return ;
}
