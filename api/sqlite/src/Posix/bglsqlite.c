/*=====================================================================*/
/*    .../project/bigloo/bigloo/api/sqlite/src/Posix/bglsqlite.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Mar 23 16:54:42 2005                          */
/*    Last change :  Tue Jul 11 17:35:16 2023 (serrano)                */
/*    Copyright   :  2005-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    SQLITE support                                                   */
/*=====================================================================*/
#include <sqlite3.h>
#include <string.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    sqlite3 *                                                        */
/*    bgl_sqlite_open ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF sqlite3 *
bgl_sqlite_open( char *path ) {
   int n;
   sqlite3 *db;

   n = sqlite3_open( path , &db );

   if( n ) {
      sqlite3_close( db );
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"instantiate::sqlite",
			(char *)sqlite3_errmsg( db ),
			string_to_bstring( path ) );
   }

   return db;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_sqlite_close ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_sqlite_close( sqlite3 *db, obj_t odb ) {
   if( db ) {
      if( sqlite3_close( db ) != SQLITE_OK ) {
	 C_SYSTEM_FAILURE( BGL_IO_ERROR,
			   "sqlite-close",
			   (char *)sqlite3_errmsg( db ),
			   odb );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    sqlite_callback_exec ...                                         */
/*---------------------------------------------------------------------*/
static int
sqlite_callback_exec( obj_t *res, int argc, char **argv, char **colname ) {
   if( argc == 0 ) {
      *res = BFALSE;
   } else {
      if( argv[ 0 ] ) {
	 *res = string_to_bstring( argv[ 0 ] );
      } else {
	 *res = BUNSPEC;
      }
   }
      
   return 0;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_sqlite_exec ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_sqlite_exec( sqlite3 *db, char *str, obj_t odb ) {
  char *msg;
  obj_t result = BFALSE;
  int rc;

  rc = sqlite3_exec( db, str, (int (*)())sqlite_callback_exec, &result, &msg );

  if( rc != SQLITE_OK ) {
     char *buf = (char *)alloca( strlen( str ) + 16 );
     
     sprintf( buf, "sqlite-exec:%s", str );
     sqlite3_free( msg );

     if( rc == SQLITE_LOCKED || rc == SQLITE_BUSY ) {
	C_SYSTEM_FAILURE( BGL_IO_TIMEOUT_ERROR, buf, msg, odb );
     } else {
	C_SYSTEM_FAILURE( BGL_ERROR, buf, msg, odb );
     }
  }
  
  return result;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    sqlite_apply ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
sqlite_apply( obj_t proc, int argc, char **argv ) {
#  define ARG( i ) (argv[ i ] ? string_to_bstring( argv[ i ] ) : BUNSPEC )

   /* sqlite_apply does not enforce a strict arity checking */
   /* it only requires argc > the-arity-of-the function.    */
   if( (VA_PROCEDUREP( proc ) && ((-argc - 1) <= (PROCEDURE_ARITY( proc ))))
       || (argc >= PROCEDURE_ARITY( proc ) ) ) {
      switch( argc ) {
	 case 0:
	    return BGL_PROCEDURE_CALL0( proc );


	 case 1:
	    return BGL_PROCEDURE_CALL1( proc, ARG( 0 ), BEOA );

	 case 2:
	    return BGL_PROCEDURE_CALL2( proc,
					ARG( 0 ), ARG( 1 ) );

	 case 3:
	    return BGL_PROCEDURE_CALL3( proc,
					ARG( 0 ), ARG( 1 ), ARG( 2 ) );

	 case 4:
	    return BGL_PROCEDURE_CALL4( proc,
					ARG( 0 ), ARG( 1 ), ARG( 2 ),
					ARG( 3 ) );

	 case 5:
	    return BGL_PROCEDURE_CALL5( proc,
					ARG( 0 ), ARG( 1 ), ARG( 2 ),
					ARG( 3 ), ARG( 4 ) );

	 case 6:
	    return BGL_PROCEDURE_CALL6( proc,
					ARG( 0 ), ARG( 1 ), ARG( 2 ),
					ARG( 3 ), ARG( 4 ), ARG( 5 ) );

	 case 7:
	    return BGL_PROCEDURE_CALL7( proc,
					ARG( 0 ), ARG( 1 ), ARG( 2 ),
					ARG( 3 ), ARG( 4 ), ARG( 5 ),
					ARG( 6 ) );

	 case 8:
	    return BGL_PROCEDURE_CALL8( proc,
					ARG( 0 ), ARG( 1 ), ARG( 2 ),
					ARG( 3 ), ARG( 4 ), ARG( 5 ),
					ARG( 6 ), ARG( 7 ) );

	 case 9:
	    return BGL_PROCEDURE_CALL9( proc,
					ARG( 0 ), ARG( 1 ), ARG( 2 ),
					ARG( 3 ), ARG( 4 ), ARG( 5 ),
					ARG( 6 ), ARG( 7 ), ARG( 8 ) );

	 case 10:
	    return BGL_PROCEDURE_CALL10( proc,
					 ARG( 0 ), ARG( 1 ), ARG( 2 ),
					 ARG( 3 ), ARG( 4 ), ARG( 5 ),
					 ARG( 6 ), ARG( 7 ), ARG( 8 ),
					 ARG( 9 ) );

	 case 11:
	    return BGL_PROCEDURE_CALL11( proc,
					 ARG( 0 ), ARG( 1 ), ARG( 2 ),
					 ARG( 3 ), ARG( 4 ), ARG( 5 ),
					 ARG( 6 ), ARG( 7 ), ARG( 8 ),
					 ARG( 9 ), ARG( 10 ) );

	 case 12:
	    return BGL_PROCEDURE_CALL12( proc,
					 ARG( 0 ), ARG( 1 ), ARG( 2 ),
					 ARG( 3 ), ARG( 4 ), ARG( 5 ),
					 ARG( 6 ), ARG( 7 ), ARG( 8 ),
					 ARG( 9 ), ARG( 10 ), ARG( 11 ) );

	 case 13:
	    return BGL_PROCEDURE_CALL13( proc,
					 ARG( 0 ), ARG( 1 ), ARG( 2 ),
					 ARG( 3 ), ARG( 4 ), ARG( 5 ),
					 ARG( 6 ), ARG( 7 ), ARG( 8 ),
					 ARG( 9 ), ARG( 10 ), ARG( 11 ),
					 ARG( 12 ) );

	 case 14:
	    return BGL_PROCEDURE_CALL14( proc,
					 ARG( 0 ), ARG( 1 ), ARG( 2 ),
					 ARG( 3 ), ARG( 4 ), ARG( 5 ),
					 ARG( 6 ), ARG( 7 ), ARG( 8 ),
					 ARG( 9 ), ARG( 10 ), ARG( 11 ),
					 ARG( 12 ), ARG( 13 ) );

	 case 15:
	    return BGL_PROCEDURE_CALL15( proc,
					 ARG( 0 ), ARG( 1 ), ARG( 2 ),
					 ARG( 3 ), ARG( 4 ), ARG( 5 ),
					 ARG( 6 ), ARG( 7 ), ARG( 8 ),
					 ARG( 9 ), ARG( 10 ), ARG( 11 ),
					 ARG( 12 ), ARG( 13 ), ARG( 14 ) );

	 case 16:
	    return BGL_PROCEDURE_CALL16( proc,
					 ARG( 0 ), ARG( 1 ), ARG( 2 ),
					 ARG( 3 ), ARG( 4 ), ARG( 5 ),
					 ARG( 6 ), ARG( 7 ), ARG( 8 ),
					 ARG( 9 ), ARG( 10 ), ARG( 11 ),
					 ARG( 12 ), ARG( 13 ), ARG( 14 ),
					 ARG( 15 ) );

	 default: {
	    obj_t lst = BNIL;
	    int i;
	 
	    for( i = argc - 1; i >= 0; i-- ) {
	       lst = MAKE_PAIR( ARG( i ), lst );
	    }

	    return apply( proc, lst );
	 }
      }
   }
   else {
      return C_SYSTEM_FAILURE( BGL_ERROR,
			       "sqlite",
			       "Illegal number of argument",
			       proc );
   }
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    sqlite_callback_eval ...                                         */
/*---------------------------------------------------------------------*/
static int
sqlite_callback_eval( obj_t *res, int argc, char **argv, char **colname ) {
   if( argc != 0 ) {
      res[ 1 ] = sqlite_apply( res[ 0 ], argc, argv );
   }

   return 0;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_sqlite_eval ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_sqlite_eval( sqlite3 *db, obj_t proc, char *str, obj_t odb ) {
  char *msg;
  obj_t result[] = { proc, BFALSE };
  int rc;

  rc = sqlite3_exec( db, str, (int (*)())sqlite_callback_eval, result, &msg );

  if( rc != SQLITE_OK ) {
     char *buf = (char *)alloca( strlen( str ) + 16 );
     
     sprintf( buf, "sqlite-eval:%s", str );
     sqlite3_free( msg );
     
     if( rc == SQLITE_LOCKED || rc == SQLITE_BUSY ) {
	C_SYSTEM_FAILURE( BGL_IO_TIMEOUT_ERROR, buf, msg, odb );
     } else {
	C_SYSTEM_FAILURE( BGL_ERROR, buf, msg, odb );
     }
  }
  
  return result[ 1 ];
}
   
/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    sqlite_callback_map ...                                          */
/*---------------------------------------------------------------------*/
static int
sqlite_callback_map( obj_t *res, int argc, char **argv, char **colname ) {
   res[ 1 ] = MAKE_PAIR( sqlite_apply( res[ 0 ], argc, argv ), res[ 1 ] );

   return 0;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_sqlite_map ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_sqlite_map( sqlite3 *db, obj_t proc, char *str, obj_t odb ) {
  char *msg;
  obj_t result[] = { proc, BNIL };
  int rc;

  rc = sqlite3_exec( db, str, (int (*)())sqlite_callback_map, result, &msg );

  if( rc != SQLITE_OK ) {
     char *buf = (char *)alloca( strlen( str ) + 16 );
     
     sprintf( buf, "sqlite-map:%s", str );
     sqlite3_free( msg );

     if( rc == SQLITE_LOCKED || rc == SQLITE_BUSY ) {
	C_SYSTEM_FAILURE( BGL_IO_TIMEOUT_ERROR, buf, msg, odb );
     } else {
	C_SYSTEM_FAILURE( BGL_ERROR, buf, msg, odb );
     }
  }
  
  return bgl_reverse_bang( result[ 1 ] );
}
