/*=====================================================================*/
/*    serrano/prgm/project/bigloo/tools/libcversion.c                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar 26 10:21:20 1998                          */
/*    Last change :  Thu Mar 26 10:47:10 1998 (serrano)                */
/*    -------------------------------------------------------------    */
/*    This program returns the full name of a symbolic link.           */
/*=====================================================================*/
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    basename ...                                                     */
/*---------------------------------------------------------------------*/
char *
basename( char *name ) {
   long i;

   for( i = strlen( name ) - 1; (name[ i ] != '/') && (i > 0); i-- );

   if( i == 0 )
      return "";
   else {
      name[ i ] = 0;
      return name;
   }
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    filename ...                                                     */
/*---------------------------------------------------------------------*/
char *
filename( char *name ) {
   long i;

   for( i = strlen( name ) - 1; (name[ i ] != '/') && (i > 0); i-- );

   if( i == 0 )
      return name;
   else
      return &name[ i + 1 ];
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    numerical_suffix ...                                             */
/*---------------------------------------------------------------------*/
char *
numerical_suffix( char *name ) {
   long i, len;

   len = strlen( name );
   i = 0;
   
   while( i < (len + 1) ) {
      if( (name[ i ] == '.') && isdigit( name[ i + 1 ] ) ) {
	 char *suffix = &name[ i + 1 ];
	 name = suffix;

	 while( isdigit( *name++ ) );
	 *(name - 1) = 0;

	 return suffix;
      } else i++;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    lnname ...                                                       */
/*---------------------------------------------------------------------*/
void
lnname( char *name ) {
   struct stat stat;

   if( lstat( name, &stat ) == -1 ) {
      puts( "1" );
   } else {
      if( !S_ISLNK( stat.st_mode ) ) {
	 char *fname, *suffix;

	 fname = filename( name );
	 suffix = numerical_suffix( fname );
	 
	 puts( suffix );
      } else {
#define BUFSIZE 10240      
	 char buf[ BUFSIZE ];
	 char *new_name;
	 char *bname;
	 int nb;
	 
	 nb = readlink( name, buf, BUFSIZE );
	 buf[ nb ] = 0;
	 new_name = alloca( strlen( name ) + nb + 1 );
	 bname = basename( name );

	 sprintf( new_name, "%s/%s", bname, buf );
	 lnname( new_name );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
int
main( int argc, char *argv[] ) {
   lnname( argv[ 1 ] );
   return 1;
}
