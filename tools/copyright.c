/*=====================================================================*/
/*    serrano/prgm/project/bigloo/tools/copyright.c                    */
/*    -------------------------------------------------------------    */
/*    Author      :  SERRANO Manuel                                    */
/*    Creation    :  Thu Feb 20 16:40:37 1997                          */
/*    Last change :  Thu Jan 14 08:00:38 1999 (serrano)                */
/*    -------------------------------------------------------------    */
/*    This program copyrights any kind of file regarding there suffix. */
/*=====================================================================*/
#include <stdio.h>
#include <string.h>

/*---------------------------------------------------------------------*/
/*    extern declaration                                               */
/*---------------------------------------------------------------------*/
extern char *license();
extern char *malloc();

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    suffix ...                                                       */
/*---------------------------------------------------------------------*/
char *
suffix( char *name )
{
   int i;

   for( i = strlen( name );
	(i > 0) && (name[ i ] != '.') && (name[ i ] != '/');
	i-- );

   if( i == 0 )
      return name;
   else
      return &name[ i + 1 ];
}

/*---------------------------------------------------------------------*/
/*    language specific variables                                      */
/*---------------------------------------------------------------------*/
char  c_prefix  = ';';
char  c_suffix  = '/';
char *c_prelude = "";

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    set_language_specific_comment ...                                */
/*---------------------------------------------------------------------*/
void
set_language_specific_comment( char *suf )
{
   if( !strcmp( suf, "scm" ) )
   {
      c_prefix = ';';
      c_suffix = '/';
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "c" ) )
   {
      c_prefix = '/';
      c_suffix = '/';
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "sch" ) )
   {
      c_prefix = ';';
      c_suffix = '/';
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "ml" ) )
   {
      c_prefix = '(';
      c_suffix = ')';
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "h" ) )
   {
      c_prefix = '/';
      c_suffix = '/';
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "s" ) )
   {
      c_prefix = 0;
      c_suffix = 0;
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "S" ) )
   {
      c_prefix = 0;
      c_suffix = 0;
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "ps" ) )
   {
      c_prefix = '%';
      c_suffix = '/';
      c_prelude = "%!PS\n";
      return;
   }

   if( !strcmp( suf, "sh" ) )
   {
      c_prefix = '#';
      c_suffix = '/';
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "el" ) )
   {
      c_prefix = ';';
      c_suffix = '/';
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "Makefile" ) )
   {
      c_prefix = '#';
      c_suffix = '/';
      c_prelude = "";
      return;
   }

   if( !strcmp( suf, "README" ) )
   {
      c_prefix = '#';
      c_suffix = '/';
      c_prelude = "";
      return;
   }

   c_prefix = c_suffix = 0;
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    string_for_read ...                                              */
/*---------------------------------------------------------------------*/
char *
string_for_read( char *str ) {
   int len = strlen( str );
   int r, w;
   char *new;

   new = malloc( len * 2 );

   for( r = w = 0; r < len; r++, w++ ) {
      new[ w ] = str[ r ];
      if( str[ r ] == '\\' )
	 new[ ++w ] = str[ r ];
   }

   new[ w ] = '\0';
   
   return new;
}
      
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    print_file ...                                                   */
/*---------------------------------------------------------------------*/
void
print_file( char *file )
{
   FILE *fin;
   int   len = strlen( file );

   if( !(fin = fopen( file, "r" )) )
   {
      fprintf( stderr, "can't open file %s for input\n", file );
      exit( -2 );
   }
   else
   {
#if( !defined( _SBFSIZ ) )
#   define _SBFSIZ 8
#endif
#   undef  BUFSIZE      
#   define BUFSIZE (BUFSIZ * _SBFSIZ)
      
      char buffer[ BUFSIZE ];
      char tname[ len + 20 ];
      FILE *fout;
      int din = fileno( fin );
      int dout;
      int nbr;

      sprintf( tname, "%s._copyright", file );
      
      if( !(fout = fopen( tname, "w" )) )
      {
	 fprintf( stderr, "can't open tmp file [%s]\n", tname );
	 exit( -2 );
      }
      
      dout = fileno( fout );

      /* we print the copyright */
      {
	 char pref, suff, *ctext, *runner;
	 int  i = 0; 

	 set_language_specific_comment( suffix( file ) );
   
	 if( c_prefix && c_suffix )
	 {
	    pref = c_prefix;
	    suff = c_suffix;

	    fprintf( fout, "%s", c_prelude );

	    runner = string_for_read( license() );
	    ctext = malloc( strlen( runner ) + 1 );
	    strcpy( ctext, runner );

	    runner = strtok( ctext, "\n" );
	    fprintf( fout, "%c*%s*%c\n", pref, runner, suff );
	 
	    while( runner = strtok( 0, "\n" ) ) {
	       fprintf( fout, "%c*%s*%c\n", pref, runner, suff );
	    }

	    fflush( fout );
	 }
      }

      /* we duplicate the file itself */
      while( (nbr = read( din, buffer, BUFSIZE )) == BUFSIZE )
	 write( dout, buffer, nbr );

      write( dout, buffer, nbr );
      
      fclose( fin );
      fclose( fout );

      remove( file );
      rename( tname, file );
      remove( tname );
   }
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    usage ...                                                        */
/*---------------------------------------------------------------------*/
void
usage()
{
   puts( "usage: copyright <in-file>" );
}
	    
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
int
main( int argc, char *argv[] )
{
   if( argc < 2 )
   {
      usage();
      
      fprintf( stderr, "Incorrect number of arguments\n" );
      exit( -1 );
   }
   else
   {
      int i;

      for( i = 1; i < argc; i++ )
	 print_file( argv[ i ] );
   }

   return 0;
}
