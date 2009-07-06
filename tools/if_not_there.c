/*=====================================================================*/
/*    serrano/prgm/project/bigloo/tools/if_not_there.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Aug  8 08:43:47 1995                          */
/*    Last change :  Tue Aug  8 09:25:37 1995 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Execute a command line a file doesn't exist (used in             */
/*    conjunction with `if_mach').                                     */
/*=====================================================================*/
#include <stdio.h>

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    usage ...                                                        */
/*---------------------------------------------------------------------*/
void
usage()
{
   puts( "usage: if_not_there <file> <command>" );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
int
main( int argc, char **argv )
{
   FILE * f;
   
   if( argc < 3 )
   {
      usage();
      exit( -1 );
   }
   else
   {
      if( (f = fopen( argv[ 1 ], "rb" )) || (f = fopen( argv[ 1 ], "r" )) )
      {
         fclose( f );
	 exit( 0 );
      }
      else
      {
	 execvp( argv[ 2 ], argv + 2 );
         exit( 0 );
      }
   }
}

