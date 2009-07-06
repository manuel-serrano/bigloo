/*=====================================================================*/
/*    serrano/prgm/project/bigloo/tools/if_mach.c                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Aug  8 08:17:21 1995                          */
/*    Last change :  Tue Oct 11 15:25:03 2005 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Execute a command on specific architecture.                      */
/*=====================================================================*/
#include <stdio.h>
#include <stdlib.h>

/*---------------------------------------------------------------------*/
/*    Sparc ...                                                        */
/*---------------------------------------------------------------------*/
#if( defined( sparc ) )
#   define MACH_TYPE "sparc"
#   include <errno.h>
#   if( defined( ECHRNG ) )
#      define OS_TYPE "sunos5"
#   else
#      define OS_TYPE "sunos4"
#   endif
#endif

/*---------------------------------------------------------------------*/
/*    Alpha                                                            */
/*---------------------------------------------------------------------*/
#if( defined( __alpha ) )
#  define MACH_TYPE "alpha"
#  if( defined( __osf__ ) )
#     define OS_TYPE "osf"
#  endif
#endif

#if( !defined( MACH_TYPE ) )
#   define MACH_TYPE ""
#endif

#if( !defined( OS_TYPE ) )
#   define OS_TYPE ""
#endif

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    usage ...                                                        */
/*---------------------------------------------------------------------*/
void
usage()
{
   fputs( "Usage: if_mach <mach-type> <os-type> <command>\n", stderr );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
int
main( int argc, char **argv )
{
   if( argc < 4 )
   {
      usage();
      exit( -1 );
   }
   else
   {
      if( strcmp( MACH_TYPE, argv[1] ) )
         exit( 0 );
      else
      if( strcmp( OS_TYPE, "" ) &&
	  strcmp( argv[2], "" ) &&
	  strcmp( OS_TYPE, argv[2] ) )
         exit( 0 );
      else
      {
	 execvp( argv[3], argv+3 );
	 perror( "Couldn't execute" );
      }
   }
}

