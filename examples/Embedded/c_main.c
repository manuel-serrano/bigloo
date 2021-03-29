/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/examples/Embedded/c_main.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 14 09:04:53 1996                          */
/*    Last change :  Mon Mar 29 09:46:13 2021 (serrano)                */
/*    -------------------------------------------------------------    */
/*    The C main.                                                      */
/*=====================================================================*/
#include <stdio.h>
extern int bigloo_initialize( int, char **, char ** );

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
int
main( int argc, char *argv[], char *env[] )
{
   puts( "Before Bigloo init" );
   bigloo_initialize( argc, argv, env );
   puts( "After Bigloo stuff" );
   return 0;
}
