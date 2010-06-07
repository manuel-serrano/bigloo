/*=====================================================================*/
/*    serrano/prgm/project/bigloo/examples/Embedded/c-main.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 14 09:04:53 1996                          */
/*    Last change :  Fri Nov 28 09:25:40 2003 (serrano)                */
/*    -------------------------------------------------------------    */
/*    The C main.                                                      */
/*=====================================================================*/

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
