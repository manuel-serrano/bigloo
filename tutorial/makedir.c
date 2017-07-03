/*=====================================================================*/
/*    serrano/prgm/project/bigloo/tutorial/makedir.c                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Aug 20 11:13:16 1998                          */
/*    Last change :  Thu Aug 20 16:38:31 1998 (serrano)                */
/*    -------------------------------------------------------------    */
/*    This file implements makedir a function that creates a           */
/*    directory.                                                       */
/*=====================================================================*/
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

/*---------------------------------------------------------------------*/
/*    makedir ...                                                      */
/*---------------------------------------------------------------------*/
int makedir( const char *pathname ) {
   if( access( pathname, R_OK ) )
      return mkdir( pathname, 0777 );
   else
      return 0;
}
	 
