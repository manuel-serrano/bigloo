/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/srfi18/src/Posix/bglsetup.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 21 15:57:25 2004                          */
/*    Last change :  Mon Dec 10 09:42:27 2012 (serrano)                */
/*    Copyright   :  2004-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The pthread setup                                                */
/*=====================================================================*/
#include <pthread.h>

#define GC_PRIVATE_H
#include <gc.h>
#include <bigloo.h>
#include <signal.h>
#include <srfi18.h>

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern void srfi18_setup_mutex();
extern void srfi18_setup_condvar();

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    srfi18_setup ...                                                 */
/*---------------------------------------------------------------------*/
BGL_EXPORTED_DEF
void
srfi18_setup( int argc, char *argv, char **env ) {
   static int srfi18_init = 0;

   if( !srfi18_init ) {
      srfi18_init = 1;
      
      srfi18_setup_mutex();
      srfi18_setup_condvar();
   }
}
