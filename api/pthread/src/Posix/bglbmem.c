/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/pthread/src/Posix/bglbmem.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 21 15:57:25 2004                          */
/*    Last change :  Fri Dec 21 09:34:12 2012 (serrano)                */
/*    Copyright   :  2004-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The pthread bmem profiling setup                                 */
/*=====================================================================*/
#include <pthread.h>
#include <sched.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*---------------------------------------------------------------------*/
/*    Bmem declaration                                                 */
/*---------------------------------------------------------------------*/
void *bglpth_pthread_getspecific( pthread_key_t key ) {
   return pthread_getspecific( key );
}

int bglpth_pthread_setspecific( pthread_key_t key, void *value ) {
   return pthread_setspecific( key, value );
}

int bglpth_pthread_key_create( pthread_key_t *key, void *fun ) {
   return pthread_key_create( key, (void (*)(void *))fun );
}

int bglpth_pthread_mutex_init( pthread_mutex_t *mutex, void *val ) {
   return pthread_mutex_init( mutex, val );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_bmem ...                                            */
/*---------------------------------------------------------------------*/
void
bglpth_setup_bmem() {
   /* left empty, a mere bmem hook */
}

