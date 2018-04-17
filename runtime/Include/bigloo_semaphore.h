/*=====================================================================*/
/*    .../project/bigloo/bigloo/runtime/Include/bigloo_semaphore.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 08:05:01 2016                          */
/*    Last change :  Tue Apr 17 07:49:20 2018 (serrano)                */
/*    Copyright   :  2016-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo SEMAPHOREs                                                */
/*=====================================================================*/
#ifndef BIGLOO_SEMAPHORE_H 
#define BIGLOO_SEMAPHORE_H

#if BGL_HAVE_SEMAPHORE
#   include <semaphore.h>
#   include <unistd.h>
#   include <fcntl.h>
#endif

/*---------------------------------------------------------------------*/
/*    Does someone really wants C++ here?                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus_just_for_emacs_indent
}
#endif

/*---------------------------------------------------------------------*/
/*    bgl_semaphore ...                                                */
/*---------------------------------------------------------------------*/
struct bgl_semaphore {
   header_t header;
   union scmobj *name;
#if( BGL_HAVE_SEMAPHORE )
   sem_t *semaphore;
#endif
};

/*---------------------------------------------------------------------*/
/*    API                                                              */
/*---------------------------------------------------------------------*/
#define BGL_SEMAPHORE_SIZE (sizeof( struct bgl_semaphore ))

#define BGL_SEMAPHORE( o ) CREF( o )->semaphore

#define BGL_SEMAPHOREP( o ) (POINTERP( o ) && (TYPE( o ) == SEMAPHORE_TYPE))

#define BGL_SEMAPHORE_NAME( s ) BGL_SEMAPHORE( s ).name
#define BGL_SEMAPHORE_SEM( s ) BGL_SEMAPHORE( s ).semaphore

/*---------------------------------------------------------------------*/
/*    C++                                                              */
/*---------------------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
#endif

