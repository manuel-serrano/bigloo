/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem-ng/lib/wrapper.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  7 19:30:50 2021                          */
/*    Last change :  Thu Oct  7 19:46:20 2021 (serrano)                */
/*    Copyright   :  2021 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Bigloo standard library wrappers                                 */
/*=====================================================================*/
#ifndef WRAPPER_H 
#define WRAPPER_H

#include <bigloo.h>
#include <bmem.h>

extern void bmem_init_wrapper(void *);

extern void *(*____create_vector)(int);
extern void *(*____create_vector_uncollectable)(int);

extern void *(*____make_fx_procedure)(void *(*)(), int, int);
extern void *(*____make_va_procedure)(void *(*)(), int, int);

#endif
