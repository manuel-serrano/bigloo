/*=====================================================================*/
/*    serrano/prgm/project/bigloo/5.0.x/bde/bglmem/lib/wrapper.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct  7 19:30:50 2021                          */
/*    Last change :  Fri Jul 31 09:34:07 2026 (serrano)                */
/*    Copyright   :  2021-26 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo standard library wrappers                                 */
/*=====================================================================*/
#ifndef WRAPPER_H 
#define WRAPPER_H

#include <bigloo.h>
#include <bglmem.h>

extern void bmem_init_wrapper(void *);

extern void *(*____create_vector)(int);
extern void *(*____create_vector_uncollectable)(int);

extern void *(*____make_fx_procedure)(function_t, int, int);
extern void *(*____make_va_procedure)(function_t, int, int);

#endif
