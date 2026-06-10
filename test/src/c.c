/*=====================================================================*/
/*    serrano/bigloo/5.0.x/test/src/c.c                                */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Mon Jun  8 17:58:46 2026                          */
/*    Last change :  Tue Jun  9 08:06:24 2026 (serrano)                */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Basic C file for testing extern "C" interface.                   */
/*=====================================================================*/
#include "extern_c.h"
#include "c.h"

long count = 10;
long init = -1;
   
int inc(int x) {
   return x+1;
}
   
   
