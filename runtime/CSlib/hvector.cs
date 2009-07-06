/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/hvector.cs             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  8 11:02:46 2006                          */
/*    Last change :  Wed Nov  8 18:33:07 2006 (serrano)                */
/*    Copyright   :  2006 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Homogenous (SRFI-4) vectors.                                     */
/*=====================================================================*/
using System;

namespace bigloo
{
   public abstract class hvector: obj {
      public int len;
      public abstract int ident();
   }
}
