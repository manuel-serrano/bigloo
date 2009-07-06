/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/hvector.java            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Nov  8 11:02:46 2006                          */
/*    Last change :  Wed Nov  8 15:42:30 2006 (serrano)                */
/*    Copyright   :  2006 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Homogenous (SRFI-4) vectors.                                     */
/*=====================================================================*/
package bigloo;

public abstract class hvector extends obj {
   int len;
   public abstract int ident();
}
