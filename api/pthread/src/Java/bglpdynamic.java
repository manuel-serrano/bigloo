/*=====================================================================*/
/*    .../project/bigloo/api/pthread/src/Java/bglpdynamic.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Oct 19 10:42:04 2002                          */
/*    Last change :  Wed Oct 19 11:07:29 2005 (serrano)                */
/*    Copyright   :  2002-05 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Pthread global dynamic environments.                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.pthread;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    bglpdynamic                                                      */
/*---------------------------------------------------------------------*/
public class bglpdynamic extends bigloo.bgldynamic {
   protected static void setup() {
      bigloo.bgldynamic.abgldynamic = new bglpdynamic();
   }

   public bgldynamic get() {
      Thread t = java.lang.Thread.currentThread();

      if( t instanceof bglpthread ) {
	 return ((bglpthread)t).env;
      } else {
	 return bigloo.bgldynamic.current_dynamic_env;
      }
   }
}
