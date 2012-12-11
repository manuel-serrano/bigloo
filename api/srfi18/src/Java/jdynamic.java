/*=====================================================================*/
/*    .../prgm/project/bigloo/api/srfi18/src/Java/jdynamic.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Oct 19 10:42:04 2002                          */
/*    Last change :  Tue Dec 11 18:16:07 2012 (serrano)                */
/*    Copyright   :  2002-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Pthread global dynamic environments.                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.srfi18;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    jdynamic                                                         */
/*---------------------------------------------------------------------*/
public class jdynamic extends bigloo.bgldynamic {
   protected static void setup() {
      bigloo.bgldynamic.abgldynamic = new jdynamic();
   }

   public bgldynamic get() {
      Thread t = java.lang.Thread.currentThread();

      if( t instanceof jthread ) {
	 return ((jthread)t).env;
      } else {
	 return bigloo.bgldynamic.current_dynamic_env;
      }
   }
}
