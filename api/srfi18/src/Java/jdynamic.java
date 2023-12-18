/*=====================================================================*/
/*    /tmp/BGL/bigloo-unstable/api/srfi18/src/Java/jdynamic.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Oct 19 10:42:04 2002                          */
/*    Last change :  Mon Dec 18 10:11:03 2023 (serrano)                */
/*    Copyright   :  2002-23 Manuel Serrano                            */
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

   public jdynamic() {
      super();
   }
    
   public jdynamic (final bgldynamic o) {
      super(o);
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
