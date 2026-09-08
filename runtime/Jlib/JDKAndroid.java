/*=====================================================================*/
/*    .../prgm/project/bigloo/5.0.x/runtime/Jlib/JDKAndroid.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Mar 11 08:51:26 2008                          */
/*    Last change :  Mon Sep  7 21:53:29 2026 (serrano)                */
/*    Copyright   :  2008-26 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JDK Android specifics                                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo;

/*---------------------------------------------------------------------*/
/*    JDKAndroid ...                                                   */
/*---------------------------------------------------------------------*/
public class JDKAndroid extends JDK2x {
   public void exitImpl(int n) {
      try {
	 Class<?> processClass = Class.forName("android.os.Process");

	 java.lang.reflect.Method myPid =
	    processClass.getMethod("myPid");

	 java.lang.reflect.Method killProcess =
            processClass.getMethod("killProcess", int.class);

	 int pid = (Integer) myPid.invoke(null);
	 killProcess.invoke(null, pid);
	 System.exit(n);
      } catch (Exception e) {
	 e.printStackTrace();
      }
   }
}
