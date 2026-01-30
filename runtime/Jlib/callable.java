/*=====================================================================*/
/*    serrano/prgm/project/bigloo/wasm/runtime/Jlib/callable.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jan 28 13:49:14 2026                          */
/*    Last change :  Wed Jan 28 13:51:17 2026 (serrano)                */
/*    Copyright   :  2026 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Interface for callable objects (i.e., procedure)                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo;

/*---------------------------------------------------------------------*/
/*    callable                                                         */
/*---------------------------------------------------------------------*/
public interface callable {
   abstract public Object apply(Object args);
   abstract public Object funcall0();
   abstract public Object funcall1(Object a1);
   abstract public Object funcall2(Object a1, Object a2);
   abstract public Object funcall3(Object a1, Object a2, Object a3);
   abstract public Object funcall4(Object a1, Object a2, Object a3, Object a4);
}
