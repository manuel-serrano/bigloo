/*=====================================================================*/
/*    serrano/bigloo/5.0.x/test/src/Point.java                         */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Mon Jun  8 18:15:14 2026                          */
/*    Last change :  Fri Jun 12 08:28:44 2026 (serrano)                */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    External jvm companion                                           */
/*=====================================================================*/

public class Point implements Intf {
   public int x;
   public int y;
   public static int point_num = 0;

   public Point(int _x, int _y) {
      point_num++;
      x = _x;
      y = _y;
   }

   public Point() {
      point_num++;
      x = 1;
      y = 1;
   }

   public int abstract_met() {
      return 1;
   }

   public String toString() {
      return "<x=" + x + " y=" + y + ">";
   }

   public static int statistics() {
      return point_num + extern_jvm.callback(1000);
   }
}
