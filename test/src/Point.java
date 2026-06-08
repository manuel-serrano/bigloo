/*=====================================================================*/
/*    serrano/bigloo/5.0.x/test/src/Point.java                         */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Mon Jun  8 18:15:14 2026                          */
/*    Last change :  Mon Jun  8 18:16:01 2026 (serrano)                */
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

    public int abstract_method() {
        return 1;
    }

    public void show() {
	System.out.print("x= " + x + " y= " + y);
    }

    public static int PointStatistics() {
	return point_num + external_jvm.callback(10);
    }
}
