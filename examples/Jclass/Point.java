import java.io.*;
import bigloo.*;

public class Point implements Intf {
    public int x;
    public int y;
    public static int point_num = 0;
    
    public Point( int _x, int _y ) {
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
       System.out.print( "x= " + x + " y= " + y );
    }

    public static int PointStatistics() {
	System.out.println( "<callback: " + main.callback( 10 ) + ">" );
	return point_num;
    }
}
