/*=====================================================================*/
/*    serrano/prgm/project/bigloo/examples/Jawt/Utils.java             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun May 13 10:50:26 2001                          */
/*    Last change :  Sat Jul  7 12:00:20 2001 (serrano)                */
/*    Copyright   :  2001 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Utilities for the Awt example                                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    Utils ...                                                        */
/*---------------------------------------------------------------------*/
public class Utils extends MouseAdapter {
    bigloo.procedure proc;

    Utils( procedure p ) {
	super();
	proc = p;
    }

    public static String bstring_to_jstring( byte[] str ) {
	return new String( str );
    }

    public static byte[] jstring_to_bstring( String str ) {
	if( str == null ) {
	    return new byte[ 0 ];
	} else {
	    return str.getBytes();
	}
    }

    public void mouseClicked( MouseEvent e ) {
	proc.funcall0();
    }
}
