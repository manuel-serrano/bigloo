/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/regexp.java             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Dec  7 11:40:40 2011                          */
/*    Last change :  Wed Dec  7 11:48:29 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Regular Expressions                                              */
/*=====================================================================*/

package bigloo;

import java.io.*;

public class regexp extends obj {
   public byte[] pat;
   public Object preg;

   public regexp( byte[] p ) {
      pat = p;
   }

   public void write( final output_port p ) {
      p.write( "#<regexp:" + pat + ">" );
   }
}
 
