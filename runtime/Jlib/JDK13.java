/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/JDK13.java              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Mar 11 08:50:52 2008                          */
/*    Last change :  Tue Dec  8 08:04:20 2015 (serrano)                */
/*    Copyright   :  2008-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JDK 1.3 specifics                                                */
/*=====================================================================*/

package bigloo;

import java.lang.*;
import java.net.*;
import java.io.*;
import java.nio.channels.*;
import java.lang.reflect.*;

public class JDK13 extends JDK {

   // JDK1.3 methods
   
   public Method getDeclaredMethodImpl( Class c, byte[] m ) throws Exception {
      return c.getDeclaredMethod( new String( m ), null );
   }
   
   public Object invokeImpl( Method m ) throws Exception {
      return m.invoke( null, null );
   }

   public Object getExceptionCauseImpl(Throwable v) {
      return unspecified.unspecified;
   }
   
   public Object invoke3Impl( Method m, int n, byte[] a ) throws Exception {
      Object[] args = { new Integer( n ), a };
      return m.invoke( null, args );
   }
   
   public ServerSocket makeServerSocketImpl(String name, int port)
      throws IOException  {

      if(name == null)
	 return new ServerSocket( port );
      else{
	 // the default backlog in Java is 50 (per javadocs)
	 InetAddress host = InetAddress.getByName(name);
	 return new ServerSocket( port, 50, host);
      }
   }

   public Socket acceptImpl(ServerSocket sock, boolean blocking)
      throws IOException {
      return sock.accept();
   }

   public boolean truncateImpl(FileOutputStream file, long size) {
      return false;
   }
}
