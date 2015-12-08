/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/JDK14.java              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Mar 11 08:51:04 2008                          */
/*    Last change :  Tue Dec  8 08:04:36 2015 (serrano)                */
/*    Copyright   :  2008-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JDK 1.4 specifics                                                */
/*=====================================================================*/

package bigloo;

import java.lang.*;
import java.net.*;
import java.io.*;
import java.nio.channels.*;
import java.lang.reflect.*;

public class JDK14 extends JDK {

   // JDK1.4 methods
   
   public Method getDeclaredMethodImpl( Class c, byte[] m ) throws Exception {
      return c.getDeclaredMethod( new String( m ), null );
   }
   
   public Object invokeImpl( Method m ) throws Exception {
      return m.invoke( null, null );
   }
   
   public Object getExceptionCauseImpl(Throwable v) {
      if( v.getCause() != null && v.getCause() != v )
	 return v.getCause().getMessage().getBytes();
      else
	 return unspecified.unspecified; 
   }
   
    public Object invoke3Impl( Method m, int n, byte[] a ) throws Exception {
      Object[] args = { new Integer( n ), a };
      return m.invoke( null, args );
   }
   
   public ServerSocket makeServerSocketImpl(String name, int port)
      throws IOException {

      InetSocketAddress addr;
      if(name != null)
	 addr = new InetSocketAddress(name, port);
      else
	 addr = new InetSocketAddress(port);

      ServerSocketChannel sch = ServerSocketChannel.open();
      ServerSocket sock = sch.socket();
      sock.bind(addr);
      return sock;
   }

   public Socket acceptImpl(ServerSocket sock, boolean blocking)
      throws IOException {
      ServerSocketChannel ssch = sock.getChannel();
      ssch.configureBlocking(blocking);
      SocketChannel sch = ssch.accept();
      if(sch != null)
	 return sch.socket();
      return null;
   }

   public boolean truncateImp(FileOutputStream file, long size) {
      return false;
   }
}
