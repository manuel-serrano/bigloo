/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/JDK15.java              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Mar 11 08:51:14 2008                          */
/*    Last change :  Sun May 18 09:37:32 2014 (serrano)                */
/*    Copyright   :  2008-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JDK 1.5 specifics                                                */
/*=====================================================================*/

package bigloo;

import java.lang.*;
import java.net.*;
import java.io.*;
import java.nio.channels.*;
import java.lang.reflect.*;

public class JDK15 extends JDK {
   // JDK1.5 methods
   
   public Method getDeclaredMethodImpl( Class c, byte[] m ) throws Exception {
      return c.getDeclaredMethod( new String( m ) );
   }
   
   public Object invokeImpl( Method m ) throws Exception {
      return m.invoke( null );
   }
   
   public Object getExceptionCauseImpl(Throwable v) {
      if( v.getCause() != null && v.getCause() != v )
	 return v.getCause().getMessage().getBytes();
      else
	 return unspecified.unspecified; 
   }
   
   public Object invoke3Impl(Method m, int n, byte[] a) throws Exception {
      Object[] args = { n, a };
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
   
   public boolean truncateImpl(FileOutputStream stream, long size) {
      try {
	 FileChannel outChan = stream.getChannel();
	 try {
	    outChan.truncate( size );
	    return true;
	 } catch( Exception _ ) {
	    return false;
	 } finally {
	    outChan.close();
	 }
      } catch( Exception _ ) {
	 return false;
      }
   }
}
