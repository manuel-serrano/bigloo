/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/JDK.java                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Mar 11 08:50:33 2008                          */
/*    Last change :  Mon Oct 24 13:41:19 2016 (serrano)                */
/*    Copyright   :  2008-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Java auto-configuration                                          */
/*=====================================================================*/

package bigloo;

import java.lang.*;
import java.io.*;
import java.io.IOException;
import java.net.*;
import java.lang.reflect.*;

public abstract class JDK {
   private static JDK impl;
   static{
      // are we 1.6?
      try {
	 Class.forName("java.text.spi.BreakIteratorProvider");
	 impl = getImpl("bigloo.JDK16");
      } catch(Exception x) {}

      // are we 1.5?
      if (impl == null)
	 try {
	    Class.forName("java.lang.ProcessBuilder");
	    impl = getImpl("bigloo.JDK15");
	 } catch(Exception x) {}
      
      if (impl == null)
	 // try 1.4
	 try {
	    Class.forName("java.nio.Buffer");
	    impl = getImpl("bigloo.JDK14");
	 } catch(Exception x) {}
    
      // default is 1.3
      if (impl == null)
	 try {
	    impl = getImpl("bigloo.JDK13");
	 } catch(Exception x) {}
   }

   private static JDK getImpl(String classname) {
      try {
	 Class c = Class.forName(classname);
	 return (JDK)c.newInstance();
      } catch (Exception x) {
	 return null;
      }
   }

   // Static methods

   public static Method getDeclaredMethod(Class c, byte[] m)
      throws Exception {
      return impl.getDeclaredMethodImpl(c, m);
   }
   
   public static Object invoke(Method m)
      throws Exception {
      return impl.invokeImpl(m);
   }
   public static Object getExceptionCause(Throwable v) {
      return impl.getExceptionCauseImpl(v);
   }
   
   public static Object invoke3(Method m, int n, byte[] s)
      throws Exception {
      return impl.invoke3Impl(m, n, s);
   }
   
   public static ServerSocket makeServerSocket(String name, int port)
      throws IOException {
      return impl.makeServerSocketImpl(name, port);
   }

   public static Socket accept(ServerSocket sock, boolean blocking)
      throws IOException {
      return impl.acceptImpl(sock, blocking);
   }

   public static byte[] password( byte[] prompt ) {
      return impl.passwordImpl( prompt );
   }

   public abstract Method getDeclaredMethodImpl(Class c, byte[] m)
      throws Exception;
   
   public abstract Object invokeImpl(Method m) throws Exception;
   public abstract Object invoke3Impl(Method m, int n, byte[] a)
      throws Exception;

   public abstract Object getExceptionCauseImpl(Throwable v);
   
   public abstract ServerSocket makeServerSocketImpl(String name, int port) throws IOException;
   public abstract Socket acceptImpl(ServerSocket sock, boolean blocking) throws IOException;
   
   public byte[] passwordImpl( byte[] prompt ) {
      BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
      System.err.print( prompt );
      try {
	 return in.readLine().getBytes();
      } catch( Exception _e ) {
	 return "".getBytes();
      }
   }

   public static boolean truncate( FileOutputStream stream, long size ) {
      return impl.truncate( stream, size );
   }
}
