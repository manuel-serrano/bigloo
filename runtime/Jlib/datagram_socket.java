/*=====================================================================*/
/*    .../prgm/project/bigloo/runtime/Jlib/datagram_socket.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Apr 30 06:45:59 2011                          */
/*    Last change :  Wed Feb 20 10:37:47 2013 (serrano)                */
/*    Copyright   :  2011-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Datagram socket implementation of the JVM back-end.          */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.net.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    DATAGRAM_SOCKET                                                  */
/*---------------------------------------------------------------------*/
public abstract class datagram_socket extends obj {
   // public fields
   public DatagramSocket socket;
   public output_datagram_port output;
   
   // constructor
   protected datagram_socket() {
   }

   // abstract methods
   public Object HOSTNAME() {
      return socket.getInetAddress().getHostName().getBytes();
   }

   public Object HOSTIP() {
      return socket.getInetAddress().getHostAddress().getBytes();
   }

   public int PORT() {
      return socket.getPort();
   }
   
   public output_port OUTPUT_PORT() {
      foreign.fail( "datagram-socket-output-port",
		    "not a datagram-client socket",
		    this );
      return output;
   }
   
   public Object close() {
      try {
	 socket.close();
      } catch( Throwable _ ) {
	 ;
      }

      return bigloo.foreign.BUNSPEC;
   }
   
   public void write( final output_port  p ) {
      Object hostname = HOSTNAME();

      if( hostname instanceof byte[] )
	 hostname = new String( (byte[])hostname );

      p.write( "#<datagram-socket:" + hostname.toString() + "." + PORT() + ">" );
   }
   
   public Object receive( int len ) {
      return foreign.fail( "receive", "not a datagram-server socket", this );
   }
   
   public Object send( byte[] string, byte[] host, int port ) {
      return foreign.fail( "send", "Not implemented yet", this );
   }
   
   public Object getsockopt( keyword se ) throws IOException {
      return foreign.BUNSPEC;
   }
   
   public Object setsockopt( keyword se, Object val ) throws IOException {
      return foreign.BFALSE;
   }
      
}
