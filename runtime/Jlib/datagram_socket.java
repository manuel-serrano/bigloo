/*=====================================================================*/
/*    .../prgm/project/bigloo/runtime/Jlib/datagram_socket.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Apr 30 06:45:59 2011                          */
/*    Last change :  Mon Oct 24 13:45:40 2016 (serrano)                */
/*    Copyright   :  2011-16 Manuel Serrano                            */
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
   public input_datagram_port input;
   
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

   public int PORTNUM() {
      return socket.getPort();
   }
   
   public obj PORT() {
      foreign.fail( "datagram-socket-output-port",
		    "not a datagram-client socket",
		    this );
      return output;
   }
   
   public input_port INPUT_PORT() {
      foreign.fail( "datagram-socket-input-port",
		    "not a datagram-client socket",
		    this );
      return input;
   }
   
   public Object close() {
      try {
	 socket.close();
      } catch( Throwable _t ) {
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
   
   public int send( byte[] string, byte[] host, int port )
      throws IOException {
      DatagramPacket p;
      InetAddress a[];

      a = InetAddress.getAllByName( new String( host ) );

      p = new DatagramPacket( string, string.length );
      p.setAddress( a[0] );
      p.setPort( port );

      socket.send( p );

      return string.length;
   }
   
   public Object getsockopt( keyword se ) throws IOException {
      return foreign.BUNSPEC;
   }
   
   public Object setsockopt( keyword se, Object val ) throws IOException {
      return foreign.BFALSE;
   }
      
}
