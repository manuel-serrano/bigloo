/*=====================================================================*/
/*    .../project/bigloo/runtime/Jlib/output_datagram_port.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Apr 30 07:02:12 2011                          */
/*    Last change :  Fri Apr  5 10:49:44 2013 (serrano)                */
/*    Copyright   :  2011-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    DatagramSocket output ports.                                     */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    OUTPUT_DATAGRAM_PORT                                             */
/*---------------------------------------------------------------------*/
public class output_datagram_port extends output_port {
   datagram_client_socket socket;
   int port;
   
   public output_datagram_port( final datagram_client_socket s,
				final byte[] hostname,
				final int p ) {
      super();

      port = p;
      String n = new String( hostname ) + ":" + port;
      name = n.getBytes();
      socket = s;
   }

   public Object close() {
      if( chook instanceof procedure ) {
	 ((procedure)chook).funcall1( this );
      }
      return this;
   }

   public Object flush() {
      return bbool.vrai;
   }
   
   public void write( final int cn ) {
      write( "" + cn );
   }

   public void write( final byte[] s ) {
      try {
	 socket.socket.send(
	    new DatagramPacket( s, s.length, socket.ip, port ) );
      } catch( final Exception e ) {
	 foreign.fail( "write", e, this );
      }
   }

   public void write( final byte[] s, int offset, int len ) {
      final byte[] tmp = new byte[ len - offset ];

      System.arraycopy( s, offset, tmp, 0, len );
      write( tmp );
   }

   public void write( final String s ) {
      write( s.getBytes() );
   }
}
