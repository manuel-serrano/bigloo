/*=====================================================================*/
/*    .../project/bigloo/runtime/Jlib/datagram_server_socket.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Fri Apr  5 11:05:16 2013 (serrano)                */
/*    Copyright   :  2000-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Datagram Server Socket implementation for the JVM back-end.  */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    DATAGRAM_SERVER_SOCKET ...                                       */
/*---------------------------------------------------------------------*/
public class datagram_server_socket extends datagram_socket {
   // constructors
   public datagram_server_socket() {
      super();
   }
   
   public datagram_server_socket( final int port ) {
      super();

      try {
	 socket = new DatagramSocket( port );
	 input = new input_datagram_port( this, port );
      } catch( final IOException e ) {
	 foreign.fail( "make-datagram-server-socket",
		       "cannot create socket",
		       foreign.BINT( port ) );
      }
   }

   public datagram_server_socket( final symbol family ) {
      super();

      try {
	 // FIXME: The address family cannot be specified.
	 socket = new DatagramSocket( null );
      } catch( final IOException e ) {
	 foreign.fail( "make-datagram-server-unbound-socket",
		       "cannot create socket",
		       family );
      }
   }

   public Object receive( int sz ) {
      DatagramPacket packet = new DatagramPacket( new byte[ sz ], sz );
      bgldynamic env = foreign.BGL_CURRENT_DYNAMIC_ENV();

      try {
	 socket.receive( packet );
      } catch( Exception e ) {
	 foreign.fail( "receive", e, this );
      }

      env.mvalues_number = 2;
	 
      env.mvalues_values[ 1 ] = packet.getAddress();

      int len = packet.getLength();
      final byte[] tmp = new byte[ len ];

      System.arraycopy( packet.getData(), 0, tmp, 0, len );
      return tmp;
   }
   
   public obj PORT() {
      return input;
   }
}
    
