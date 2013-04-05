/*=====================================================================*/
/*    .../project/bigloo/runtime/Jlib/datagram_client_socket.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Fri Apr  5 11:03:06 2013 (serrano)                */
/*    Copyright   :  2000-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Datagram Client Socket implementation for the JVM back-end.  */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    DATAGRAM_CLIENT_SOCKET ...                                       */
/*---------------------------------------------------------------------*/
public class datagram_client_socket extends datagram_socket {
   // public fields
   public InetAddress ip;
   output_datagram_port output;
   
   // constructors
   public datagram_client_socket() {
      super();
   }
   
   public datagram_client_socket( final byte[] hostname,
				  final int port,
				  final boolean broadcast ) {
      super();

      try {
	 ip = InetAddress.getByName( new String( hostname ) );
	 
	 socket = new DatagramSocket();
	 socket.setBroadcast( broadcast );

      } catch (final UnknownHostException e) {
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    foreign.BGL_IO_UNKNOWN_HOST_ERROR,
	    "make-datagram-client-socket".getBytes(),
	    e.getMessage().getBytes(),
	    hostname );
      } catch (final IOException e) {
	 foreign.fail( "make-datagram-client-socket",
		       e.getMessage(),
		       hostname );
      }
      
      output = new output_datagram_port( this, hostname, port );
   }

   // public methods
   public Object close() {
      output.close();
      super.close();

      return bigloo.foreign.BUNSPEC;
   }
   
   public obj PORT() {
      return output;
   }
}
