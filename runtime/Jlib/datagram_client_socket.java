/*=====================================================================*/
/*    .../bigloo/5.0.x/runtime/Jlib/datagram_client_socket.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Sat Jun 20 12:04:01 2026 (serrano)                */
/*    Copyright   :  2000-26 Manuel Serrano                            */
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
   
   public datagram_client_socket(final byte[] hostname,
				 final int port,
				 final boolean broadcast,
				 final symbol family) {
      super();

      try {
	 // FIXME
	 // java provides no way to force the address family
	 // at runtime, so we ignore for now.
	 ip = InetAddress.getByName(new String(hostname));
	 
	 socket = new DatagramSocket();
	 socket.setBroadcast(broadcast);

      } catch (final UnknownHostException e) {
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    foreign.BGL_IO_UNKNOWN_HOST_ERROR,
	    "make-datagram-client-socket".getBytes(),
	    e.getMessage().getBytes(),
	    hostname);
      } catch (final IOException e) {
	 foreign.fail("make-datagram-client-socket",
		      e.getMessage(),
		      hostname);
      }
      
      output = new output_datagram_port(this, hostname, port);
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

   public Object HOSTNAME() {
      return ip.getHostName().getBytes();
   }

   public Object HOSTIP() {
      if (socket.isConnected()) {
	 return ip.getHostAddress().getBytes();
      } else {
	 return foreign.BUNSPEC;
      }
   }
}
