/*=====================================================================*/
/*    .../project/bigloo/runtime/Jlib/input_datagram_port.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 11:53:13 2000                          */
/*    Last change :  Mon Oct 24 13:45:50 2016 (serrano)                */
/*    Copyright   :  2000-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JVM Socket input ports implementation.                           */
/*=====================================================================*/
package bigloo;
import bigloo.foreign;
import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    INPUT_DATAGRAM_PORT                                              */
/*---------------------------------------------------------------------*/
public class input_datagram_port extends input_port {
   private int port;
   private datagram_server_socket socket;

   public input_datagram_port( final datagram_server_socket s,
			       final int p ) {
      super( "[datagram-socket]", null );
      socket = s;
      port = p;
   }

   public void close() {
      eof = true;
      other_eof = true;
      buffer = null;
      super.close();
   }

   public boolean rgc_charready() {
      return true;
   }

   public boolean rgc_fill_buffer() throws IOException {
      final int bufsize = this.buffer.length;
      int bufpose = this.bufpos;
      final int matchstart = this.matchstart;

      // if the buffer is not full, we fill it */
      if (bufpose < bufsize)
	 return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );

      if (0 < matchstart) {
	 // we shift the buffer left and we fill the buffer */
	 final byte[] buffer = this.buffer;
	 final int movesize = bufpose-matchstart;

	 for ( int i = 0 ; i < movesize ; ++i )
	    buffer[i] = buffer[matchstart+i];

	 bufpose -= matchstart;
	 this.matchstart = 0;
	 this.matchstop -= matchstart;
	 this.forward -= matchstart;
	 this.lastchar = buffer[matchstart-1];

	 return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );
      }

      // we current token is too large for the buffer */
      // we have to enlarge it.                       */
      rgc_double_buffer();

      return rgc_fill_buffer();
   }

   final boolean rgc_size_fill_con_buffer( int bufpose, final int size )
      throws IOException {
      // we start reading at BUFPOSE - 1 because we have */
      // to remove the '\0' sentinel that ends the buffer */
      final byte[] buffer = this.buffer;

      socket.socket.receive( new DatagramPacket( buffer, bufpose, size ) );
      
      final int nbread = size;

      if (nbread == -1)
	 eof = true;
      else
	 bufpose += nbread;

      this.bufpos = bufpose;
      return (0 < bufpos);
   }

   public Object bgl_input_port_clone( input_port src )
      {
	 super.bgl_input_port_clone( src );
	 socket = ((input_datagram_port)src).socket;

	 return this;
      }
   
   public boolean timeout_set( int to ) {
      try {
	 socket.socket.setSoTimeout( to );
	 return true;
      } catch( Exception _e ) {
	 return false;
      }
   }
}
