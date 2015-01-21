/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/socket.java             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Tue Jan 20 19:37:35 2015 (serrano)                */
/*    Copyright   :  2000-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Socket implementation for the JVM back-end.                  */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    SOCKET ...                                                       */
/*---------------------------------------------------------------------*/
public abstract class socket extends obj {
   /*--- static protect variables -------------------------------------*/
   protected static keyword tcp_nodelay = keyword.make_keyword( "TCP_NODELAY" );
   protected static keyword so_keepalive = keyword.make_keyword( "SO_KEEPALIVE" );
   protected static keyword so_oobinline = keyword.make_keyword( "SO_OOBINLINE" );
   protected static keyword so_rcvbuf = keyword.make_keyword( "SO_RCVBUF" );
   protected static keyword so_sndbuf = keyword.make_keyword( "SO_SNDBUF" );
   protected static keyword so_reuseaddr = keyword.make_keyword( "SO_REUSEADDR" );
   protected static keyword so_timeout = keyword.make_keyword( "SO_TIMEOUT" );
   
   /*--- public fields ------------------------------------------------*/
   public obj input;
   public obj output;

   /*--- protected fields ---------------------------------------------*/
   protected boolean down = false;
   protected boolean closed = false;

   /*--- constructors -------------------------------------------------*/
   protected socket() {
      input = bigloo.foreign.BFALSE;
      output = bigloo.foreign.BFALSE;
   }

   /*--- protected methods --------------------------------------------*/
   protected static void socket_error( String s1, String s2, Object o ) {
      foreign.fail( s1, s2, o );
   }

   protected void set_socket_io_ports( Socket client_socket,
				       byte[] inbuf,
				       byte[] outbuf,
				       byte[] name )
      throws IOException {
      input = new input_socket_port( client_socket, inbuf );
      output = output_buffered_port.make_output_buffered_port( client_socket.getOutputStream(), outbuf, name );
   }

   protected void set_socket_io_ports( Socket client_socket,
				       byte[] inbuf,
				       byte[] outbuf )
      throws IOException {
      byte[] name = client_socket.getClass().getName().getBytes();
      set_socket_io_ports( client_socket, inbuf, outbuf, name );
   }

   /*--- public methods -----------------------------------------------*/
   public boolean DOWNP() {
      return down;
   }

   public Object close() {
      if (!closed) {
	 closed= true;
	 if (input instanceof input_port)
	    ((input_port)input).close();
	 if (output instanceof output_port)
	    ((output_port)output).close();
      }

      return bigloo.foreign.BUNSPEC;
   }

   public Object getsockopt( keyword se ) throws IOException {
      return foreign.BUNSPEC;
   }
   
   public Object setsockopt( keyword se, Object val ) throws IOException {
      return foreign.BFALSE;
   }
      
   /*--- public abstract methods --------------------------------------*/
   public abstract Object HOSTNAME();
   public abstract Object HOSTIP();
   public abstract int shutdown( int how );
   public abstract int PORT();
   public abstract byte[] local_addr();
}
