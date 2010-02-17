/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/server_socket.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Wed Feb 17 16:13:18 2010 (serrano)                */
/*    Copyright   :  2000-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Server Socket implementation for the JVM back-end.           */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    SOCKET ...                                                       */
/*---------------------------------------------------------------------*/
public class server_socket extends socket {
   /*--- private fields ---------------------------------------------------*/
   protected ServerSocket server_socket;
   protected Socket client_socket;
   protected boolean blocking;

   /*--- constructors ----------------------------------------------------*/
   public server_socket( Object name, final int port ) {
      super();

      try {
         String n = (name == bbool.faux) ? null : new String((byte[])name);
         server_socket = JDK.makeServerSocket(n, port);
      } catch (final IOException _) {
	 socket_error( "make-server-socket",
		       "cannot create socket",
		       new bint( port ) );
      }
   }

   protected server_socket() {
   }

   private server_socket( final server_socket templ ) {
      super();
      server_socket = templ.server_socket;
      client_socket = templ.client_socket;
      blocking = templ.blocking;
   }

   /*--- public methods --------------------------------------------------*/
   public Object HOSTNAME() {
      if (client_socket != null)
	 return client_socket.getInetAddress().getHostName().getBytes();
      else
	 return bigloo.foreign.BFALSE;
   }

   public Object HOSTIP() {
      if (client_socket != null)
	 return client_socket.getInetAddress().getHostAddress().getBytes();
      else
	 return bigloo.foreign.BFALSE;
   }

   public byte[] local_addr() {
      return server_socket.getInetAddress().getHostAddress().getBytes();
   }

   public client_socket accept( byte[] inbuf, byte[] outbuf, boolean errp )
      throws IOException, SecurityException {
      // WARNING this blocking parameter is not called blocking in C
      // it is called buffered, and C does not do non-blocking [yet], so
      // we don't support it [yet] (but we can)
      final Socket accepted_socket = JDK.accept(server_socket, true);
      if( accepted_socket == null ) {
	 throw new IOException("Nothing to accept");
      }
	 
      return new client_socket( accepted_socket, inbuf, outbuf );
   }

   public Object shutdown( final boolean close_socket ) {
      try {
	 if (client_socket != null)
	    try {
	       client_socket.shutdownOutput();
	       client_socket.shutdownInput();
	    } catch (Exception _) {
	    }

	 if( close_socket ) close();
      } catch( Throwable _ ) {
	 ;
      }

      return bigloo.foreign.BUNSPEC;
   }

   public Object close() {
      try {
	 server_socket.close();
      } catch( Throwable _ ) {
	 ;
      } finally {
	 down = true;
      }
      return bigloo.foreign.BUNSPEC;
   }

   public int PORT() {
      return server_socket.getLocalPort();
   }

   public void write( final output_port  p ) {
      p.write( "#<socket:" + server_socket.getInetAddress() 
	       + ":" + PORT() + ">" );
   }
  
   public ServerSocket getSocket() {
      return server_socket;
   }

   public Object getsockopt( keyword se ) throws IOException {
      if( client_socket != null ) {
	 if( se == socket.tcp_nodelay ) {
	    return foreign.BBOOL( client_socket.getTcpNoDelay() );
	 }
	 if( se == socket.so_keepalive ) {
	    return foreign.BBOOL( client_socket.getKeepAlive() );
	 }
	 if( se == socket.so_oobinline ) {
	    return foreign.BBOOL( client_socket.getOOBInline() );
	 }
	 if( se == socket.so_rcvbuf ) {
	    return foreign.BINT( client_socket.getReceiveBufferSize() );
	 }
	 if( se == socket.so_sndbuf ) {
	    return foreign.BINT( client_socket.getSendBufferSize() );
	 }
	 if( se == socket.so_reuseaddr ) {
	    return foreign.BBOOL( client_socket.getReuseAddress() );
	 }
	 if( se == socket.so_timeout ) {
	    return foreign.BINT( client_socket.getSoTimeout() );
	 }
      }

      return foreign.BUNSPEC;
   }
   
   public Object setsockopt( keyword se, Object val ) throws IOException {
      if( client_socket != null ) {
	 if( se == socket.tcp_nodelay ) {
	    client_socket.setTcpNoDelay( foreign.CBOOL( val ));
	    return this;
	 }
	 if( se == socket.so_keepalive ) {
	    client_socket.setKeepAlive( foreign.CBOOL( val ));
	    return this;
	 }
	 if( se == socket.so_oobinline ) {
	    client_socket.setOOBInline( foreign.CBOOL( val ));
	    return this;
	 }
	 if( se == socket.so_rcvbuf ) {
	    client_socket.setReceiveBufferSize( foreign.CINT( (bint)val ));
	    return this;
	 }
	 if( se == socket.so_sndbuf ) {
	    client_socket.setSendBufferSize( foreign.CINT( (bint)val ));
	    return this;
	 }
	 if( se == socket.so_reuseaddr ) {
	    client_socket.setReuseAddress( foreign.CBOOL( val ));
	    return this;
	 }
	 if( se == socket.so_timeout ) {
	    client_socket.setSoTimeout( foreign.CINT( (bint)val ));
	    return this;
	 }
      }
      
      return foreign.BUNSPEC;
   }
}
