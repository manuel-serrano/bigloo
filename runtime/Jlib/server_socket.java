/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/server_socket.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Mon Oct 24 13:44:56 2016 (serrano)                */
/*    Copyright   :  2000-16 Manuel Serrano                            */
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
   public ServerSocket server_socket;
   protected Socket client_socket;
   protected boolean blocking;

   /*--- constructors ----------------------------------------------------*/
   public server_socket( Object name, final int port ) {
      super();

      try {
         String n = (name == bbool.faux) ? null : new String((byte[])name);
         server_socket = JDK.makeServerSocket(n, port);
      } catch (final IOException _i) {
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

   public int shutdown( final int how ) {
      try {
	 if (client_socket != null)
	    try {
	       switch( how ) {
		  case 0:
		     client_socket.shutdownOutput();
		     client_socket.shutdownInput();
		     break;
		  case 1:
		     client_socket.shutdownInput();
		     break;
		  default:
		     client_socket.shutdownOutput();
		     break;
	       }
	    } catch (Exception _e) {
	       return 2;
	    }
      } catch( Throwable _t ) {
	 return 1;
      }

      return 0;
   }

   public Object close() {
      try {
	 server_socket.close();
      } catch( Throwable _t ) {
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
