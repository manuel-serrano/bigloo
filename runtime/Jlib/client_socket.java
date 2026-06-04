/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/client_socket.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Mon Oct 24 13:44:37 2016 (serrano)                */
/*    Copyright   :  2000-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Client Socket implementation for the JVM back-end.           */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    CLIENT_SOCKET ...                                                */
/*---------------------------------------------------------------------*/
public class client_socket extends socket {
   /*--- public fields ---------------------------------------------------*/
   public Socket socket;

   /*--- constructors ----------------------------------------------------*/
   // This is a client socket creation
   public client_socket() {
      super();
   }
   
   public client_socket( final byte[] hostname,
			 final int port,
			 final int timeoutUs,
			 final byte[] inbuf,
			 final byte[] outbuf,
			 final Object family ) {
      super();

      try {
	 String host = new String( hostname );
	 InetAddress addr = resolveAddress( host, family );
	 int timeoutMs = timeoutUs / 1000;

	 if (timeoutMs > 0) {
	    socket = new Socket();
	    socket.connect( new InetSocketAddress( addr, port ), timeoutMs );
	 } else {
	    socket = new Socket( addr, port );
	 }

	 String name = host + ":" + port;
	 set_socket_io_ports( socket, inbuf, outbuf, name.getBytes() );
      } catch (final UnknownHostException e) {
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    foreign.BGL_IO_UNKNOWN_HOST_ERROR,
	    "make-client-socket".getBytes(),
	    "unknown or misspelled host name".getBytes(),
	    hostname );
      } catch (final SocketTimeoutException e) {
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    foreign.BGL_IO_TIMEOUT_ERROR,
	    "make-client-socket".getBytes(),
	    "connection timed out".getBytes(),
	    hostname );
      } catch (final IOException e) {
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    foreign.BGL_IO_ERROR,
	    "make-client-socket".getBytes(),
	    (e.getMessage() != null ? e.getMessage() : "cannot create socket").getBytes(),
	    hostname );
      }
   }

   protected static InetAddress resolveAddress( String hostname,
					      Object family )
      throws UnknownHostException {
      String dom = family.toString();
      if (dom.equals( "unspec" )) {
	 return InetAddress.getByName( hostname );
      }
      InetAddress[] addrs = InetAddress.getAllByName( hostname );
      for (InetAddress addr : addrs) {
	 if (dom.equals( "inet6" ) && addr instanceof Inet6Address) {
	    return addr;
	 }
	 if (dom.equals( "inet" ) && addr instanceof Inet4Address) {
	    return addr;
	 }
      }
      return addrs[0];
   }

   public client_socket( final Socket socket,
			 final byte[] inbuf ,
			 final byte[] outbuf ) {
      super();

      this.socket = socket;

      try {
	 String name = socket.toString();
	 set_socket_io_ports( socket, inbuf, outbuf, name.getBytes() );
      } catch (final IOException _i) {
	 socket_error( "make-client-socket",
		       "cannot create socket",
		       unspecified.unspecified );
      }
   }

   /*--- public methods --------------------------------------------------*/
   public Object HOSTNAME() {
      return socket.getInetAddress().getHostName().getBytes();
   }

   public Object HOSTIP() {
      return socket.getInetAddress().getHostAddress().getBytes();
   }

   public byte[] local_addr() {
      return socket.getLocalAddress().getHostAddress().getBytes();
   }

   public int shutdown( final int how ) {
      if( !down ) {
	 try {
	    switch( how ) {
	       case 0:
		  socket.shutdownOutput();
		  socket.shutdownInput();
		  break;
	       case 1:
		  socket.shutdownInput();
		  break;
	       default:
		  socket.shutdownOutput();
	       break;
	    }
	    down = true;
	 } catch( Throwable _t ) {
	    return 1;
	 }
      }
      return 0;
   }

   public Object close() {
      try {
	 socket.close();
      } catch( Throwable _t ) {
	 ;
      } finally {
	 super.close();
      }

      return bigloo.foreign.BUNSPEC;
   }

   public int PORT() {
      return socket.getPort();
   }

   public void write( final output_port  p ) {
      Object hostname = HOSTNAME();

      if (hostname instanceof byte[])
	 hostname= new String( (byte[])hostname );

      p.write( "#<socket:" + hostname.toString() + "." + PORT() + ">" );
   }
   
   public Socket getSocket() {
      return socket;
   }
   
   public Object getsockopt( keyword se ) throws IOException {
      if( se == bigloo.socket.tcp_nodelay ) {
	 return foreign.BBOOL( socket.getTcpNoDelay() );
      }
      if( se == bigloo.socket.so_keepalive ) {
	 return foreign.BBOOL( socket.getKeepAlive() );
      }
      if( se == bigloo.socket.so_oobinline ) {
	 return foreign.BBOOL( socket.getOOBInline() );
      }
      if( se == bigloo.socket.so_rcvbuf ) {
	 return foreign.BINT( socket.getReceiveBufferSize() );
      }
      if( se == bigloo.socket.so_sndbuf ) {
	 return foreign.BINT( socket.getSendBufferSize() );
      }
      if( se == bigloo.socket.so_reuseaddr ) {
	 return foreign.BBOOL( socket.getReuseAddress() );
      }
      if( se == bigloo.socket.so_timeout ) {
	 return foreign.BINT( socket.getSoTimeout() );
      }

      return foreign.BUNSPEC;
   }
   
   public Object setsockopt( keyword se, Object val ) throws IOException {
      if( se == bigloo.socket.tcp_nodelay ) {
	 socket.setTcpNoDelay( foreign.CBOOL( val ));
	 return this;
      }
      if( se == bigloo.socket.so_keepalive ) {
	 socket.setKeepAlive( foreign.CBOOL( val ));
	 return this;
      }
      if( se == bigloo.socket.so_oobinline ) {
	 socket.setOOBInline( foreign.CBOOL( val ));
	 return this;
      }
      if( se == bigloo.socket.so_rcvbuf ) {
	 socket.setReceiveBufferSize( foreign.CINT( (bint)val ));
	 return this;
      }
      if( se == bigloo.socket.so_sndbuf ) {
	 socket.setSendBufferSize( foreign.CINT( (bint)val ));
	 return this;
      }
      if( se == bigloo.socket.so_reuseaddr ) {
	 socket.setReuseAddress( foreign.CBOOL( val ));
	 return this;
      }
      if( se == bigloo.socket.so_timeout ) {
	 socket.setSoTimeout( foreign.CINT( (bint)val ));
	 return this;
      }
      
      return foreign.BUNSPEC;
   }
}
