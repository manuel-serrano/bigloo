/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/server_socket.cs       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Sat Sep  6 15:37:17 2008 (serrano)                */
/*    Copyright   :  2000-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Server Socket implementation for the JVM back-end.           */
/*=====================================================================*/
using System;
using System.Net;
using System.Net.Sockets;

/*---------------------------------------------------------------------*/
/*    SOCKET ...                                                       */
/*---------------------------------------------------------------------*/
namespace bigloo {
   public sealed class server_socket: socket {
      private readonly Socket _server_socket;
      private Socket client_socket;
      private bool blocking;

      public server_socket( Object name, int port ) : base() {
	 try {
/* 	    _server_socket = new Socket( AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp ); */
/* 	    _server_socket.Bind( new IPEndPoint( 0, port ) );          */

	    IPEndPoint endpoint;
	    
	    if( name != bigloo.foreign.BFALSE ) {
	       String server = bigloo.foreign.newstring( name );
	       IPHostEntry host = Dns.Resolve(server);
	       IPAddress address = host.AddressList[0];
	       endpoint = new IPEndPoint(address, port);
	    } else {
	       endpoint = new IPEndPoint( 0, port );
	    }

	    _server_socket = new Socket( endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp );
	    _server_socket.Bind( endpoint );
	
	    _server_socket.Listen( 10 );
	 } 
	 catch (Exception e) {
	    socket_error( "make-server-socket",
			  "cannot create socket (" + e.Message + ")",
			  new bint( port ) );
	 }
      }

      private server_socket( server_socket templ ) : base() {
	 _server_socket= templ._server_socket;
	 client_socket= templ.client_socket;
	 blocking= templ.blocking;
      }

      public override Object HOSTNAME() {
	 if (client_socket != null)
	    return foreign.getbytes( Dns.GetHostByAddress( ((IPEndPoint)client_socket.RemoteEndPoint).Address ).HostName );
	 else
	    return bigloo.foreign.BFALSE;
      }

      public override Object HOSTIP() {
	 if (client_socket != null)
	    return foreign.getbytes( ((IPEndPoint)client_socket.RemoteEndPoint).Address.ToString() );
	 else
	    return bigloo.foreign.BFALSE;
      }

      public override byte[] local_addr() {
	 return foreign.getbytes( ((IPEndPoint)_server_socket.LocalEndPoint).Address.ToString() );
      }

      public client_socket accept( byte[] inbuf, byte[] outbuf, bool errp ) {
	 Socket accepted_socket= _server_socket.Accept();

	 return new client_socket( accepted_socket, inbuf, outbuf );
      }

      public override Object shutdown( bool close_socket ) {
	 if (client_socket != null)
	    try {
	       client_socket.Shutdown( SocketShutdown.Both );
	    } catch (Exception) {
	    }

	 if (close_socket)
	    close();

	 return bigloo.foreign.BUNSPEC;
      }

      public override Object close() {
	 // !!!!! On fait pas de shutdown ?????
	 _server_socket.Close();
	 down= true;
	 return bigloo.foreign.BUNSPEC;
      }

      public override int PORT() {
	 return ((IPEndPoint)_server_socket.LocalEndPoint).Port;
      }

      public override void write( output_port  p ) {
	 p.write( "#<socket:" +
		  ((IPEndPoint)_server_socket.LocalEndPoint).Address +
		  ":" + PORT() + ">" );
      }
   }
}
