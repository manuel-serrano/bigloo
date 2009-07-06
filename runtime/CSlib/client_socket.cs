/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/client_socket.cs       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Sat Sep  6 15:35:46 2008 (serrano)                */
/*    Copyright   :  2000-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Client Socket implementation for the JVM back-end.           */
/*=====================================================================*/
using System;
using System.Net;
using System.Net.Sockets;

namespace bigloo
{
   /*---------------------------------------------------------------------*/
   /*    SOCKET ...                                                       */
   /*---------------------------------------------------------------------*/
   public sealed class client_socket: socket
   {
      /*--- private fields ---------------------------------------------------*/
      private readonly Socket _socket;
 
      /*--- constructors ----------------------------------------------------*/
      // This is a client socket creation
      public client_socket( byte[]  hostname, int port, byte[] inbuf, byte[] outbuf ) : base() {
	 try {
	    IPHostEntry ip_host_entry= Dns.Resolve( foreign.newstring( hostname ) );
	    IPEndPoint ip_end_point= new IPEndPoint( ip_host_entry.AddressList[0], port );

	    _socket = new Socket( AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp );
	    _socket.Connect( ip_end_point );
	    set_socket_io_ports( _socket, inbuf, outbuf );
	 }
	 catch (Exception e) {
	    socket_error( "make-client-socket",
			  "cannot create socket (" + e.Message + ")",
			  hostname );
	 }
      }

      internal client_socket( Socket _socket, byte[] inbuf, byte[] outbuf ) : base() {
	 this._socket= _socket;
	 set_socket_io_ports( _socket, inbuf, outbuf );
      }

      /*--- public methods --------------------------------------------------*/
      public override Object HOSTNAME()
      {
	 return foreign.getbytes( Dns.GetHostByAddress( ((IPEndPoint)_socket.RemoteEndPoint).Address ).HostName );
      }

      public override Object HOSTIP()
      {
	 return foreign.getbytes( ((IPEndPoint)_socket.RemoteEndPoint).Address.ToString() );
      }

      public override byte[] local_addr()
      {
	 return foreign.getbytes( ((IPEndPoint)_socket.LocalEndPoint).Address.ToString() );
      }

      public override Object shutdown( bool  close_socket )
      {
	 close();
	 // !!!!! On appelle _socket.Shutdown() ou bien ?????  (Pas fait en Java)
	 down= true;
	 return bigloo.foreign.BUNSPEC;
      }

      public override Object close()
      {
	 _socket.Close();
	 return base.close();
      }

      public override int PORT()
      {
	 return ((IPEndPoint)_socket.RemoteEndPoint).Port;
      }

      public override void write( output_port  p )
      {
	 p.write( "#<socket:" + foreign.newstring( HOSTNAME() ) + "." + PORT() + ">" );
      }
   }
}
