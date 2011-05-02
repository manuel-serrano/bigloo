/*=====================================================================*/
/*    .../project/bigloo/runtime/CSlib/datagram_server_socket.cs       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun May  1 16:39:49 2011                          */
/*    Last change :  Sun May  1 16:55:47 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Server DatagramSocket implementation for the .NET backend.       */
/*=====================================================================*/

using System;
using System.Net.Sockets;

namespace bigloo 
{
  /*---------------------------------------------------------------------*/
  /*    DATAGRAM SERVER SOCKET ...                                       */
  /*---------------------------------------------------------------------*/
   public class datagram_server_socket: datagram_socket {
      // constructors
      public datagram_server_socket() : base() {
	 ;
      }
      public datagram_server_socket( int p ) : base() {
	 foreign.fail( "make-datagram-server-socket", "not supported", p );
      }
   }
}

