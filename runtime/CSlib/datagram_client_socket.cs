/*=====================================================================*/
/*    .../project/bigloo/runtime/CSlib/datagram_client_socket.cs       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun May  1 16:39:49 2011                          */
/*    Last change :  Sun May  1 16:55:37 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Client DatagramSocket implementation for the .NET backend.       */
/*=====================================================================*/

using System;
using System.Net.Sockets;

namespace bigloo 
{
  /*---------------------------------------------------------------------*/
  /*    DATAGRAM CLIENT SOCKET ...                                       */
  /*---------------------------------------------------------------------*/
   public class datagram_client_socket: datagram_socket {
      // constructors
      public datagram_client_socket() : base() {
	 ;
      }
      public datagram_client_socket( byte[] hostname,
				     int p,
				     bool broadcast ) : base() {
	 foreign.fail( "make-datagram-client-socket", "not supported", p );
      }
   }
}

