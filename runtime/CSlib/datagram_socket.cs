/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/datagram_socket.cs     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun May  1 16:39:49 2011                          */
/*    Last change :  Sun May  1 16:53:58 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    DatagramSocket implementation for the .NET backend.              */
/*=====================================================================*/

using System;
using System.Net.Sockets;

namespace bigloo 
{
  /*---------------------------------------------------------------------*/
  /*    DATAGRAMSOCKET ...                                               */
  /*---------------------------------------------------------------------*/
  public abstract class datagram_socket: obj
  {
     // public fields
     public output_port output;
     
     // constructor
     protected datagram_socket() {
	;
     }

     // abstract methods
     public Object HOSTNAME() {
	return foreign.BUNSPEC;
     }

     public Object HOSTIP() {
	return foreign.BUNSPEC;
     }

     public int PORT() {
	return 0;
     }
   
     public output_port OUTPUT_PORT() {
	return output;
     }
   
     public Object close() {
      return bigloo.foreign.BUNSPEC;
     }
   
     public Object receive( int len ) {
	return foreign.fail( "receive", "not a datagram-server socket", this );
     }
  }
}
