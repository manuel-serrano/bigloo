using System;
using System.Net.Sockets;
using System.IO;

namespace bigloo {
   public class output_socket_port: output_port {
      public Socket _socket;

      public output_socket_port( Socket _socket ) {
	 this._socket= _socket;
      }

      public override Object close() {
	 _socket= null;
	 base.close();
	 return this;
      }

      private void sendbuffer( byte []s, int start, int end ) {
	 byte[] r = new byte[ end - start ];
	 int j = 0;
	 
	 for ( int i = start ; i < end ; ++i, ++j ) r[ j ]= s[ i ];

	 _socket.Send( r );
      }
      
      public override void invoke_flush_hook( bigloo.bint size ) {
	 if( fhook is procedure ) {
	    Object s = ((procedure)fhook).funcall2( this, size );

	    if( s is byte[] ) {
	       sendbuffer( (byte [])s, 0, ((byte [])s).Length );
	    } else {
	       if( s is bigloo.bint &&
		   flushbuf is byte[] &&
		   bigloo.foreign.CINT( (bigloo.bint)s ) <= ((byte[])flushbuf).Length ) {
		  sendbuffer( (byte[])flushbuf,
			      0,
			      bigloo.foreign.CINT( (bigloo.bint)s ) );
	       }
	    }
	 }
      }
      
      public override Object flush() {
	 invoke_flush_hook( bigloo.foreign.BINT( 0 ) );
	 return bbool.vrai;
      }

      public override void write( int cn ) {
	 try {
	    invoke_flush_hook( bigloo.foreign.BINT( 1 ) );
	    byte[] buffer= { (byte)cn };

	    _socket.Send( buffer );
	 } catch ( Exception e ) {
	    if( _socket != null ) foreign.fail( "write", e.Message, this );
	 }
      }

      public override void write( byte[] s ) {
	 try {
	    invoke_flush_hook( bigloo.foreign.BINT(s.Length ) );
	    _socket.Send( s );
	 } catch( Exception e ) {
	    if( _socket != null ) foreign.fail( "write", e.Message, this );
	 }
      }

      public override void write( byte[] s, int start, int end ) {
	 try {
	    invoke_flush_hook( bigloo.foreign.BINT( end - start ) );
	    sendbuffer( s, start, end );
	 } catch( Exception e ) {
	    if( _socket != null ) foreign.fail( "write", e.Message, this );
	 }
      }

      public override void write( String s ) {
	 try {
	    invoke_flush_hook( bigloo.foreign.BINT( s.Length ) );
	    byte[] buffer= foreign.getbytes( s );

	    _socket.Send( buffer );
	 } catch( Exception e ) {
	       if( _socket != null ) foreign.fail( "write", e.Message, this );
	    }
      }
   }
}
