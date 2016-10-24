package bigloo;

import java.io.*;

public class output_buffered_port extends output_port {
   // Bigloo implements its own bufferization to controlbuffer allocations.
   byte[] buffer;
   int count;
   
   public output_buffered_port() {
      super();
      buffer = null;
      count = 0;
   }
   
   public output_buffered_port( final OutputStream stream,
				byte[] _buffer, 
				final byte[] name ) {
      super( stream, name );
      buffer = _buffer;
      count = 0;
   }
   
   public output_buffered_port( final byte[] file,
				byte[] _buffer, 
				final boolean append )
      throws IOException {
      super( file, append );
      buffer = _buffer;
      count = 0;
   }
   
   public output_buffered_port( final byte[] file,
				byte[] _buffer )
      throws IOException {
      super( file );
      buffer = _buffer;
      count = 0;
   }
   
   public static output_port make_output_buffered_port( final OutputStream stream,
							byte[] buffer,
							final byte[] name ) {
      if( buffer == null || buffer.length == 0 ) {
	 return new output_port( stream, name );
      } else {
	 return new output_buffered_port( stream, buffer, name );
      }
   }
   
   public static output_port make_output_buffered_port( final byte[] file,
							byte[] buffer,
							final boolean app )
      throws IOException {
      if( buffer == null || buffer.length == 0 ) {
	 return new output_port( file, app );
      } else {
	 return new output_buffered_port( file, buffer, app );
      }
   }
   
   public static output_port make_output_buffered_port( final byte[] file,
							byte[] buffer )
      throws IOException {
      if( buffer == null || buffer.length == 0 ) {
	 return new output_port( file );
      } else {
	 return new output_buffered_port( file, buffer );
      }
   }
   
   public Object close() {
      flush();
      
      return super.close();
   }
   
   public Object flush() {
      if( count > 0 ) {
	 invoke_flush_hook( bigloo.foreign.BINT( count ) );
	 try {
	    out.write( buffer, 0, count );
	 } catch( Throwable _t ) {
	    ;
	 }
	 count = 0;
      }

      try {
	 super.flush();
      } catch( Throwable _t ) {
	 return bbool.faux;
      }
      
      return bbool.vrai;
   }
   
   public void write( final int cn ) {
      try {
	 if( count < buffer.length ) {
	    buffer[ count++ ] = (byte)cn;
	 } else {
	    if( count > 0 ) invoke_flush_hook( bigloo.foreign.BINT( count ) );
	    out.write( buffer );
	    buffer[ 0 ] = (byte)cn;
	    count = 1;
	 }
      } catch (final Exception e) {
	 if( out != null ) foreign.fail( "write", e, this );
      }
   }

   public void write( final byte[] s ) {
      try {
	 if( (count + s.length) < buffer.length ) {
	    System.arraycopy( s, 0, buffer, count, s.length );
	    count += s.length;
	 } else {
	    if( count > 0 ) {
	       invoke_flush_hook( bigloo.foreign.BINT( count ) );
	       out.write( buffer, 0, count );
	       count = 0;
	    }

	    if( s.length > 0 )
	       invoke_flush_hook( bigloo.foreign.BINT( s.length ) );
	    out.write( s );
	 }
      } catch( final Exception e ) {
	 if( out != null ) foreign.fail( "write", e, this );
      }
   }

   public void write( final byte[] s, int offset, int len ) {
      try {
	 final int l = len - offset;
	 
	 if( (count + l) < buffer.length ) {
	    System.arraycopy( s, offset, buffer, count, l );
	    count += (len - offset);
	 } else {
	    if( count > 0 ) {
	       invoke_flush_hook( bigloo.foreign.BINT( count ) );
	       out.write( buffer, 0, count );
	       count = 0;
	    }

	    if( l > 0 ) invoke_flush_hook( bigloo.foreign.BINT( l ) );
	    out.write( s, offset, l );
	 }
      } catch ( final Exception e ) {
	 if( out != null ) foreign.fail( "write", e, this );
      }
   }

   public void write( final String s ) {
      final int len = s.length();
      
      try {
	 if( (count + len) < buffer.length ) {
	    for ( int i = 0 ;i < len ; ++i ) {
	       buffer[ count++ ] = (byte)s.charAt( i );
	    }
	 } else {

	    if( count > 0 ) invoke_flush_hook( bigloo.foreign.BINT( count ) );
	    out.write( buffer, 0, count );
	    count = 0;

	    if( len > 0 ) invoke_flush_hook( bigloo.foreign.BINT( len ) );
	    for ( int i = 0 ;i < len ; ++i ) {
	       out.write( (byte)s.charAt( i ) );
	    }
	 }
      } catch ( final Exception e ) {
	 if( out != null ) foreign.fail( "write", e, this );
      }
   }
}
