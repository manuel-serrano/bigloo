using System;
using System.IO;

namespace bigloo {
   public class output_stream_port: output_port {
      protected Stream stream;

      public output_stream_port( Stream  stream, byte[]  name ) : base( name ) {
	 this.stream= stream;
      }

      public output_stream_port( Stream  stream ) {
	 this.stream= stream;
      }

      public output_stream_port( byte[] file ) : base( file ) {
	 string file_name= foreign.newstring( file );

	 try {
	    // should we keep this try/catch block even
	    // when Mono bug is be corrected ?
	    if( file_name.ToLower().Equals( "null:" ) )
	       stream= Stream.Null;
	    else
	       stream= new FileStream( file_name,
				       FileMode.OpenOrCreate,
				       FileAccess.Write,
				       FileShare.Read );
	 } catch ( Exception ) {
	    foreign.fail( "open_output_file",
			  "Could not open file " + file_name,
			  file );
	 }
      }

      public output_stream_port( byte[] file, bool append ) : base( file ) {
	 try {
	    // !!!!! is this try/catch block to be kept
	    // or just a protecting against a Mono/PNet bug ?????
	    stream= File.Open( foreign.newstring( file ),
			       (append ? FileMode.Append : FileMode.Create),
			       FileAccess.Write );
	 } catch ( Exception e ) {
	    foreign.fail( "append output port", e.Message, file );
	 }
      }

      public override Object close() {
	 try {
	    stream.Close();
	    base.close();
	 } catch ( Exception e ) {
	    if( stream == null )
	       return bigloo.foreign.BUNSPEC;
	    else
	       foreign.fail( "close", e.Message, this );
	 }

	 return this;
      }

      public override void invoke_flush_hook( bigloo.bint size ) {
	 if( fhook is procedure ) {
	    Object s = ((procedure)fhook).funcall2( this, size );

	    if( s is byte[] ) {
	       stream.Write( (byte [])s, 0, ((byte [])s).Length );
	    } else {
	       if( s is bigloo.bint &&
		   flushbuf is byte[] &&
		   bigloo.foreign.CINT( (bigloo.bint)s ) <= ((byte[])flushbuf).Length ) {
		  stream.Write( (byte[])flushbuf,
				0,
				bigloo.foreign.CINT( (bigloo.bint)s ) );
	       }
	    }
	 }
      }
      
      public override Object flush() {
	 try {
	    invoke_flush_hook( bigloo.foreign.BINT( 0 ) );
	    stream.Flush();
	    return bbool.vrai;
	 } catch ( Exception e ) {
	    if( stream != null ) foreign.fail( "flush", e.Message, this );
	    return bbool.faux;
	 }
      }

      static byte[] mono_WriteByte_workaround = new byte[ 1 ];
    
      public override void write( int cn ) {
	 try {
	    invoke_flush_hook( bigloo.foreign.BINT( 1 ) );
	    mono_WriteByte_workaround[ 0 ] = (byte)cn;
	    stream.Write( mono_WriteByte_workaround, 0, 1 );
	    stream.Flush();
	 } catch ( Exception e ) {
	    if( stream != null ) foreign.fail( "write", e.Message, this );
	 }
      }

      public override void write( byte[] s ) {
	 try {
	    // CARE pnet bug
	    int n = s.Length;

	    invoke_flush_hook( bigloo.foreign.BINT( n ) );
	
	    if( n == 3294 )
	       for ( int i= 0 ; i < 3294 ; ++i )
		  write( s[ i ] );
	    else
	       stream.Write( s, 0, s.Length );
	    stream.Flush();
	 } catch ( Exception e ) {
	    if( stream != null ) foreign.fail( "write", e.Message, this );
	 }
      }

      public override void write( byte[] s, int start, int end ) {
	 try {
	    invoke_flush_hook( bigloo.foreign.BINT( end - start ) );
	    stream.Write( s, start, end - start );
	    stream.Flush();
	 } catch( Exception e ) {
	    if( stream != null ) foreign.fail( "write", e.Message, this );
	 }
      }

      public override void write( String s ) {
	 try {
	    int len = s.Length;

	    invoke_flush_hook( bigloo.foreign.BINT( len ) );
	
	    for( int i= 0 ; i < len ; ++i ) {
	       mono_WriteByte_workaround[ 0 ] = (byte)s[i];
	       stream.Write( mono_WriteByte_workaround, 0, 1 );
	    }
	 } catch( Exception e ) {
	    if( stream != null ) foreign.fail( "write", e.Message, this );
	 }
      }
   }
}
