package bigloo;

import java.io.*;
import java.util.*;
import java.net.*;

public class input_resource_port extends input_port {
   public InputStream in;

   public input_resource_port( String resource, final byte[] buf )
      throws IOException {
      super( "/resource/" + resource, buf );
      in = foreign.class.getClassLoader().getResourceAsStream( resource.replace( '\\', '/' ) );
   }

   public static boolean exists( String name ) {
      return foreign.class.getClassLoader().getResource( name.replace( '\\', '/' ) ) != null;
   }

   public static int file_size( String name ) {
      try {
	 InputStream in = foreign.class.getClassLoader().getResourceAsStream( name.replace( '\\', '/' ) );
	 int sz = in.available();
	 in.close();
	 return sz;
      } catch( Exception _e ) {
	 return -1;
      }
   }

   public static byte[] readline( InputStream in ) throws Exception {
      int c = in.read();

      if( c == -1 ) {
	 return null;
      } else {
	 byte[] buf = new byte[ 255 ];
	 int l = 1;
	 buf[ 0 ] = (byte)c;

	 while( true ) {
	    c = in.read();

	    if( (c == '\n') || (c == -1) ) {
	       byte[] res = new byte[ l ];
	       System.arraycopy( buf, 0, res, 0, l );
	       return res;
	    }

	    buf[ l++ ] = (byte)c;
	 }
      }
   }
      
   public static Object bgl_directory_to_list( String name ) {
      try {
	 String dir = name.replace( '\\', '/' );
	 String cname = dir + "/.list";
	 
	 if( input_resource_port.exists( cname ) ) {
	    InputStream in;
	    Object res = bigloo.foreign.BNIL;
	    in = foreign.class.getClassLoader().getResourceAsStream( cname );
	    byte[] o;

	    while( (o = readline( in )) != null ) {
	       res = new pair( o, res );
	    }

	    in.close();

	    return res;
	 } else {
	    return bigloo.foreign.BNIL;
	 }
      } catch( Exception _e ) {
	 return bigloo.foreign.BNIL;
      }
   }

   public static boolean bgl_directoryp( String name ) {
      String dir = name.replace( '\\', '/' );
      String cname = dir + "/.list";

      return input_resource_port.exists( cname );
   }
   
   public void close() {
      eof = true;
      other_eof = true;
      try {
	 in.close();
      } catch( Throwable _t ) {
	 ;
      }
      super.close();
   }

   public boolean rgc_charready() {
      if (eof || other_eof)
	 return false;
      else
	 return true;
   }

   public boolean rgc_fill_buffer() throws IOException {
      final int bufsize = this.buffer.length;
      int bufpose = this.bufpos;
      final int matchstart = this.matchstart;
      final byte[] buffer = this.buffer;

      if (0 < matchstart) {
	 // we shift the buffer left and we fill the buffer */
	 final int movesize = bufpose-matchstart;

	 for ( int i= 0 ; i < movesize ; ++i )
	    buffer[i] = buffer[matchstart+i];

	 bufpose -= matchstart;
	 this.matchstart = 0;
	 this.matchstop -= matchstart;
	 this.forward -= matchstart;
	 this.lastchar = buffer[matchstart-1];

	 return rgc_size_fill_resource_buffer( bufpose, bufsize-bufpose );
      }

      if (bufpose < bufsize)
	 return rgc_size_fill_resource_buffer( bufpose, bufsize-bufpose );

      // we current token is too large for the buffer */
      // we have to enlarge it.                       */
      rgc_double_buffer();

      return rgc_fill_buffer();
   }

   final boolean rgc_size_fill_resource_buffer( int bufpose, final int  size )
      throws IOException {
      final int nbread = in.read( buffer, bufpose, size );

      if (nbread == -1)
	 eof = true;
      else
	 bufpose += nbread;

      this.bufpos = bufpose;

      if (0 < bufpose) {
	 return true;
      }

      return false;
   }

   Object bgl_input_port_seek( final int  pos ) throws IOException {
      return bigloo.foreign.BFALSE;
   }

   Object bgl_input_port_reopen() throws IOException {
      in.close();

      in = foreign.class.getClassLoader().getResourceAsStream( name );

      filepos = 0;
      eof = false;
      matchstart = 0;
      matchstop = 0;
      forward = 0;
      bufpos = 0;
      lastchar = (byte)'\n';

      return bigloo.foreign.BTRUE;
   }

   public Object bgl_input_port_clone( input_port src ) {
      super.bgl_input_port_clone( src );
      in = ((input_resource_port)src).in;
      
      return this;
   }
   
   public void write( final output_port  p ) {
      p.write( "#<input_resource_port:" + name + ">" );
   }
}
