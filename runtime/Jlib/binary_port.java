/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/binary_port.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Dec  3 11:33:29 2000                          */
/*    Last change :  Thu Jul 16 07:57:20 2015 (serrano)                */
/*    Copyright   :  2000-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The binary port JVM connection                                   */
/*=====================================================================*/
package bigloo;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    BINARY_PORT ...                                                  */
/*---------------------------------------------------------------------*/
public class binary_port extends obj {
   private static final byte[] MAGIC_WORD= "1966".getBytes();
   public final Object stream;

   public binary_port( final InputStream stream ) {
      this.stream = stream;
   }

   public binary_port( final OutputStream stream ) {
      this.stream = stream;
   }

   public binary_port close() throws IOException  {
      if( stream instanceof InputStream )
	 ((InputStream)stream).close();
      else
	 if( stream instanceof OutputStream )
	    ((OutputStream)stream).close();

      return this;
   }

   public obj flush() throws IOException {
      if( stream instanceof OutputStream )
	 ((OutputStream)stream).flush();

      return this;
   }

   public void write( final output_port p ) {
      p.write( "#<binary:" );

      if( stream instanceof InputStream )
	 p.write( "input_port:" );
      else
	 p.write( "output_port:" );

      p.write( stream.toString() );
      p.write( ">" );
   }

   public obj output_obj( final Object obj ) throws IOException {
      final OutputStream file = (OutputStream)stream;

      /* the magic key */
      final byte[] string = bigloo.runtime.Unsafe.intext.obj_to_string( obj, bigloo.foreign.BFALSE );

      file.write( MAGIC_WORD );

      /* the object length */
      final byte[] slen = new byte[4];
      final int clen = string.length;

      slen[0] = (byte)clen;
      slen[1] = (byte)(clen >> 8);
      slen[2] = (byte)(clen >> 16);
      slen[3] = (byte)(clen >> 24);

      file.write( slen );

      /* the serialized object */
      file.write( string );

      return this;
   }

   public Object input_obj() throws IOException {
	 final InputStream file = (InputStream)stream;

	 /* magic key */
	 final byte[] magic = new byte[4];
	 int size = file.read( magic );

	 if( size == -1 )
	    return eof.eof;

	 if( (size != 4)
	     || (magic[0] != MAGIC_WORD[0])
	     || (magic[1] != MAGIC_WORD[1])
	     || (magic[2] != MAGIC_WORD[2])
	     || (magic[3] != MAGIC_WORD[3]) )
	    foreign.fail( "input_obj", "corrupted file", this );

	 /* the object length */
	 final byte[] slen = new byte[4];

	 size = file.read( slen );
	 if( size != 4 )
	    foreign.fail( "input_obj", "corrupted file", this );

	 final int clen = (slen[0] & 0xff)
	    + ((slen[1] & 0xff) << 8)
	    + ((slen[2] & 0xff) << 16)
	    + ((slen[3] & 0xff) << 24);

	 /* we start reading */
	 final byte[] string = new byte[clen];

	 file.read( string );

	 /* unserialize the object */
	 return bigloo.runtime.Unsafe.intext.string_to_obj( string, false, false );
      }
}

