/*=====================================================================*/
/*    /users2GB/serrano2/bigloo/runtime/CSlib/binary_port.cs           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Dec  3 11:33:29 2000                          */
/*    Last change :  Thu Feb  1 19:35:05 2007 (serrano)                */
/*    Copyright   :  2000-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The binary port JVM connection                                   */
/*=====================================================================*/
using System;
using System.IO;

namespace bigloo 
{
   /*---------------------------------------------------------------------*/
   /*    BINARY_PORT ...                                                  */
   /*---------------------------------------------------------------------*/
   public sealed class binary_port: obj 
   {
      private static readonly byte[] MAGIC_WORD= foreign.getbytes( "1966" );
      public readonly Stream stream;

      public binary_port( Stream  stream ) 
	 {
	    this.stream= stream;
	 }

      public binary_port close() 
	 {
	    stream.Close();
	    return this;
	 }

      public binary_port flush() 
	 {
	    stream.Flush();
	    return this;
	 }

      public override void write( output_port  p ) 
	 {
	    p.write( "#<binary:port:" );
	    p.write( stream.ToString() );
	    p.write( ">" );
	 }

      public obj output_obj( Object  obj )
	 {

	    // the magic key
	    byte[] _string= foreign.__cb__.obj_to_string( obj );

	    stream.Write( MAGIC_WORD, 0, 4 );

	    // the object length
	    byte[] slen= new byte[4];
	    int clen= _string.Length;

	    slen[0]= (byte)clen;
	    slen[1]= (byte)(clen >> 8);
	    slen[2]= (byte)(clen >> 16);
	    slen[3]= (byte)(clen >> 24);

	    stream.Write( slen, 0, 4 );

	    // the serialized object
	    stream.Write( _string, 0, clen );

	    return this;
	 }

      public Object input_obj() 
	 {
	    // magic key
	    byte[] magic= new byte[4];
	    int size= stream.Read( magic, 0, 4 );

	    if (size == -1)
	       return eof._eof;

	    if ( (size != 4)
		 || (magic[0] != MAGIC_WORD[0])
		 || (magic[1] != MAGIC_WORD[1])
		 || (magic[2] != MAGIC_WORD[2])
		 || (magic[3] != MAGIC_WORD[3]))
	       foreign.fail( "input_obj", "corrupted file", this );

	    // the object length
	    byte[] slen= new byte[4];

	    size= stream.Read( slen, 0, 4 );
	    if (size != 4)
	       foreign.fail( "input_obj", "corrupted file", this );

	    int clen= (slen[0] & 0xff)
	       + ((slen[1] & 0xff) << 8)
	       + ((slen[2] & 0xff) << 16)
	       + ((slen[3] & 0xff) << 24);

	    // we start reading
	    byte[] _string= new byte[clen];

	    stream.Read( _string, 0, clen );
      
	    // unserialize the object
	    return foreign.__cb__.string_to_obj( _string );
	 }
   }
}
