/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/stackwriter.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jan 31 21:00:51 2001                          */
/*    Last change :  Wed Nov  8 17:15:43 2006 (serrano)                */
/*    Copyright   :  2001-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The PrintWriter to display print stack trace                     */
/*=====================================================================*/

package bigloo;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    stackwriter ...                                                  */
/*---------------------------------------------------------------------*/
public class stackwriter extends PrintWriter {
   final OutputStream _out;

   public stackwriter( final OutputStream _out, final boolean _flush ) {
      super( _out, _flush );
      this._out = _out;
   }

   private int next_index( final char[] s, int start ) {
      final int len = s.length;

      while( start < len ) {
	 if (!Character.isJavaIdentifierPart( s[start] ))
	    return start;
	 ++start;
      }
      return len;
   }

   private byte[] char_to_byte( final char[] src,
				final int start,
				final int len ) {
      final int nlen = len-start;
      final byte[] res = new byte[nlen];

      for( int i = 0 ; i < nlen ; ++i )
	 res[i]= (byte)src[start + i];

      return res;
   }

   private void demangle( final byte[] id ) throws IOException {
      if( bigloo.runtime.Llib.bigloo.bigloo_mangledp( id ) ) {
	 final byte[] did = (byte[])bigloo.runtime.Llib.bigloo.bigloo_demangle( id );

	 for ( int i= 0 ; i < did.length ; ++i )
	    _out.write( did[ i ] );
      }
      else
	 for( int i= 0 ; i < id.length ; ++i )
	    _out.write( id[i] );
   }

   private void print_demangle( final char[] s ) {
      int start = 0;
      final int len = s.length;
      int next= 0;

      try {
	 while (start < len) {
	    next= next_index( s, start );
	    if (start < next)
	       demangle( char_to_byte( s, start, next ) );
	    if (next < len)
	       _out.write( s[next] );
	    start= next + 1;
	 }
	 _out.flush();
      } catch (final Exception e) {
	 e.printStackTrace();
      }
   }

   public void print( final char[] s ) {
      print_demangle( s );
   }

   public void print( final String s ) {
      print_demangle( s.toCharArray() );
   }

   public void println( final char[] s ) {
      print_demangle( s );
      try {
	 _out.write( "\n".getBytes() );
      } catch (final Exception e) {
	 e.printStackTrace();
      }
   }

   public void write( final String s ) {
      print_demangle( s.toCharArray() );
   }
}
