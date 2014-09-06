package bigloo;

import java.io.*;

public class input_string_port extends input_port {
   public input_string_port( final byte[] s, int start, int end ) {
      super( "[string]", new byte[ end+1 - start ] );

      final int size = end - start;
      length = size;
      
      for ( int i= 0 ; i < size ; ++i ) buffer[i] = s[i + start];
      
      bufpos = bufsiz;
      buffer[size] = 0;
      eof = true;
   }

   public input_string_port( final byte[] s, int start, int end, byte c0 ) {
      super( "[string]", s );
      int size = end - start;
      
      length = size;
      bufsiz = size + 1;
      
      s[size + start] = 0;
      s[0] = c0;
      
      matchstart = start;
      matchstop = start;
      
      bufpos = bufsiz;
      eof = true;
   }

   public boolean rgc_charready() {
      return ((forward+1) < bufpos);
   }

   public void reopen_input_c_string( final byte[] s ) {
      int len = s.length;

      if (bufsiz < (len + 1)) {
	 bufsiz = len + 1;
	 buffer = new byte[len + 1];
      }

      bufpos = len + 1;
      matchstart = 0;
      matchstop = 0;
      forward = 0;
      lastchar = (byte)'\n';
      buffer[len] = 0;
      --len;

      while (0 <= len) {
	 buffer[len]= s[len];
	 --len;
      }
   }

   Object bgl_input_port_seek( final int  pos ) throws IOException {
      if (pos < bufsiz) {
	 filepos = pos;
	 matchstart = pos;
	 matchstop = pos;
	 forward = pos;
	 return foreign.BTRUE;
      }

      return foreign.BFALSE;
   }

   Object bgl_input_port_reopen() throws IOException {
      eof = false;
      filepos = 0;
      matchstart = 0;
      matchstop = 0;
      forward = 0;
      lastchar = (byte)'\n';
      return bigloo.foreign.BTRUE;
   }

   public void close() {
      eof = true;
      other_eof= true;
      super.close();
   }

   public boolean rgc_fill_buffer() {
      return false;
   }
}
