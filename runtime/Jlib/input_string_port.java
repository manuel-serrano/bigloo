package bigloo;

import java.io.*;

public class input_string_port extends input_port {
   int offset;
   
   public input_string_port( final byte[] s, int start, int end ) {
      super( "[string]", new byte[ end - start ] );

      final int size = end - start;
      length = size;
      
      for ( int i= 0 ; i < size ; ++i ) buffer[i] = s[i + start];
      
      bufpos = size;
      offset = start;
      eof = true;
   }

   public input_string_port( final byte[] s, int start, int end, boolean _b ) {
      super( "[string]", s );
      int size = end - start;
      
      length = size;
      
      matchstart = start;
      matchstop = start;
      
      bufpos = end;
      forward = start;
      eof = true;
   }

   public boolean rgc_charready() {
      return ((forward) < bufpos);
   }

   public void reopen_input_c_string( final byte[] s ) {
      int len = s.length;

      if (buffer.length < (len + 1)) {
	 buffer = new byte[len];
      }

      bufpos = len;
      matchstart = 0;
      matchstop = 0;
      forward = 0;
      lastchar = (byte)'\n';
      --len;

      while (0 <= len) {
	 buffer[len]= s[len];
	 --len;
      }
   }

   Object bgl_input_port_seek( final int  pos ) throws IOException {
      if (pos < buffer.length) {
	 filepos = pos + offset;
	 matchstart = pos + offset;
	 matchstop = pos + offset;
	 forward = pos + offset;
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
