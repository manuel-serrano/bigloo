/*=====================================================================*/
/*    .../project/bigloo/5.0a/runtime/Jlib/output_random_port.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Tue Apr 28 17:13:10 2026                          */
/*    Last change :  Wed Apr 29 15:24:08 2026 (serrano)                */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Output file ports                                                */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.*;

/*---------------------------------------------------------------------*/
/*    output_random_port ...                                           */
/*---------------------------------------------------------------------*/
public class output_random_port extends output_port {
   RandomAccessFile out;
   byte[] buffer;
   int count;
   
   public output_random_port() {
      super();
      buffer = null;
      count = 0;
   }
   
   public output_random_port(final byte[] file) throws IOException {
      super();
      out = new RandomAccessFile(new String(file), "rw");
      buffer = new byte[0];
      count = 0;
   }
   
   public output_random_port(final byte[] file, byte[] _buffer, boolean append) throws IOException {
      super();
      out = new RandomAccessFile(new String(file), "rw");
      out.seek(out.length());
      buffer = _buffer;
      count = 0;
   }
   
   public output_random_port(final byte[] file, byte[] _buffer) throws IOException {
      super(file);
      out = new RandomAccessFile(new String(file), "rw");
      buffer = _buffer;
      count = 0;
   }
   
   public static output_port make_output_random_port(final byte[] file, byte[] buffer, final boolean app) throws IOException {
      if (buffer == null || buffer.length == 0) {
	 String fname = new String(file);
	 return new output_stream_port(new FileOutputStream(fname, app));
      } else {
	 return new output_random_port(file, buffer, app);
      }
   }
   
   public static output_port make_output_random_port(final byte[] file, byte[] buffer) throws IOException {
      if (buffer == null || buffer.length == 0) {
	 return new output_random_port(file);
      } else {
	 return new output_random_port(file, buffer);
      }
   }
   
   public Object close() {
      flush();
      try {
	 out.close();
      } catch(Throwable t) {
	 ;
      }
      return super.close();
   }
   
   public Object flush() {
      if (count > 0) {
	 invoke_flush_hook(bigloo.foreign.BINT(count));
	 try {
	    out.write(buffer, 0, count);
	 } catch(Throwable _t) {
	    ;
	 }
	 count = 0;
      }

      try {
	 out.getFD().sync();
      } catch(Throwable _t) {
	 return bbool.faux;
      }
      
      return bbool.vrai;
   }
   
   public void write(final int cn) {
      try {
	 if (count < buffer.length) {
	    buffer[ count++ ] = (byte)cn;
	 } else {
	    if (count > 0) invoke_flush_hook(bigloo.foreign.BINT(count));
	    out.write(buffer);
	    buffer[ 0 ] = (byte)cn;
	    count = 1;
	 }
      } catch (final Exception e) {
	 if (out != null) foreign.fail("write", e, this);
      }
   }

   public void write(final byte[] s) {
      try {
	 if ((count + s.length) < buffer.length) {
	    System.arraycopy(s, 0, buffer, count, s.length);
	    count += s.length;
	 } else {
	    if (count > 0) {
	       invoke_flush_hook(bigloo.foreign.BINT(count));
	       out.write(buffer, 0, count);
	       count = 0;
	    }

	    if (s.length > 0)
	       invoke_flush_hook(bigloo.foreign.BINT(s.length));
	    out.write(s);
	 }
      } catch(final Exception e) {
	 if (out != null) foreign.fail("write", e, this);
      }
   }

   public void write(final byte[] s, int offset, int len) {
      try {
	 final int l = len - offset;
	 
	 if ((count + l) < buffer.length) {
	    System.arraycopy(s, offset, buffer, count, l);
	    count += (len - offset);
	 } else {
	    if (count > 0) {
	       invoke_flush_hook(bigloo.foreign.BINT(count));
	       out.write(buffer, 0, count);
	       count = 0;
	    }

	    if (l > 0) invoke_flush_hook(bigloo.foreign.BINT(l));
	    out.write(s, offset, l);
	 }
      } catch (final Exception e) {
	 if (out != null) foreign.fail("write", e, this);
      }
   }

   public void write(final String s) {
      final int len = s.length();
      
      try {
	 if ((count + len) < buffer.length) {
	    for (int i = 0 ;i < len ; ++i) {
	       buffer[ count++ ] = (byte)s.charAt(i);
	    }
	 } else {

	    if (count > 0) invoke_flush_hook(bigloo.foreign.BINT(count));
	    out.write(buffer, 0, count);
	    count = 0;

	    if (len > 0) invoke_flush_hook(bigloo.foreign.BINT(len));
	    for (int i = 0 ;i < len ; ++i) {
	       out.write((byte)s.charAt(i));
	    }
	 }
      } catch (final Exception e) {
	 if (out != null) foreign.fail("write", e, this);
      }
   }

   public boolean truncate(long size) {
      try {
	 out.setLength(size);
      } catch (Throwable t) {
	 return false;
      }
      return true;
   }

   public int filepos() {
      try {
         return (int)out.getFilePointer();
      } catch (IOException e) {
         return 0;
      }
   }
   
   public OutputStream getOutputStream() {
      try {
	 return new FileOutputStream(out.getFD());
      } catch (Throwable t) {
	 return null;
      }
   }
}
