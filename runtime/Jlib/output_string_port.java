/*=====================================================================*/
/*    .../project/bigloo/5.0a/runtime/Jlib/output_string_port.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Tue Apr 28 11:40:06 2026                          */
/*    Last change :  Wed Apr 29 07:01:35 2026 (serrano)                */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Output string ports                                              */
/*=====================================================================*/
package bigloo;

import java.io.*;
import java.nio.ByteBuffer;

/*---------------------------------------------------------------------*/
/*    ouput_string_port ...                                            */
/*---------------------------------------------------------------------*/
public class output_string_port extends output_port {
   ByteBuffer out;
   
   public output_string_port() {
      super("string".getBytes());
      out = ByteBuffer.allocate(256);
   }

   public Object close() {
      try {
	 final byte[] result = getBytes();
	 super.close();
	 return result;
      } catch (final Exception  e) {
	 if (out == null)
	    foreign.fail("close", "port already closed", this);
	 else
	    foreign.fail("close", e.getMessage(), this);

	 return this;
      }
   }

   public Object flush() {
      return getBytes();
   }

   public Object reset() {
      final byte[] result = getBytes();

      out.position(0);
      
      return result;
   }      

   public void write(final int cn) {
      super.write(cn);

      checkSize(1);
      out.put((byte)cn);
   }
   
   public void write(final byte[] s) {
      super.write(s);
      checkSize(s.length);
      out.put(s);
   }
   
   public void write(final byte[] s, int offset, int len) {
      super.write(s, offset, len);
      checkSize(len - offset);
      out.put(s, offset, len - offset);
   }

   public void write(String s) {
      super.write(s);
      checkSize(s.length());
      out.put(s.getBytes());
   }

   public int filepos() {
      return out.position();
   }
   
   public byte[] get_string() {
      return getBytes();
   }

   public Object bgl_output_port_seek(final int pos) throws IOException {
      return out.position(pos);
   }

   @Override
   public OutputStream getOutputStream() {
      return new ByteBufferOutputStream(out);
   }

   void checkSize(int sz) {
      if (out.remaining() < sz) {
	 
	 ByteBuffer buf = ByteBuffer.allocate(out.capacity() * 2 + sz);
	 out.flip();
	 buf.put(out);
	 out = buf;
      }
   }
   
   byte[] getBytes() {
      int pos = out.position();
      byte[] bytes = new byte[pos];
      out.flip();
      out.get(bytes);
      
      return bytes;
   }
   
}

class ByteBufferOutputStream extends OutputStream {
   private final ByteBuffer buffer;

   public ByteBufferOutputStream(ByteBuffer buffer) {
      this.buffer = buffer;
   }

   @Override
   public void write(int b) throws IOException {
      buffer.put((byte) b);
   }

   @Override
   public void write(byte[] b, int off, int len) throws IOException {
      buffer.put(b, off, len);
   }
}

