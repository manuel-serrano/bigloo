/*=====================================================================*/
/*    .../bigloo/5.0a/runtime/Jlib/output_buffered_port.java           */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Apr 29 08:11:07 2026                          */
/*    Last change :  Wed Apr 29 18:56:47 2026 (serrano)                */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Buffered OutputStream                                            */
/*=====================================================================*/
package bigloo;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    output_buffered_port ...                                         */
/*                                                                     */
/*---------------------------------------------------------------------*/
public class output_buffered_port extends output_stream_port {
   byte[] buffer;
   int count;
   
   public output_buffered_port(final OutputStream stream, byte[] _buffer, final byte[] name) {
      super(stream, name);
      buffer = _buffer;
      count = 0;
   }
   
   public static output_port make_output_buffered_port(final OutputStream stream, byte[] buffer, final byte[] name) {
      if (buffer == null || buffer.length == 0) {
	 return new output_stream_port(stream, name);
      } else {
	 return new output_buffered_port(stream, buffer, name);
      }
   }
   
   public Object close() {
      flush();
      
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
	 super.flush();
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

	    if (s.length > 0) {
	       invoke_flush_hook(bigloo.foreign.BINT(s.length));
	    }
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
}
