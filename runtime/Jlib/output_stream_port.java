/*=====================================================================*/
/*    .../project/bigloo/5.0a/runtime/Jlib/output_stream_port.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Tue Apr 28 18:22:05 2026                          */
/*    Last change :  Thu Apr 30 12:40:18 2026 (serrano)                */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Output stream ports                                              */
/*=====================================================================*/
package bigloo;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    output_stream_port ...                                           */
/*---------------------------------------------------------------------*/
public class output_stream_port extends output_port {
   OutputStream out;

   public output_stream_port(byte[] name) {
      super(name);
   }

   public output_stream_port(OutputStream _out) {
      super();
      out = _out;
   }
   
   public output_stream_port(OutputStream _out, byte[] name) {
      super(name);
      out = _out;
   }
   
   public Object close() {
      try {
         out.close();
      } catch(Throwable _t) {
         ;
      }

      return super.close();
   }
   
   public Object flush() {
      try {
         out.flush();
         return bbool.vrai;
      } catch (final Exception e) {
         if (out != null)
            foreign.fail("flush", e, this);
         return bbool.faux;
      }
   }

   public Object bgl_output_port_seek(final int pos) throws IOException {
      return bigloo.foreign.BFALSE;
   }

   protected void invoke_flush_hook(bigloo.bint size) {
      if (fhook instanceof procedure) {
	 Object s = ((procedure)fhook).funcall2(this, size);

	 try {
	    if (s instanceof byte[]) {
	       out.write((byte [])s, 0, ((byte [])s).length);
	    } else {
	       if (s instanceof bigloo.bint &&
		   flushbuf instanceof byte[] &&
		   bigloo.foreign.CINT((bigloo.bint)s) <= ((byte[])flushbuf).length &&
		   bigloo.foreign.CINT((bigloo.bint)s) > 0)
	       {
		  out.write((byte[])flushbuf,
			    0,
			    bigloo.foreign.CINT((bigloo.bint)s));
	       }
	       
	    }
	 } catch(Throwable _t) {
	    ;
	 }
      }
   }
      
   public void write(final int cn) {
      try {
	 invoke_flush_hook(bigloo.foreign.BINT(1));
	 out.write(cn);
      } catch (final Exception e) {
	 if (out != null)
	    foreign.fail("write", e, this);
      }
   }

   public void write(final byte[] s) {
      try {
	 invoke_flush_hook(bigloo.foreign.BINT(s.length));
	 out.write(s, 0, s.length);
      } catch (final Exception e) {
	 if (out != null)
	    foreign.fail("write", e, this);
      }
   }

   public void write(final byte[] s, int offset, int len) {
      try {
	 invoke_flush_hook(bigloo.foreign.BINT(len - offset));
	 out.write(s, offset, (len - offset));
      } catch (final Exception e) {
	 if (out != null)
	    foreign.fail("write", e, this);
      }
   }

   public void write(final String s) {
      try {
	 final int len = s.length();

	 invoke_flush_hook(bigloo.foreign.BINT(len));
	 
	 for (int i= 0 ;i < len ; ++i)
	    out.write((byte)s.charAt(i));
      } catch (final Exception e) {
	 if (out != null)
	    foreign.fail("write", e, this);
      }
   }

   public void write(final output_port p) {
      p.write("#<output_port:" + new String(name) + ">");
   }

   public boolean truncate(long size) {
      if (out instanceof FileOutputStream) {
	 try {
	    return JDK.truncate((FileOutputStream)out, size);
	 } catch(Exception _e) {
	    return false;
	 }
      } else {
	 return false;
      }
   }

   public OutputStream getOutputStream() {
      return out;
   }
}
