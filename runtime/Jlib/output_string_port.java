/*=====================================================================*/
/*    .../project/bigloo/5.0a/runtime/Jlib/output_string_port.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Tue Apr 28 11:40:06 2026                          */
/*    Last change :                                                    */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Output string ports                                              */
/*=====================================================================*/
package bigloo;

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
	 final byte[] result = out.toByteArray();
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
      try {
	 final byte[] result = out.toByteArray();

	 return result;
      } catch (final Exception e) {
	 if (out != null)
	    foreign.fail("flush", e.getMessage(), this);
	 return bbool.faux;
      }
   }

   public Object reset() {
      try {
	 final byte[] result = out.toByteArray();

	 out.position(0);

	 return result;
      } catch (final Exception e) {
	 if (out != null)
	    foreign.fail("flush", e.getMessage(), this);
	 return bbool.faux;
      }
   }      

   public byte[] get_string() {
      return out.toByteArray();
   }

   public Object bgl_output_port_seek(final int pos) throws IOException {
      out.position(pos);
   }
}
