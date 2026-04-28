/*=====================================================================*/
/*    .../prgm/project/bigloo/5.0a/runtime/Jlib/output_port.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Tue Apr 28 11:39:51 2026                          */
/*    Last change :                                                    */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Output ports                                                     */
/*=====================================================================*/
package bigloo;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    output_port ...                                                  */
/*---------------------------------------------------------------------*/
public class output_port extends obj {
   public byte[] name;
   public Object chook = bigloo.foreign.BUNSPEC;
   public Object fhook = bigloo.foreign.BUNSPEC;
   public Object flushbuf = bigloo.foreign.BUNSPEC;
   public boolean isclosed = false;

   public output_port() {
      name = "???".getBytes();
   }
   
   public output_port(final byte[] _name) {
      name = _name;
   }

   public output_port(final byte[] file, final boolean append) throws IOException {
      this(new FileOutputStream( new String(file), append));
      name = file;
   }

   public Object close() {
      if (chook instanceof procedure) {
         ((procedure)chook).funcall1(this);
      }

      return this;
   }

   public Object flush() {
      try {
         return bbool.vrai;
      } catch (final Exception e) {
         return bbool.faux;
      }
   }

   public Object bgl_output_port_seek(final int pos) throws IOException {
      return bigloo.foreign.BFALSE;
   }

   protected void invoke_flush_hook(bigloo.bint size) {
      if (fhook instanceof procedure) {
	 Object s = ((procedure)fhook).funcall2(this, size);
      }
   }
      
   public void write(final int cn) {
      invoke_flush_hook(bigloo.foreign.BINT(1));
   }

   public void write(final byte[] s) {
      invoke_flush_hook(bigloo.foreign.BINT(s.length));
   }

   public void write(final byte[] s, int offset, int len) {
      invoke_flush_hook(bigloo.foreign.BINT( len - offset));
   }

   public void write(final String s) {
      invoke_flush_hook(bigloo.foreign.BINT(len));
   }

   public void write(final output_port p) {
      p.write("#<output_port:" + new String(name) + ">");
   }

   public boolean truncate(long size) {
      return false;
   }
}
