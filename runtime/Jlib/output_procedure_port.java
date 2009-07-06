package bigloo;

import java.io.*;

public class output_procedure_port extends output_port {
   procedure proc, flush, close;
   
   public output_procedure_port( procedure p, procedure f, procedure c ) {
      super();
      proc = p;
      flush = f;
      close = c;
   }

   public Object close() {
      foreign.eval_funcall_0( this.close );
      if( chook instanceof procedure )
      {
	 ((procedure)chook).funcall1(this);
      }
      return this;
   }

   public Object flush() {
      return foreign.eval_funcall_0( this.flush );
   }

   public void write( final int cn ) {
      byte[] b = new byte[ 1 ];
      b[ 0 ] = (byte)cn;
      foreign.eval_funcall_1( this.proc, b );
   }

   public void write( final byte[] s ) {
      foreign.eval_funcall_1( this.proc, s );
   }

   public void write( final byte[] s, int offset, int len ) {
      final byte[] tmp = new byte[ len - offset ];

      System.arraycopy( s, offset, tmp, 0, len );
      foreign.eval_funcall_1( this.proc, tmp );
   }

   public void write( final String s ) {
      foreign.eval_funcall_1( this.proc, s.getBytes() );
   }

   public void write( final output_port  p ) {
      String s = "#<output_port:" + new String( name ) + ">";
      foreign.eval_funcall_1( this.proc, s.getBytes() );
   }
}
