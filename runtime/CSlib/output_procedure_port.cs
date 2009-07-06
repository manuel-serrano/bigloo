using System;
using System.IO;

namespace bigloo {
   public sealed class output_procedure_port: output_stream_port {
      procedure proc, flushproc, closeproc;
      public output_procedure_port(procedure p, procedure f, procedure c)
	 : base( new MemoryStream(), foreign.getbytes( "string" ) ) {
	 proc = p;
	 flushproc = f;
	 closeproc = c;
      }

      private static byte[] getbytes( String s ) {
	 int len = s.Length;
	 byte[] r = new byte[ len ];

	 for ( int i= 0 ; i < len ; ++i )
	    r[ i ]= (byte)s[ i ];
	 return r;
      }

      public override Object close() {
	 base.close();
	 foreign.eval_funcall_0( closeproc );
	 return this;
      }

      public override Object flush() {
	 foreign.eval_funcall_0( flushproc );
	 return this;
      }
       
      public override void write( int cn ) {
	 byte[] b = new byte[ 1 ];
	 b[ 0 ] = (byte)cn;
	 foreign.eval_funcall_1( proc, b );
      }

      public override void write( byte[] s ) {
	 foreign.eval_funcall_1( proc, s );
      }

      public override void write( byte[] s, int start, int len ) {
	 byte[] tmp = new byte[ len ];
	 int i, j;

	 for( i = start, j = 0; i < len; i++, j++ ) {
	    tmp[ j ] = s[ i ];
	 }
	 foreign.eval_funcall_1( proc, tmp );
      }

      public override void write( String s ) {
	 foreign.eval_funcall_1( proc, getbytes( s ) );
      }

      public override void write( output_port  p ) {
	 String s = "#<output_port: " + foreign.newstring( name ) + ">";
	 foreign.eval_funcall_1( proc, getbytes( s ) );
      }
   }
}
