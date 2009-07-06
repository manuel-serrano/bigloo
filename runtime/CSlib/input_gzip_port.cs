using System;

namespace bigloo {
   public class input_gzip_port: input_procedure_port {
      public input_port input_port;
      
      public input_gzip_port( procedure p, input_port inp, byte[] buf )
	 : base( p, "[procedure]", buf  ) {
	 input_port = inp;
	 _in= p;
      }
   }
}

