package bigloo;

import java.io.*;

public class input_gzip_port extends input_procedure_port {
   public input_port input_port;
   public input_gzip_port( final procedure p, input_port inp, byte[] buf ) {
      super( p, "[gzip]", buf );
      input_port = inp;
      in = p;
   }
}

