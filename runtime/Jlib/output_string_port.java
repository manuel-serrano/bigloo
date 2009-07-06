package bigloo;

import java.io.*;

public class output_string_port extends output_port {
   public output_string_port() {
      super( new ByteArrayOutputStream() );
      name= "string".getBytes();
   }

   public Object close() {
      try {
	 final byte[] result= ((ByteArrayOutputStream)out).toByteArray();
	 super.close();
	 return result;
      } catch (final Exception  e) {
	 if (out == null)
	    foreign.fail( "close", "port already closed", this );
	 else
	    foreign.fail( "close", e.getMessage(), this );

	 return this;
      }
   }

   public Object flush() {
      try {
	 final byte[] result = ((ByteArrayOutputStream)out).toByteArray();

	 out.flush();

	 return result;
      } catch (final Exception e) {
	 if (out != null)
	    foreign.fail( "flush", e.getMessage(), this );
	 return bbool.faux;
      }
   }

   public Object reset() {
      try {
	 final byte[] result = ((ByteArrayOutputStream)out).toByteArray();

	 ((ByteArrayOutputStream)out).reset();

	 return result;
      } catch (final Exception e) {
	 if (out != null)
	    foreign.fail( "flush", e.getMessage(), this );
	 return bbool.faux;
      }
   }      

   public byte[] get_string() {
      return ((ByteArrayOutputStream)out).toByteArray();
   }
}
