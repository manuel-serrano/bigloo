 /*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/flusher.java            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec  8 17:13:14 2000                          */
/*    Last change :  Mon Oct 24 13:47:06 2016 (serrano)                */
/*    Copyright   :  2000-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    A simple class that implements object that flush two output      */
/*    ports.                                                           */
/*=====================================================================*/
package bigloo;

import java.io.InputStream;
import java.io.IOException;
import java.io.PrintStream;


/*---------------------------------------------------------------------*/
/*    FLUSHER ...                                                      */
/*---------------------------------------------------------------------*/
final class flusher implements Runnable {
   private final InputStream input_stream;
   private final PrintStream output_stream;
   private final Thread thread;

   public flusher( final InputStream input_stream,
		   final PrintStream output_stream ) {
      this.input_stream= input_stream;
      this.output_stream= output_stream;
      thread= new Thread( this );
      thread.start();
   }

   public void join()
      {
	 try {
	    thread.join();
	 } catch (final InterruptedException _i) {
	 }
      }

   public void run() {
      try {
	 final byte[] buffer= new byte[1024];
	 int len;

	 do {
	    len= input_stream.read( buffer );
	    if (0 < len)
	       output_stream.write( buffer, 0, len );
	 } while (len == buffer.length);
      } catch (final IOException _i) {
      }
   }
}
