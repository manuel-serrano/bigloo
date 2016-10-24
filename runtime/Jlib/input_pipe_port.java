/*=====================================================================*/
/*    .../prgm/project/bigloo/runtime/Jlib/input_pipe_port.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec  9 11:49:41 2000                          */
/*    Last change :  Mon Oct 24 13:44:07 2016 (serrano)                */
/*    Copyright   :  2000-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo JVM input pipe ports.                                     */
/*=====================================================================*/
package bigloo;

import java.lang.*;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    input_pipe_port ...                                              */
/*---------------------------------------------------------------------*/
public class input_pipe_port extends input_port {
   public InputStream in;

   /*--- public constructors ---------------------------------------------*/
   public input_pipe_port( final InputStream in, final byte[] c ) {
      super( new String( c ), new byte[ 1024 ] );
      this.in = in;
   }

   public input_pipe_port( final byte[] cmd, final byte[] buf ) {
      super( new String( cmd ), buf );

      try {
	 String[] scmd = new String[ 3 ];

	 scmd[ 0 ] = "/bin/sh";
	 scmd[ 1 ] = "-c";
	 scmd[ 2 ] = new String( cmd );

	 final Process process = Runtime.getRuntime().exec( scmd );

	 in = process.getInputStream();
      } catch (final IOException _i) {
	 final String scmd = new String( cmd, 2, cmd.length-2 );

	 foreign.fail("open-input-file","Can't execute command",scmd.getBytes());
      }
   }

   public input_pipe_port( final String cmd, final byte[] buf ) {
      super( cmd, buf );

      try {
	 String[] scmd = new String[ 3 ];

	 scmd[ 0 ] = "/bin/sh";
	 scmd[ 1 ] = "-c";
	 scmd[ 2 ] = new String( cmd );

	 final Process process = Runtime.getRuntime().exec( scmd );

	 in= process.getInputStream();
      } catch (final IOException _i) {
	 foreign.fail("open-input-file", "Can't execute command", cmd.getBytes());
      }
   }

   /*--- public methods --------------------------------------------------*/
   public void close() {
      super.close();
      eof = true;
      other_eof = true;
      try {
	 in.close();
      } catch( Throwable _t ) {
	 ;
      }
   }

   public boolean rgc_charready() {
      try {
	 return (((forward+1) < bufpos) || (0 < in.available()));
      } catch (Exception _e) {
	 return false;
      }
   }

   public boolean rgc_fill_buffer()
      throws IOException {
      final int bufsize = this.buffer.length;
      int bufpose = this.bufpos;
      final int matchstart = this.matchstart;
      final byte[] buffer = this.buffer;

      if (0 < matchstart) {
	 // we shift the buffer left and we fill the buffer */
	 final int movesize = bufpose-matchstart;

	 for ( int i = 0 ; i < movesize ; ++i )
	    buffer[i] = buffer[matchstart+i];

	 bufpose -= matchstart;
	 this.matchstart = 0;
	 this.matchstop -= matchstart;
	 this.forward -= matchstart;
	 this.lastchar = buffer[matchstart-1];

	 return rgc_size_fill_file_buffer( bufpose, bufsize-bufpose );
      }

      if (bufpose < bufsize)
	 return rgc_size_fill_file_buffer( bufpose, bufsize-bufpose );

      // we current token is too large for the buffer */
      // we have to enlarge it.                       */
      rgc_double_buffer();

      return rgc_fill_buffer();
   }

   final boolean rgc_size_fill_file_buffer( int bufpose, final int size )
      throws IOException {
      final int nbread = in.read( buffer, bufpose, size );

      if (nbread == -1)
	 eof = true;
      else
	 bufpose += nbread;

      this.bufpos = bufpose;

      if (0 < bufpose) {
	 return true;
      }

      return false;
   }

   public void write( final output_port  p ) {
      p.write( "#<input_pipe_port:" + name + ">" );
   }

   public Object bgl_input_port_clone( input_port src ) {
      super.bgl_input_port_clone( src );
      in = ((input_pipe_port)src).in;

      return this;
   }
   
   /*--- static methods --------------------------------------------------*/
   static boolean pipe_name_p( final byte[] name ) {
      return ( (name[ 0 ] == (byte)'|') && (name[ 1 ] == (byte)' ') );
   }
}
