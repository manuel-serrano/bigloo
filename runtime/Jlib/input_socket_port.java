/*=====================================================================*/
/*    .../prgm/project/bigloo/runtime/Jlib/input_socket_port.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 11:53:13 2000                          */
/*    Last change :  Sat Apr 25 11:18:04 2009 (serrano)                */
/*    Copyright   :  2000-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JVM Socket input ports implementation.                           */
/*=====================================================================*/
package bigloo;
import bigloo.foreign;
import java.io.*;

/*---------------------------------------------------------------------*/
/*    INPUT_SOCKET_PORT                                                */
/*---------------------------------------------------------------------*/
public class input_socket_port extends input_port {
  public final InputStream in;

  public input_socket_port( final InputStream stream, final byte[] buf ) {
     super( "[socket]", buf );
     in = stream;
  }

  public void close() throws IOException {
    eof = true;
    other_eof = true;
    buffer = null;
    in.close();
    super.close();
  }

  public boolean rgc_charready() {
    try {
      return ( (forward+1) < bufpos) || (0 < in.available());
    } catch (final Exception _) {
      return false;
    }
  }

  public boolean rgc_fill_buffer() throws IOException {
    final int bufsize = this.bufsiz;
    int bufpose = this.bufpos;
    final int matchstart = this.matchstart;

    // if the buffer is not full, we fill it */
    if (bufpose < bufsize)
      return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );

    if (0 < matchstart) {
      // we shift the buffer left and we fill the buffer */
      final byte[] buffer = this.buffer;
      final int movesize = bufpose-matchstart;

      for ( int i = 0 ; i < movesize ; ++i )
        buffer[i] = buffer[matchstart+i];

      bufpose -= matchstart;
      this.matchstart = 0;
      this.matchstop -= matchstart;
      this.forward -= matchstart;
      this.lastchar = buffer[matchstart-1];

      return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );
    }

    // we current token is too large for the buffer */
    // we have to enlarge it.                       */
    rgc_double_buffer();

    return rgc_fill_buffer();
  }

  final boolean rgc_size_fill_con_buffer( int bufpose, final int size )
    throws IOException {
    // we start reading at BUFPOSE - 1 because we have */
    // to remove the '\0' sentinel that ends the buffer */
    final byte[] buffer = this.buffer;

    // FIX Dustin DeWeese" <dustin.deweese gmail.com> Feb 2006.
    // final int nbread = in.read( buffer, bufpose-1, (a < size ? a : size) );
    final int nbread = in.read( buffer, bufpose-1, size );

    if (nbread == -1)
      eof = true;
    else
      bufpose += nbread;

    buffer[bufpose-1] = (byte)'\0';
    this.bufpos = bufpose;
    return (0 < bufpos);
  }
}
