package bigloo;

import java.io.*;

public class input_console_port extends input_port
{
  public InputStream in = System.in;

  public input_console_port( final byte[] buf )
  {
    super( "[stdin]", buf );
  }

  public boolean rgc_charready()
  {
    try
    {
      return ( (forward+1) < bufpos) || (0 < in.available());
    }
    catch (final Exception _e)
    {
      return false;
    }
  }

  public boolean reset_eof()
  {
    eof= false;
    reset_console();
    //clearerr();
    return true;
  }

  public final void reset_console()
  {
    matchstart = 0;
    matchstop = 0;
    bufpos = 1;
    lastchar = (byte)'\n';
  }

  public boolean rgc_fill_buffer()
    throws IOException
  {
    final int bufsize = this.buffer.length;
    int bufpose = this.bufpos;
    final int matchstart = this.matchstart;

    // if the buffer is not full, we fill it */
    if (bufpose < bufsize)
      return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );

    if (0 < matchstart)
    {
      // we shift the buffer left and we fill the buffer */
      final byte[] buffer = this.buffer;
      final int movesize = bufpose - matchstart;

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
    throws IOException
  {
    final byte[] buffer = this.buffer;

    // we start reading at BUFPOSE - 1 because we have */
    // to remove the '\0' sentinel that ends the buffer */
    final int nbread = in.read( buffer, bufpose, size );

    if (nbread == -1)
      eof= true;
    else
      bufpose += nbread;

    this.bufpos = bufpose;
    return (0 < this.bufpos);
  }
   
  public Object bgl_input_port_clone( input_port src ) {
     super.bgl_input_port_clone( src );
     in = ((input_console_port)src).in;

     return this;
  }
   
}
