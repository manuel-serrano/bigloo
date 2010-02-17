package bigloo;

import java.io.*;

public class input_file_port extends input_port
{
  public RandomAccessFile in;

  public input_file_port( final byte[] file, final byte[] buf )
    throws IOException
  {
    super( new String( file ), buf );
    in = new RandomAccessFile( new String( file ), "r" );
  }

   public void close() {
    eof = true;
    other_eof = true;
    try {
       in.close();
    } catch( Throwable _ ) {
       ;
    }
    super.close();
  }

  public boolean rgc_charready()
  {
    if (eof || other_eof)
      return false;

    try
    {
      return ((forward+1) < bufpos) || (in.getFilePointer() < in.length());
    }
    catch (final Exception _)
    {
      return false;
    }
  }

  public boolean rgc_fill_buffer()
    throws IOException
  {
    final int bufsize = this.bufsiz;
    int bufpose = this.bufpos;
    final int matchstart = this.matchstart;
    final byte[] buffer = this.buffer;

    if (0 < matchstart)
    {
      // we shift the buffer left and we fill the buffer */
      final int movesize = bufpose-matchstart;

      for ( int i= 0 ; i < movesize ; ++i )
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

  final boolean rgc_size_fill_file_buffer( int bufpose, final int  size )
    throws IOException
  {
    final int nbread = in.read( buffer, bufpose-1, size );

    if (nbread == -1)
      eof = true;
    else
      bufpose += nbread;

    this.bufpos = bufpose;

    if (0 < bufpose)
    {
      buffer[bufpose-1] = 0;
      return true;
    }

    return false;
  }

  Object bgl_input_port_seek( final int  pos )
    throws IOException
  {
    in.seek( pos );

    filepos = pos;
    eof = false;
    matchstart = 0;
    matchstop = 0;
    forward = 0;
    bufpos = 1;
    lastchar = (byte)'\n';
    buffer[0] = 0;

    return bigloo.foreign.BTRUE;
  }

  Object bgl_input_port_reopen()
    throws IOException
  {
    in.close();

    in= new RandomAccessFile( name, "r" );

    filepos = 0;
    eof = false;
    matchstart = 0;
    matchstop = 0;
    forward = 0;
    bufpos = 1;
    lastchar = (byte)'\n';
    buffer[0] = 0;

    return bigloo.foreign.BTRUE;
  }

  public void write( final output_port  p )
  {
    p.write( "#<input_file_port:" + name + ">" );
  }
}
