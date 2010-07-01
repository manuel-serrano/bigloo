package bigloo;

import java.io.*;

public abstract class input_port extends obj
{
  public final String name;
  public int filepos = 0;
  public int pseudoeof = -1;
  public long length = -1;
  public int bufsiz;
  public boolean eof = false;
  public boolean other_eof = false;
  public int matchstart = 0;
  public int matchstop = 0;
  public int forward = 0;
  public byte lastchar = (byte)'\n';
  public int bufpos = 1;
  public byte[] buffer;
  public Object chook = bigloo.foreign.BUNSPEC;

  public input_port( final String name, final byte[] buf )
  {
    this.name= name;
    this.bufsiz= (int)buf.length;
    buffer= buf;
    buf[0] = (byte)'\0';
  }

  public void close() 
  {
    if( chook instanceof procedure )
     {
	((procedure)chook).funcall1(this);
     }
  }

  public abstract boolean rgc_charready();

  public boolean reset_eof()
  {
    return false;
  }

  public abstract boolean rgc_fill_buffer()
    throws IOException;

   final void rgc_enlarge_buffer_size( int nsize ) {
    final int bufsize= this.bufsiz;

    if( nsize < bufsize ) return;
    
    if (bufsize == 2)
      foreign.fail( "input-port",
                    "Can't enlarge buffer for non bufferized port (see the user manual for details)",
                    this );
    else
    {
      final byte[] obuffer= buffer;
      final byte[] nbuffer= new byte[nsize];

      for ( int i= 0 ; i < bufsize ; ++i )
        nbuffer[i]= obuffer[i];
      this.bufsiz= nsize;
      buffer= nbuffer;
    }
  }

   public final void rgc_double_buffer() {
      rgc_enlarge_buffer_size( this.bufsiz * 2 );
   }
   
  Object bgl_input_port_seek( final int pos )
    throws IOException
  {
    return bigloo.foreign.BFALSE;
  }

  Object bgl_input_port_reopen()
    throws IOException
  {
    return bigloo.foreign.BFALSE;
  }

  public void write( final output_port p )
  {
    p.write( "[PORT " + name + " @" + filepos + "-" + matchstart + "." + forward + "." + matchstop + "-" + bufsiz + "." + bufpos + "]" );
  }

   public boolean timeout_set( int to ) {
      return false;
   }
}
