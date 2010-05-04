using System;

namespace bigloo
{
  public abstract class input_port: obj
  {
    public readonly String name;
    public int filepos = 0;
    public int pseudoeof = -1;
    public long length = -1;
    public int bufsiz;
    public bool eof = false;
    public bool other_eof = false;
    public int matchstart = 0;
    public int matchstop = 0;
    public int forward = 0;
    public byte lastchar = (byte)'\n';
    public int bufpos = 1;
    public byte[] buffer;
    public Object chook = bigloo.foreign.BUNSPEC;

    public input_port( String name, int bufsiz )
    {
      this.name= name;
      this.bufsiz= bufsiz;
      buffer= new byte[bufsiz];
    }

    public input_port( String name, byte[] buf )
    {
      this.name= name;
      this.bufsiz= buf.Length;
      buffer= buf;
    }

    public virtual void close()
    {
       if( chook is procedure )
       {
	  ((procedure)chook).funcall1(this);
       }
    }

    public abstract bool rgc_charready();

    public virtual bool reset_eof()
    {
      return false;
    }

    public abstract bool rgc_fill_buffer();

    public void rgc_enlarge_buffer_size( int  new_size )
    {
      int bufsize = this.bufsiz;

      if (new_size < bufsize)
        return;

      if (bufsize == 2)
        foreign.fail( "input-port",
                      "Can't enlarge buffer for non bufferized port (see the user manual for details)",
                      this );
      else {
        byte[] obuffer = buffer;
        byte[] nbuffer = new byte[new_size];

        for ( int i= 0 ; i < bufsize ; ++i )
          nbuffer[i]= obuffer[i];
        this.bufsiz= new_size;
        buffer= nbuffer;
      }
    }

    public void rgc_double_buffer()
    {
      rgc_enlarge_buffer_size( 2 * bufsiz );
    }

    public virtual object bgl_input_port_seek( int  pos )
    {
      return bbool.faux;
    }

    public virtual Object bgl_input_port_reopen()
    {
      return bbool.faux;
    }

    public override void write( output_port  p )
    {
      p.write( "[PORT " + name + " @" + filepos + "-" + matchstart + "." + forward + "." + matchstop + "-" + bufsiz + "." + bufpos + "]" );
    }
  }
}
