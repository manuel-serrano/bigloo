using System;

namespace bigloo
{
  using System.IO;

  public sealed class input_file_port: input_port
  {
    private FileStream _in;

    public input_file_port( byte[] file, byte[] buf )
       : base( foreign.newstring( file ), buf )
    {
      _in= new FileStream( foreign.newstring( file ), FileMode.Open, FileAccess.Read, FileShare.ReadWrite );
    }

    public override void close()
    {
      eof= true;
      other_eof= true;
      _in.Close();
      base.close();
    }

    public override bool rgc_charready()
    {
      if (eof || other_eof)
        return false;

      try
      {
        return (   (forward+1) < bufpos)
                || (_in.Position < _in.Length);
      }
      catch (Exception)
      {
        return false;
      }
    }

    public override bool rgc_fill_buffer() 
    {
      int          bufsize=    this.bufsiz;
      int          bufpose=   this.bufpos;
      int          matchstart= this.matchstart;
      byte[]       buffer=     this.buffer;

      if (0 < matchstart)
      {
        // we shift the buffer left and we fill the buffer 
        int        movesize= bufpose - matchstart;

        for ( int i= 0 ; i < movesize ; ++i )
          buffer[i]= buffer[matchstart +i];

        bufpose-= matchstart;
        this.matchstart= 0;
        this.matchstop-= matchstart;
        this.forward-= matchstart;
        this.lastchar= buffer[matchstart-1];
        return rgc_size_fill_file_buffer( bufpose, bufsize-bufpose );
      }

      if (bufpose < bufsize)
        return rgc_size_fill_file_buffer( bufpose, bufsize-bufpose );

      // we current token is too large for the buffer
      // we have to enlarge it.
      rgc_double_buffer();

      return rgc_fill_buffer();
    }

    bool rgc_size_fill_file_buffer( int  bufpose,
                                    int  size )
    {
      int          nbread= _in.Read( buffer, bufpose-1, size );

      if (nbread == 0)
        eof= true;
      else
        bufpose+= nbread;

      this.bufpos= bufpose;

      if (0 < bufpose) 
      {
        buffer[bufpose-1]= 0;
        return true;
      }

      return false;
    }

    public override object bgl_input_port_seek( int  pos )
    {
      _in.Position= pos;

      filepos= pos;
      eof= false;
      matchstart= 0;
      matchstop= 0;
      forward= 0;
      bufpos= 1;
      lastchar= (byte)'\n';
      buffer[0]= 0;

      return bbool.vrai;
    }

    public override Object bgl_input_port_reopen()
    {
      _in.Close();
      _in= new FileStream( name, FileMode.Open, FileAccess.Read, FileShare.ReadWrite );
      filepos= 0;
      eof= false;
      matchstart= 0;
      matchstop= 0;
      forward= 0;
      bufpos= 1;
      lastchar= (byte)'\n';
      buffer[0]= 0;

      return bbool.vrai;
    }

    public override void write( output_port  p )
    {
      p.write( "#<input_file_port:" + name + ">" );
    }
  }
}
