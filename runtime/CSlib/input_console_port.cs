using System;
using System.IO;

namespace bigloo 
{    
  public sealed class input_console_port: input_port 
  {
    private Stream _in= Console.OpenStandardInput();

    public input_console_port( int  size ) : base( "[stdin]", size ) 
    {
    }

    public override bool rgc_charready() 
    {
      try
      {
        return (((forward+1) < bufpos) || (_in.Position < _in.Length));
      }
      catch (Exception)
      {
        return false;
      }
    }

    public override bool reset_eof()
    {
      eof= false;
      reset_console();
      //clearerr();
      return true;
    }

    public void reset_console()
    {
      matchstart= 0;
      matchstop= 0;
      bufpos= 1;
      buffer[0]= (byte)'\0';
      lastchar= (byte)'\n';
    }
    
    public override bool rgc_fill_buffer() 
    {
      int          bufsize= this.bufsiz;
      int          bufpose= this.bufpos;
      int          matchstart= this.matchstart;

      // if the buffer is not full, we fill it 
      if (bufpose < bufsize)
        return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );

      if (0 < matchstart) 
      {
        // we shift the buffer left and we fill the buffer 
        byte[]     buffer= this.buffer;
        int        movesize= bufpose-matchstart;

        for ( int i= 0 ; i < movesize ; ++i )
          buffer[i]= buffer[matchstart+i];
        bufpose-= matchstart;
        this.matchstart= 0;
        this.matchstop-= matchstart;
        this.forward-= matchstart;
        this.lastchar= (byte)buffer[matchstart-1];

        return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );
      }

      // we current token is too large for the buffer 
      // we have to enlarge it.                       
      rgc_double_buffer();
      return rgc_fill_buffer();
    }

    static byte[] mono_ReadByte_workaround = new byte[1];
   
    bool rgc_size_fill_con_buffer( int  bufpose,
                                   int  size )
    {
      byte[] buffer= this.buffer;

      --bufpose;
      while (0 < size)
      {
        // mono patch, mono ReadByte is buggous!
        int ch = -1, n;
        n = _in.Read( mono_ReadByte_workaround, 0, 1 );
        if( n == 1 )
          ch = (int)mono_ReadByte_workaround[ 0 ];

        if (ch == -1) 
        {
          eof= true;
          break;
        }
        buffer[bufpose++]= (byte)ch;
        --size;
        if (ch == '\n')
          break;
      }
      buffer[bufpose++]= (byte)'\0';
      this.bufpos= bufpose;

      return (0 < this.bufpos);
    }
  }
}
