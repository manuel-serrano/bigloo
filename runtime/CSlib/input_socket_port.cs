/*=====================================================================*/
/*    .../prgm/project/bigloo/runtime/CSlib/input_socket_port.cs       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 11:53:13 2000                          */
/*    Last change :  Mon Aug 25 12:27:58 2008 (serrano)                */
/*    Copyright   :  2000-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JVM Socket input ports implementation.                           */
/*=====================================================================*/
using System;
using System.Net.Sockets;
using System.IO;

namespace bigloo
{
  /*---------------------------------------------------------------------*/
  /*    INPUT_SOCKET_PORT                                                */
  /*---------------------------------------------------------------------*/
  public sealed class input_socket_port: input_port 
  {
    /*--- private fields --------------------------------------------------*/
    private readonly Socket _socket;

    /*--- constructors ----------------------------------------------------*/
    public input_socket_port( Socket _socket, byte[] b ) : base( "[socket]", b )
    {
      this._socket= _socket;
    }

    /*--- public methods --------------------------------------------------*/
    public override void close()
    {
      eof= true;
      other_eof = true;
      buffer= null;
      _socket.Close();
      base.close();
    }

    public override bool rgc_charready() 
    {
      try
      { 
        return (0 < _socket.Available);
      } 
      catch (Exception)
      {
        return false;
      }
    }

    public override bool rgc_fill_buffer()
    {
      int bufsize = this.bufsiz;
      int bufpose = this.bufpos;
      int matchstart = this.matchstart;

      // if the buffer is not full, we fill it
      if (bufpose < bufsize)
        return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );

      if (0 < matchstart) 
      {
        // we shift the buffer left and we fill the buffer
        byte[] buffer = this.buffer;
        int movesize = bufpose-matchstart;

        for ( int i= 0 ; i < movesize ; ++i)
          buffer[i] = buffer[matchstart + i];
        bufpose -= matchstart;
        this.matchstart = 0;
        this.matchstop -= matchstart;
        this.forward -= matchstart;
        this.lastchar = buffer[matchstart-1];

        return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );
      }

      // we current token is too large for the buffer
      // we have to enlarge it.                      
      rgc_double_buffer();

      return rgc_fill_buffer();
    }
    
    bool rgc_size_fill_con_buffer( int bufpose, int size )
    {
      // we start reading at BUFPOSE - 1 because we have
      // to remove the '\0' sentinel that ends the buffer
      byte[] buffer = this.buffer;
      int a = _socket.Available;
      int nbread = _socket.Receive( buffer,
				    bufpose-1,
				    a < size ? a : size,
				    SocketFlags.None );

      if (nbread == 0)
        eof = true;
      else
        bufpose += nbread;

      buffer[bufpose-1] = (byte)'\0';
      this.bufpos = bufpose;

      return (0 < bufpos);
    }
  }
}
