/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/socket.cs              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Sat Sep  6 16:08:09 2008 (serrano)                */
/*    Copyright   :  2000-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Socket implementation for the JVM back-end.                  */
/*=====================================================================*/
using System;
using System.Net.Sockets;

namespace bigloo 
{
  /*---------------------------------------------------------------------*/
  /*    SOCKET ...                                                       */
  /*---------------------------------------------------------------------*/
  public abstract class socket: obj
  {
    /*--- public fields ---------------------------------------------------*/
    public obj input;
    public obj output;

    /*--- protected fields ------------------------------------------------*/
    protected bool down= false;
    protected bool closed= false;

    /*--- constructors ----------------------------------------------------*/
    protected socket()
    {
      input= bigloo.foreign.BFALSE;
      output= bigloo.foreign.BFALSE;
    }

  /*--- protected methods -------------------------------------------------*/
    protected void socket_error( String  s1,
                                 String  s2,
                                 Object  o )
    {
      foreign.fail( s1, s2, o );
    }

    protected void set_socket_io_ports( Socket _socket, byte[] inbuf, byte[] outbuf )
    {
       input= new input_socket_port( _socket, inbuf );
       // CARE must be buffered !?
       output= new output_socket_port( _socket );
    }

    /*--- public methods --------------------------------------------------*/
    public bool DOWNP()
    {
      return down;
    }

    public virtual Object close()
    {
      if (!closed)
      {
        closed= true;
        if (input is input_port)
          ((input_port)input).close();
        if (output is output_port)
          ((output_port)output).close();
      }
      return bigloo.foreign.BUNSPEC;
    }

    /*--- public abstract methods -----------------------------------------*/
    public abstract Object HOSTNAME();
    public abstract Object HOSTIP();
    public abstract Object shutdown( bool  close_socket );
    public abstract int PORT();
    public abstract byte[] local_addr();
  }
}
