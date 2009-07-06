/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/flusher.java            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec  8 17:13:14 2000                          */
/*    Last change :  Fri Jul  6 15:14:40 2001 (serrano)                */
/*    Copyright   :  2000-01 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    A simple class that implements object that flush two output      */
/*    ports.                                                           */
/*=====================================================================*/
using System;
using System.IO;
using System.Threading;

namespace bigloo
{
  public sealed class flusher
  {
    private readonly StreamReader input_stream;
    private readonly TextWriter output_stream;
    private readonly Thread flusher_thread;

    public flusher( StreamReader  input_stream,
                    TextWriter    output_stream )
    {
      this.input_stream= input_stream;
      this.output_stream= output_stream;
      flusher_thread= new Thread( new ThreadStart( Start ) );
      flusher_thread.Start();
    }

    public void Join()
    {
      flusher_thread.Join();
    }

    private void Start()
    {
      try
      {
        char[]          buf= new char[1024];
        int             len;
        do
        {
          len= input_stream.Read( buf, 0, buf.Length );
          output_stream.Write( buf, 0, len );
        } while (len == buf.Length);
      }
      catch (IOException)
      {
      }
    }
  }
}
