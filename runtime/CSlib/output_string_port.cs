using System;
using System.IO;

namespace bigloo
{
  public sealed class output_string_port: output_stream_port 
  {
    public output_string_port() : base( new MemoryStream(), foreign.getbytes( "string" ) )
    {
    }

    public override Object close() 
    {
      try
      {
        byte[] result= ((((MemoryStream)stream).Length == 0)     // !!!!! PNet bug !!!!
			? new byte[0]
			: ((MemoryStream)stream).ToArray());

        stream.Close();
        stream = null;
	base.close();
        return result;
      }
      catch (Exception  e)
      {
        if (stream == null)
          return bigloo.foreign.BUNSPEC;
        else
          foreign.fail( "close", e.Message, this );

        return this;
      }
    }

    public override Object flush() 
    {
      try
      {
        byte[] result= ((MemoryStream)stream).ToArray();

        stream.Flush();
        return result;
      }
      catch (Exception e)
      {
        if (stream != null)
          foreign.fail( "flush", e.Message, this );
        return bbool.faux;
      }
    }

    public Object reset() 
    {
      try
      {
        byte[] result= ((MemoryStream)stream).ToArray();

	stream = new MemoryStream();
        return result;
      }
      catch (Exception e)
      {
        if (stream != null)
          foreign.fail( "flush", e.Message, this );
        return bbool.faux;
      }
    }

    public byte[] get_string()
    {
      return ((MemoryStream)stream).ToArray();
    }
  }
}
