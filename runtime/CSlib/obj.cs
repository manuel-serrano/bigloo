using System;
using System.Text;

namespace bigloo 
{
  public class obj 
  {
    public override String ToString()
    {
      output_port strstream = new output_string_port();

      write( strstream );

      Object r = strstream.close();

      if (r is byte[])
        return foreign.newstring( (byte[])r );

      return ("<obj " + GetType().ToString() + ">");
    }

    public virtual void write( output_port  p )
    {
      p.write( p.ToString() );
    }
  }
}
