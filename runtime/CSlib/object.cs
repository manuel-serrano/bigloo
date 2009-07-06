using System;

namespace bigloo
{
  public class bobject: obj
  {
    public int header;
    public Object widening;

    public override void write( output_port  p )
    {
      p.write( "#<" );
      p.write( header );
      p.write( ">" );
    }
  }
}
