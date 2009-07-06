using System;

namespace bigloo
{
  public sealed class real: numeral
  {
    public readonly double value;

    public real( double  value )
    {
      this.value= value;
    }

    public override void write( output_port  p )
    {
      p.write( value.ToString() );
    }
  }
}
