using System;

namespace bigloo
{
  public sealed class belong: numeral
  {
    public readonly long value;

    public belong( long  value )
    {
      this.value= value;
    }

    public static belong make_elong( String  s )
    {
      return new belong( Int64.Parse( s ) );
    }

    public override void write( output_port  p )
    {
      p.write( "#e" + value );
    }
  }
}
