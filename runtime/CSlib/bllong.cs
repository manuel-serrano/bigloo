using System;

namespace bigloo
{
  public sealed class bllong: numeral
  {
    public readonly long value;

    public bllong( long  value )
    {
      this.value= value;
    }

    public static bllong make_llong( String  s )
    {
      return new bllong( Int64.Parse( s ) );
    }

    public override void write( output_port  p )
    {
      p.write( "#l" + value );
    }
  }
}
