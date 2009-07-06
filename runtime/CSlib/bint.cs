using System;

namespace bigloo 
{
  public sealed class bint: numeral 
  {
    public readonly int value;

    public static readonly bint BZERO= new bint( 0 );

    public bint( int  value ) 
    {
      this.value= value;
    }

    public override void write( output_port  p ) 
    {
      p.write( value.ToString() );
    }
  }
}
