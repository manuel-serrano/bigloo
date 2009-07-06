namespace bigloo 
{
  public sealed class optional: cnst 
  {
    public static readonly optional _optional= new optional( 0x102 );

    private optional( int  n )
      : base( n )
    {
    }

    public override void write( output_port  p ) 
    {
      p.write( "#!optional" );
    }
  }
}
