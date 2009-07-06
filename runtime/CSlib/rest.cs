namespace bigloo
{
  public sealed class rest: cnst
  {
    public static readonly rest _rest= new rest( 0x103 );

    private rest( int  n )
      : base( n ) 
    {
    }

    public override void write( output_port  p )
    {
      p.write( "#!rest" );
    }
  }
}
