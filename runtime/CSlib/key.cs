namespace bigloo 
{
  public sealed class key: cnst 
  {
    public static readonly key _key= new key( 0x106 );

    private key( int  n )
      : base( n ) 
    {
    }

    public override void write( output_port  p ) 
    {
      p.write( "#!key" );
    }
  }
}
