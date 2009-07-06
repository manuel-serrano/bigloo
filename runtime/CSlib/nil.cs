namespace bigloo
{
  public sealed class nil: cnst
  {
    public static readonly nil _nil= new nil( 0 );

    private nil( int  n )
      : base( n )
    {
    }

    public override void write( output_port  p )
    {
      p.write( "()" );
    }
  }
}
