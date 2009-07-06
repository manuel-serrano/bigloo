namespace bigloo 
{
  public sealed class unspecified: cnst
  {
    public static readonly unspecified _unspecified= new unspecified( 3 );

    private unspecified( int  n )
      : base( n )
    {
    }

    public override void write( output_port  p )
    {
      p.write( "#unspecified" );
    }
  }
}
