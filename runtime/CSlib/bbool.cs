namespace bigloo
{
  public sealed class bbool: cnst
  {
    public static readonly bbool faux= new bbool( 1 );
    public static readonly bbool vrai= new bbool( 2 );

    public bbool( int  n )
      : base( n )
    {
    }

    public override void write( output_port  p )
    {
      p.write( (this == vrai) ? "#t" : "#f" );
    }
  }
}
