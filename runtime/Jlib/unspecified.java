package bigloo;

public final class unspecified extends cnst
{
  public static final unspecified unspecified = new unspecified( 3 );

  private unspecified( final int  n )
  {
    super( n );
  }

  public void write( final output_port  p )
  {
    p.write( "#unspecified" );
  }
}
