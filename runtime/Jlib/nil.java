package bigloo;

public final class nil extends cnst
{
  public static final nil nil= new nil( 0 );

  private nil( final int  n )
  {
    super( n );
  }

  public void write( final output_port  p )
  {
    p.write( "()" );
  }
}