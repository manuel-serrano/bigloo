package bigloo;

public final class optional extends cnst
{
  public static final optional optional= new optional( 0x102 );

  private optional( final int  n )
  {
    super( n );
  }

  public void write( final output_port  p )
  {
    p.write( "#!optional" );
  }
}