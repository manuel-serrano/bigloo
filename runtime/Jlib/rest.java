package bigloo;

public final class rest extends cnst
{
  public static final rest rest= new rest( 0x103 );

  private rest( final int  n )
  {
    super( n );
  }

  public void write( final output_port  p )
  {
    p.write( "#!rest" );
  }
}
