package bigloo;

public final class key extends cnst
{
  public static final key key= new key( 0x106 );

  private key( final int  n )
  {
    super( n );
  }

  public void write( final output_port  p )
  {
    p.write( "#!key" );
  }
}
