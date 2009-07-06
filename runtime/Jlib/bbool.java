package bigloo;

public class bbool extends cnst
{
  public static final bbool faux= new bbool( 1 );
  public static final bbool vrai= new bbool( 2 );

  public bbool( final int  n )
  {
    super( n );
  }

  public void write( final output_port  p )
  {
    p.write( (this == vrai) ? "#t" : "#f" );
  }
}
