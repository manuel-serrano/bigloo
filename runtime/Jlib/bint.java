package bigloo;

public class bint extends numeral
{
  public final int value;
  public static final bint BZERO= new bint( 0 );

  public bint( final int  value )
  {
    this.value = value;
  }

  public void write( final output_port  p )
  {
    p.write( Integer.toString( value ) );
  }
}