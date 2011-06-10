package bigloo;

public class real extends numeral
{
  public double value;

  public real( final double  value )
  {
    this.value = value;
  }

  public void write( final output_port  p )
  {
    p.write( Double.toString( value ) );
  }
}
