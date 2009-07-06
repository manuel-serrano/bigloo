package bigloo;

public class bucs2 extends obj
{
  public final char value;

  public bucs2( final char  value )
  {
    this.value= value;
  }

  public void write( final output_port  p )
  {
    p.write( "#u" + value );
  }
}
