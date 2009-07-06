package bigloo;

public class belong extends numeral
{
  public final long value;

  public belong( final long  value )
  {
    this.value= value;
  }

  public static belong make_elong( final String  s )
  {
    return new belong( Long.parseLong( s ) );
  }

  public void write( final output_port  p )
  {
    p.write( "#e" + value );
  }
}
