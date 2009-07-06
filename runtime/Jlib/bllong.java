package bigloo;

public class bllong extends numeral
{
  public final long value;

  public bllong( final long  value )
  {
    this.value= value;
  }

  public static bllong make_llong( final String  s )
  {
    return new bllong( Long.parseLong( s ) );
  }

  public void write( final output_port  p )
  {
    p.write( "#l" + value );
  }
}
