package bigloo;

public class object extends obj
{
  public int header;
  public Object widening;

  public void write( final output_port  p )
  {
    p.write( "#<" );
    p.write( header );
    p.write( ">" );
  }
}
