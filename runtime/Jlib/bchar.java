package bigloo;

public class bchar extends obj
{
  public final byte value;

  public static final bchar[] allocated= new bchar[256];

  static
  {
    for ( int i= 0 ; i < 256 ; ++i )
      allocated[i]= new bchar( (byte)i );
  }

  public bchar( final byte  value )
  {
    this.value= value;
  }

  public void write( final output_port  p )
  {
    p.write( value );
  }
}
