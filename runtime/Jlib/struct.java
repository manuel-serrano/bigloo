package bigloo;

public class struct extends obj
{
  public Object key;
  public final Object[] values;

  public struct( final Object  key,
                 final int     size )
  {
    this.key= key;
    values= new Object[size];
  }

  public struct( final Object  key,
                 final int     size,
                 final Object  init )
  {
    this.key= key;
    values= new Object[size];
    for ( int i= 0 ; i < size ; ++i )
      values[i]= init;
  }

  public void write( final output_port  p )
  {
    p.write( "#<" );
    foreign.write_object( key, p );
    for ( int i= 0 ; i < values.length ; ++i )
    {
      p.write( " " );
      foreign.write_object( values[i], p );
    }
    p.write( ">" );
  }
}