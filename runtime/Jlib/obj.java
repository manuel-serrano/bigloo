package bigloo;

public class obj
{
  /*
  public void dump( final buffer  b )
  {
     b.add("?");
  }

  public static void dump( final buffer  b,
                           final Object  o )
  {
    if (o instanceof OBJ)
      ((OBJ)o).dump( b );
    else if (o instanceof byte[])
    {
      b.add( "\"" );
      b.add( (byte[])o );
      b.add( "\"" );
    }
    else if (o instanceof Object[])
    {
      final Object[]    v= (Object[])o;

      b.add( "#(" );
      for ( int i= 0 ; i < v.length ; ++i )
      {
        if (i != 0)
          b.add( " " );
        dump( b, v[i] );
      }
      b.add( ")" );
    }
    else
      b.add( "Unknown object" );
  }

  public String toString()
  {
    final buffer        buf= new buffer( 80 );

    try
    {
      dump( buf );
    }
    catch (Exception _)
    {
    }

    return buf.toString();
  }
  */

  public void write( final output_port  p )
  {
    p.write( p.toString() );
  }
}
