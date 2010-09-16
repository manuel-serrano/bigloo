package bigloo;

import java.util.*;

public class cnst extends obj
{
  public final int value;

  private static final Hashtable table = new Hashtable();

  public cnst( final int  value )
  {
    this.value = value;
  }

  public static cnst make_cnst( final int value )
  {
    final Integer key = new Integer( value );
    final Object r = table.get( key );

    if (r != null)
      return (cnst)r;

    final cnst result = new cnst( value );

    table.put( key, result );
    return result;
  }
}
