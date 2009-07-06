using System;
using System.Collections;

namespace bigloo
{
  public class cnst: obj
  {
    public readonly int value;

    private static readonly Hashtable table= new Hashtable();

    public cnst( int  value )
    {
      this.value= value;
    }

    public static cnst make_cnst( int  n )
    {
      Object       r= table[n];

      if (r != null)
        return (cnst)r;

      cnst         result= new cnst( n );

      table[n]= result;

      return result;
    }
  }
}
