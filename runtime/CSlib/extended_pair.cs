using System;

namespace bigloo
{
  public sealed class extended_pair: pair
  {
    public Object cer;

    public extended_pair( Object a, Object d, Object e ) : base( a, d )
    {
      this.cer = e;
    }
  }
} 
