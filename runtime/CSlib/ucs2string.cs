using System;

namespace bigloo
{
  public sealed class ucs2string: obj
  {
    public readonly String value;

    public ucs2string( String  value )
    {
      this.value= value;
    }

    public override String ToString()
    {
      return value;
    }
  }
}
