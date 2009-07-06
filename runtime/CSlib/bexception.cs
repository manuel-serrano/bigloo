using System;

namespace bigloo
{
  public sealed class bexception: ApplicationException
  {
    public readonly Object tag;
    public readonly Object value;

    public bexception( Object  tag,
      Object  value )
    {
      this.tag= tag;
      this.value= value;
    }
  }
}
