namespace bigloo
{
  public sealed class opaque: obj
  {
    private static opaque _nil = new opaque();

    public static opaque nil() {
      return _nil;
    }
   
    public bool equal( opaque  o )
    {
      return (o == this);
    }

    public int hash()
    {
      return GetHashCode();
    }
  }
}
