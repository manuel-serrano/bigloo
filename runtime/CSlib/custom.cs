namespace bigloo
{
  public sealed class custom: obj
  {
    private static custom _nil = new custom();
    public byte[] identifier;

    public static custom nil() {
      return _nil;
    }
   
    public bool equal( custom  o )
    {
      return (o == this);
    }

    public int hash()
    {
      return GetHashCode();
    }
  }
}
