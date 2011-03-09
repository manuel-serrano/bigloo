package bigloo;

public class opaque extends obj
{
  private static opaque nil = new opaque();
   
  public byte[] identifier;
  
  public static opaque nil() {
     return nil;
  }
   
  public boolean equal( final opaque  o )
  {
    return (o == this);
  }

  public int hash()
  {
    return hashCode();
  }
}
