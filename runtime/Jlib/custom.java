package bigloo;

public class custom extends obj
{
  private static custom nil = new custom();
   
  public byte[] identifier;
  
  public static custom nil() {
     return nil;
  }
   
  public boolean equal( final custom  o )
  {
    return (o == this);
  }

  public int hash()
  {
    return hashCode();
  }
}
