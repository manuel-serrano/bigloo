package bigloo;

public class bexception extends RuntimeException {
  public final Object tag;
  public final Object value;

  public bexception( final Object tag, final Object value ) {
    this.tag = tag;
    this.value = value;
  }
}
