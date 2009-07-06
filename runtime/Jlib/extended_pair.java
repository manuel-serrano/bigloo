package bigloo;

public class extended_pair extends pair
{
  public Object cer;

  public extended_pair( final Object  car,
                        final Object  cdr,
                        final Object  cer )
  {
    super( car, cdr );
    this.cer= cer;
  }
}
