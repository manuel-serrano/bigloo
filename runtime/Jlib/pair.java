package bigloo;

public class pair extends obj
{
  public Object car;
  public Object cdr;

  public pair( final Object car, final Object cdr )
  {
    this.car= car;
    this.cdr= cdr;
  }

  public static pair cons( final Object car, final Object cdr ) {
    return new pair( car, cdr );
  }

  public void write( final output_port p ) {
    p.write( "(" );
    foreign.write_object( car, p );

    Object rest = cdr;

    while (rest instanceof pair) {
      final pair c = (pair)rest;

      p.write( " " );
      foreign.write_object( c.car, p );
      rest= c.cdr;
    }
    
    if (rest != nil.nil) {
      p.write( " . " );
      foreign.write_object( rest, p );
    }
    p.write( ")" );
  }
}
