using System;

namespace bigloo
{
  public class pair: obj
  {
    public Object car;
    public Object cdr;
    
    public pair( Object  car,
      Object  cdr )
    {
      this.car = car;
      this.cdr = cdr;
    }

    public static pair cons( Object  car,
      Object  cdr )
    {
      return new pair( car, cdr );
    }

    public override void write( output_port  p )
    {
      p.write( "(" );
      foreign.write_object( car, p );

      Object rest= cdr;

      while (rest is pair)
      {
       pair c= (pair)rest;

        p.write( " " );
        foreign.write_object( c.car, p );
        rest= c.cdr;
      }
      if (rest != nil._nil)
      {
        p.write( " . " );
        foreign.write_object( rest, p );
      }
      p.write( ")" );
    }
  }
}
