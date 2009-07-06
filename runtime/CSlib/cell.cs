using System;

namespace bigloo
{
  public sealed class cell: obj
  {
    public Object car;

    public cell( Object  car )
    {
      this.car= car;
    }

    public override void write( output_port  p )
    {
      foreign.write_object( car, p );
    }
  }
}
