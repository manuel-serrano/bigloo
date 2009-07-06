package bigloo;

public class cell extends obj
{
  public Object car;

  public cell( final Object  car )
  {
    this.car= car;
  }

  public void write( final output_port  p )
  {
    foreign.write_object( car, p );
  }
}