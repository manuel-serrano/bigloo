package bigloo;

public final class eof extends obj
{
  public static final eof eof= new eof();

  private eof()
  {
  }

  public void write( final output_port  p )
  {
    p.write( "#eof" );
  }
}
