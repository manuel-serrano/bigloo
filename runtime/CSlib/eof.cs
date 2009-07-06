namespace bigloo
{
  public sealed class eof: obj
  {
    public static readonly eof _eof= new eof();

    private eof()
    {
    }

    public override void write( output_port  p )
    {
      p.write( "#eof" );
    }
  }
}
