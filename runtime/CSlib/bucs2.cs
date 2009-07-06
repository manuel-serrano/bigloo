namespace bigloo 
{
  public sealed class bucs2: obj 
  {
    public readonly char value;

    public bucs2( char  value ) 
    {
      this.value= value;
    }

    public override void write( output_port  p ) 
    {
      p.write( "#u" + value );
    }
  }
}
