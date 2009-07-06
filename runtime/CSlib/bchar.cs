namespace bigloo
{
  public sealed class bchar: obj
  {
    public readonly byte value;

    public static readonly bchar[] allocated;

    static bchar()
    {
      allocated= new bchar[256];
      for( int i= 0 ; i < 256 ; ++i )
        allocated[i]= new bchar( (byte)i );
    }

    public bchar( byte  value )
    {
      this.value= value;
    }

    public override void write( output_port  p )
    {
      p.write( value );
    }
  }
}
