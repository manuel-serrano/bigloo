package bigloo;

public class buffer
{
  public final byte[] buf;
  public int pos;

  public buffer( final int  size )
  {
    buf= new byte[size + 3];
    pos= 0;
  }

  public String toString()
  {
    return new String( buf, 0, pos );
  }

  public void add( final String  s )
  {
    add( s.getBytes() );
  }

  public void add( final byte[]  s )
  {
    if (pos+s.length > buf.length-3)
    {
      blit( "...".getBytes() );
      throw new RuntimeException( "end of buffer" );
    }
    blit( s );
  }

  public void add( final byte  b )
  {
    if (pos+1 > buf.length-3)
    {
      blit( "...".getBytes() );
      throw new RuntimeException( "end of buffer" );
    }
    buf[pos++]= b;
  }

  private void blit( final byte[]  s )
  {
    for ( int i= 0 ; i < s.length ; ++i )
      buf[pos++]= s[i];
  }
}
