using System;
using System.Text;

namespace bigloo
{
  public sealed class buffer
  {
    public readonly byte[] buf;
    public int pos;

    public buffer( int  size )
    {
      buf= new byte[size + 3];
      pos= 0;
    }

    public override String ToString()
    {
      return foreign.newstring( buf, 0, pos );
    }

    public void add( String  s )
    {
      add( foreign.getbytes( s ) );
    }

    public void add( byte[]  s )
    {
      if (pos+s.Length > buf.Length-3)
      {
        blit( foreign.getbytes( "..." ) );
        throw new ApplicationException( "end of buffer" );
      }
      blit( s );
    }

    public void add( byte  b )
    {
      if (pos+1 > buf.Length-3)
      {
        blit( foreign.getbytes( "..." ) );
        throw new ApplicationException( "end of buffer" );
      }
      buf[pos++]= b;
    }

    private void blit( byte[]  s )
    {
      for ( int i= 0 ; i < s.Length ; ++i )
        buf[pos++]= s[i];
    }
  }
}
