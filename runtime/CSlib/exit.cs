using System;

namespace bigloo 
{
  public sealed class exit 
  {
    public static int count= 0;
    public int userp;
    public int stamp= count++;
    public exit prev;

    public exit() 
    {
    }

    public void write( output_port  p ) 
    {
      p.write( "<exit" );

      for( exit i= this ; i != null ; i= i.prev ) 
      {
        p.write( " " );
        p.write( i.stamp );
      }

      p.write(">");
    }
  }
}
