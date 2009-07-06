/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/stackwriter.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jan 31 21:00:51 2001                          */
/*    Last change :  Wed Feb 28 12:44:30 2001 (serrano)                */
/*    Copyright   :  2001 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    The PrintWriter to display print stack trace                     */
/*=====================================================================*/
using System;
using System.IO;
using System.Text;

namespace bigloo
{
  /*---------------------------------------------------------------------*/
  /*    stackwriter ...                                                  */
  /*---------------------------------------------------------------------*/
  public sealed class stackwriter
  {
    public static String demangle( String  s )
    {
      if (s == null)
        // !!!!! where do we get that from ?
        return "";

      StringBuilder     result= new StringBuilder( s.Length );
      int               start= 0;
      int               len= s.Length;
      int               next= 0;

      try 
      {
        while (start < len)
        {
          next= next_index( s, start );
          if (start < next)
          {
            byte[]      ds = demangle( char_to_byte( s, start, next ) );

            result.Append( foreign.newstring( ds ) );
          }
          if (next < len)
            result.Append( (char)s[next] );
          start= next+1;
        }

        return result.ToString();
      }
      catch (Exception e)
      {
        Console.Error.WriteLine( e.Message );
        Console.Error.WriteLine( e.StackTrace );
        return s;
      }
    }

    private static int next_index( String  s,
                                   int     start )
    {
      int          len= s.Length;

      while (start < len)
      {
        if (!issymbol( (byte)s[start] ))
          return start;
        ++start;
      }
      return len;
    }

    private static bool issymbol( byte  cn )
    {
      return (   (('a' <= cn) && (cn <= 'z'))
              || (('A' <= cn) && (cn <= 'Z'))
              || (('0' <= cn) && (cn <= '9'))
              || (cn == '_'));
    }

    private static byte[] char_to_byte( String  src,
                                        int     start,
                                        int     len )
    {
      int          nlen= len-start;
      byte[]       res= new byte[nlen];
	
      for ( int i= 0 ; i < nlen ; ++i )
        res[i]= (byte)src[start + i];
	
      return res;
    }

    private static byte[] demangle( byte[]  id )
    {
      if (foreign.__cb__.mangledp( id ))
        return (byte[])foreign.__cb__.demangle( id );
      return id;
    }
  }
}
