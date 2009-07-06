using System;
using System.Globalization;

namespace bigloo
{
  public sealed class date: obj 
  {
    public static TimeSpan TS_EPOCH= new TimeSpan( 1970, 1, 1 );     // !!!!! should be readonly but M$ peverify then rejects ldsflda in methods
    public static DateTime DT_EPOCH= new DateTime( 1970, 1, 1 );     // !!!!! should be readonly but M$ peverify then rejects ldsflda in methods
    public static readonly long EPOCH_in_seconds= (long)(DT_EPOCH.Ticks / 1e7);
    public static readonly GregorianCalendar GREGORIAN_CALENDAR= new GregorianCalendar();
    public DateTime date_time;
    public int timezone;

    public date( int   s,
                 int   min,
                 int   h,
                 int   d,
                 int   mon,
                 int   y,
                 long  tz,
                 bool  istz,
	         int   dst )
    {
      // !!!!! Time zones not yet fully available in .NET !!!!!
      date_time= new DateTime( y, mon, d, h, min, s, GREGORIAN_CALENDAR );
      timezone= 0;
    }

    public date( long  seconds )
    {
      date_time= DT_EPOCH.AddSeconds( seconds );
    }
  }
}
