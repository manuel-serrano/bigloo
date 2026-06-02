/*=====================================================================*/
/*    serrano/bigloo/5.0.x/runtime/Jlib/date.java                      */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Mon Jun  1 10:20:45 2026                          */
/*    Last change :  Tue Jun  2 07:42:18 2026 (serrano)                */
/*    Copyright   :  2026 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Java Dates implementation                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Package                                                          */
/*---------------------------------------------------------------------*/
package bigloo;

import java.util.*;

/*---------------------------------------------------------------------*/
/*    date                                                             */
/*---------------------------------------------------------------------*/
public class date extends obj {
   public Calendar calendar; // an UTC calendar
   public int timezone;      // the timezone offset
   public long nsec;

   static TimeZone tmzUTC = new SimpleTimeZone(0, "UTC");
   
   public date(final long ns,
	       final int s,
	       final int min,
	       final int h,
	       final int d,
	       final int mon,
	       final int y,
	       final long tz,
	       boolean istz) {
      nsec = ns;
      
      if (!istz) {
	 // build a temporary locale time to get its epoch relative value
	 Calendar c = new GregorianCalendar();
	 c.set(y, mon, d, h, min, s);
	 final Date dt = c.getTime();
	 final TimeZone tmz = c.getTimeZone();   

	 // build the real epoch-base time
	 calendar = new GregorianCalendar(tmzUTC);
	 calendar.setTime(dt);
	 timezone = tmz.getOffset(c.getTimeInMillis()) / 1000;
      } else {
	 // build an utc+timezone date
	 calendar = new GregorianCalendar(tmzUTC);
	 calendar.set(y, mon, d, h, min, s);
	 calendar.add(Calendar.MILLISECOND, (int)tz * -1000);
	 timezone = (int)tz;
      }
   }

   public date(final long seconds) {
      // build the utc calendar
      calendar = new GregorianCalendar(tmzUTC);
      calendar.setTimeInMillis(seconds * 1000);

      // get the current timezone
      Calendar c = new GregorianCalendar();
      final TimeZone tmz = c.getTimeZone();   
      timezone = tmz.getOffset(c.getTimeInMillis()) / 1000;
   }
   
   public date(final long n, final long q) {
      // build the utc calendar
      calendar = new GregorianCalendar(tmzUTC);
      calendar.setTimeInMillis(n / q);
      nsec = (n % q);

      // get the current timezone
      Calendar c = new GregorianCalendar();
      final TimeZone tmz = c.getTimeZone();   
      timezone = tmz.getOffset(c.getTimeInMillis()) / 1000;
   }

   public Calendar tzCalendar() {
      if (timezone == 0) {
	 return calendar;
      } else {
	 TimeZone tz = TimeZone.getTimeZone("UTC");
	 tz.setRawOffset(timezone * 1000);
	 Calendar c = new GregorianCalendar(tz);
	 c.setTimeInMillis(calendar.getTimeInMillis());
	 return c;
      }
   }
}
