/*=====================================================================*/
/*    serrano/prgm/project/bigloo/5.0.x/runtime/Jlib/date.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Mon Jun  1 10:20:45 2026                          */
/*    Last change :  Tue Jun  2 05:20:59 2026 (serrano)                */
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
   public Calendar calendar;
   public int timezone;
   public long nsec;

   public date(final long ns,
		final int s,
		final int min,
		final int h,
		final int d,
		final int mon,
		final int y,
		final long tz,
		final boolean istz,
		final int dst) {
      nsec = ns;
      if (!istz) {
	 Calendar c = new GregorianCalendar(y, mon, d, h, min, s);
	 final TimeZone tmz = c.getTimeZone();
	 timezone = tmz.getOffset(c.getTimeInMillis()) / 1000;
         
	 final TimeZone tmzutc = new SimpleTimeZone(0, "UTC");
	 calendar = new GregorianCalendar(tmzutc);
	 calendar.set(y, mon, d, h, min, s);
         
      } else {
	 final TimeZone tmz = new SimpleTimeZone(0, "UTC");
	 calendar = new GregorianCalendar(tmz);
	 calendar.set(y, mon, d, h, min, s);
	 timezone = (int)tz;
      }
   }

   public date(final long seconds) {
      calendar = new GregorianCalendar();
      final Date d = new Date();
      final long milliseconds = seconds * 1000;	 
      d.setTime(milliseconds);
      calendar.setTime(d);
      final TimeZone tmz = calendar.getTimeZone();   
      timezone = tmz.getOffset(milliseconds) / 1000;
/*       final TimeZone tmz = new SimpleTimeZone(0, "UTC");            */
/*       calendar = new GregorianCalendar(tmz);                        */
/*       final Date d = new Date();                                    */
/*       final long milliseconds = seconds * 1000;                     */
/*       d.setTime(milliseconds);                                      */
/*       calendar.setTime(d);                                          */
/*       System.out.println("ICI s=" + seconds + " " + calendar.get(Calendar.DAY_OF_MONTH)); */
/*       timezone = 0;                                                 */
   }
   
   public date(final long n, boolean b) {
      final TimeZone tmz = new SimpleTimeZone(0, "UTC");
      calendar = new GregorianCalendar(tmz);
      final Date d = new Date();
      final long milliseconds = b ? n : n / 1000000;
      d.setTime(milliseconds);
      calendar.setTime(d);
      timezone = 0;
      nsec = b ? n : (n % 1000000);
   }
   
   public date(final long n, boolean b, boolean x) {
      calendar = new GregorianCalendar();
      final Date d = new Date();
      final long milliseconds = b ? n : n / 1000000;
      
      d.setTime(milliseconds);
      calendar.setTime(d);
      final TimeZone tmz = calendar.getTimeZone();
      final TimeZone tmz2 = new SimpleTimeZone(0, "UTC");
      Calendar c2 = new GregorianCalendar(tmz2);
      c2.setTime(d);
      timezone = tmz.getOffset(milliseconds) / 1000;
      nsec = b ? n : (n % 1000000);
   }

   public int getYday() {
      int y = calendar.get(Calendar.YEAR);
      int m = calendar.get(Calendar.MONTH);
      int d = calendar.get(Calendar.DAY_OF_MONTH);

      final TimeZone tmz = new SimpleTimeZone(0, "UTC");
      Calendar c = new GregorianCalendar(tmz);
      calendar.set(y, m, d, 3, 1, 1);
      calendar.add(Calendar.MILLISECOND, timezone * 1000);
      return c.get(Calendar.YEAR);
   }
}
