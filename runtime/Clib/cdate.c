/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cdate.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Feb  4 11:51:17 2003                          */
/*    Last change :  Thu Mar 26 14:26:00 2020 (serrano)                */
/*    Copyright   :  2003-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C implementation of time & date                                  */
/*=====================================================================*/
#include <string.h>
#include <bigloo.h>
#if defined( _MSC_VER) || defined( _MINGW_VER )
// force 32 bits time values as Bigloo requires sizeof(time_t) == sizeof(long)
#  define _USE_32BIT_TIME_T 1
#endif
#include <time.h>
#include <ctype.h>
#include <sys/time.h>

#define MILLIBASE 1000
#define MICROBASE (1000 * MILLIBASE)
#define NANOBASE ((BGL_LONGLONG_T)(1000 * MICROBASE))

/*---------------------------------------------------------------------*/
/*    date mutex                                                       */
/*---------------------------------------------------------------------*/
static obj_t date_mutex = BUNSPEC;
DEFINE_STRING( date_mutex_name, _2, "date-mutex", 10 );

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_date ...                                                */
/*    -------------------------------------------------------------    */
/*    Initialize the mutex needed by the date lib.                     */
/*---------------------------------------------------------------------*/
void
bgl_init_date() {
   if( date_mutex == BUNSPEC ) {
      date_mutex = bgl_make_spinlock( date_mutex_name );
   }
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_get_timezone ...                                             */
/*---------------------------------------------------------------------*/
static long
bgl_get_timezone( time_t s ) {
   struct tm *tm = localtime( &s );
   long m1, h1, d1;

   m1 = tm->tm_min;
   h1 = tm->tm_hour;
   d1 = tm->tm_yday;

   tm = gmtime( &s );

   if( tm->tm_yday == d1 ) {
      return (h1 - tm->tm_hour) * 3600 + (m1 - tm->tm_min);
   } else if( tm->tm_yday > d1 ) {
      return (h1 - (tm->tm_hour + 24)) * 3600 + (m1 - tm->tm_min);
   } else {
      return (h1 - (tm->tm_hour - 24)) * 3600 + (m1 - tm->tm_min);
   }
}
   
/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_timezone ...                                                 */
/*---------------------------------------------------------------------*/
static long
bgl_timezone() {
   static long timezone = 23;

   if( timezone == 23 ) {
      timezone = bgl_get_timezone( time( 0L ) );
   }

   return timezone;
}
       
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    tm_to_date ...                                                   */
/*---------------------------------------------------------------------*/
static obj_t
tm_to_date( struct tm *tm ) {
   obj_t date;

   date = GC_MALLOC_ATOMIC( BGL_DATE_SIZE );
   date->date.header = MAKE_HEADER( DATE_TYPE, 0 );

#if( BGL_HAVE_GMTOFF )
   date->date.timezone = tm->tm_gmtoff;
#else
   date->date.timezone = bgl_timezone();   
#endif

   date->date.nsec = 0;
   date->date.sec = tm->tm_sec;
   date->date.min = tm->tm_min;
   date->date.hour = tm->tm_hour;
      
   date->date.mday = tm->tm_mday;
   date->date.mon = tm->tm_mon + 1;
   date->date.year = tm->tm_year + 1900;
   date->date.wday = tm->tm_wday + 1;
   date->date.yday = tm->tm_yday + 1;

   date->date.isdst = tm->tm_isdst;

   return BREF( date );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_to_date ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_seconds_to_date( long s ) {
   obj_t res;
   time_t sec = (time_t)s;

   BGL_MUTEX_LOCK( date_mutex );
   res = tm_to_date( localtime( &sec ) );
   BGL_MUTEX_UNLOCK( date_mutex );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_to_gmtdate ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_seconds_to_gmtdate( long s ) {
   obj_t res;
   time_t sec = (time_t)s;

   BGL_MUTEX_LOCK( date_mutex );
   res = tm_to_date( gmtime( &sec ) );
   BGL_MUTEX_UNLOCK( date_mutex );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_nanoseconds_to_date ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_nanoseconds_to_date( BGL_LONGLONG_T nsec ) {
   obj_t res;
   time_t sec = nsec / NANOBASE;

   BGL_MUTEX_LOCK( date_mutex );
   res = tm_to_date( localtime( &sec ) );
   BGL_MUTEX_UNLOCK( date_mutex );

   BGL_DATE( res ).nsec = (nsec - ((BGL_LONGLONG_T) sec * NANOBASE));
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_date ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_date( BGL_LONGLONG_T ns, int s, int m, int hr, int mday, int mon, int year, long tz, bool_t istz, int isdst ) {
   struct tm tm;
   time_t t;
   obj_t date;
   BGL_LONGLONG_T nsec = (ns % (BGL_LONGLONG_T)1000000000);
      
   tm.tm_sec = s + (long)(ns / (BGL_LONGLONG_T)1000000000);
   tm.tm_min = m; 
   tm.tm_hour = hr;
   tm.tm_mday = mday;
   tm.tm_mon = mon - 1;
   tm.tm_year = year - 1900;
   tm.tm_isdst = isdst;

   t = mktime( &tm );

   date = bgl_seconds_to_date( t );
   BGL_DATE( date ).nsec = nsec;

   if( istz ) {
      BGL_DATE( date ).timezone = tz;
   }
   return date;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_date_to_seconds ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_date_to_seconds( obj_t date ) {
   struct tm t, *tl;
   time_t n, m;
   long tz;

   t.tm_sec = BGL_DATE( date ).sec;
   t.tm_min = BGL_DATE( date ).min;
   t.tm_hour = BGL_DATE( date ).hour;
   t.tm_mday = BGL_DATE( date ).mday;
   t.tm_mon = BGL_DATE( date ).mon - 1;
   t.tm_year = BGL_DATE( date ).year - 1900;
   t.tm_isdst = BGL_DATE( date ).isdst;

   n = mktime( &t );

   BGL_MUTEX_LOCK( date_mutex );
#if( BGL_HAVE_GMTOFF )
   /* get the timezone at that date */
   tl = localtime( &n );
   tz = tl->tm_gmtoff;
#else
   /* mid-month to avoid day overflow */
   t.tm_mday = 15;
   m = mktime( &t );
   tz = bgl_get_timezone( n );
#endif

   BGL_MUTEX_UNLOCK( date_mutex );
   return (long)n + (tz - (BGL_DATE( date ).timezone));
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF BGL_LONGLONG_T                                   */
/*    bgl_date_to_nanoseconds ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BGL_LONGLONG_T
bgl_date_to_nanoseconds( obj_t date ) {
   return (BGL_LONGLONG_T)bgl_date_to_seconds( date ) * NANOBASE +
      BGL_DATE( date ).nsec;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_current_seconds ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_current_seconds() {
   return (long)( time( 0L ) );
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    bgl_current_microseconds ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BGL_LONGLONG_T
bgl_current_microseconds() {
#if( BGL_HAVE_TIMEVAL )   
   struct timeval tv;
   if( gettimeofday( &tv, 0 ) == 0 ) {
      return (BGL_LONGLONG_T)(tv.tv_sec) * MICROBASE +
	 (BGL_LONGLONG_T)(tv.tv_usec);
   } else {
      C_SYSTEM_FAILURE( BGL_ERROR,
			"current-microseconds",
			strerror( errno ),
			BUNSPEC );
   }
#else
   return (BGL_LONGLONG_T)(time( 0L ) ) * MICROBASE;
#endif
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    bgl_current_nanoseconds ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BGL_LONGLONG_T
bgl_current_nanoseconds() {
#if( BGL_HAVE_TIMEVAL )   
   struct timeval tv;

   if( gettimeofday( &tv, 0 ) == 0 ) {
      return (BGL_LONGLONG_T)(tv.tv_sec) * NANOBASE +
	 (BGL_LONGLONG_T)(tv.tv_usec) * 1000;
   } else {
      C_SYSTEM_FAILURE( BGL_ERROR,
			"current-nanoseconds",
			strerror( errno ),
			BUNSPEC );
   }
#else
   return (BGL_LONGLONG_T)(time( 0L ) ) * NANOBASE;
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_to_utc_string ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_seconds_to_utc_string( long sec ) {
   struct tm *t;
   char *s;

   t = gmtime( (time_t *)&sec );
   s = asctime( t );

   return string_to_bstring_len( s, strlen( s ) - 1 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_to_string ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_seconds_to_string( long sec ) {
   char *s;
   obj_t res;
   
   BGL_MUTEX_LOCK( date_mutex );
   s = ctime( (time_t *)&sec );
   res = string_to_bstring_len( s, strlen( s ) - 1 );
   BGL_MUTEX_UNLOCK( date_mutex );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_format ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bgl_seconds_format( long s, obj_t fmt ) {
   char *buffer;
   struct tm *p;
   int len = (int)STRING_LENGTH( fmt ) + 256;
   time_t sec = (time_t)s;

   buffer = (char *)GC_MALLOC_ATOMIC( len + 1 );
   
   BGL_MUTEX_LOCK( date_mutex );
   p = localtime( &sec );
   BGL_MUTEX_UNLOCK( date_mutex );
   
   len = (int)strftime( buffer, len, BSTRING_TO_STRING( fmt ), p );

   if( len > 0 )
      return string_to_bstring_len( buffer, len );
   else {
      C_FAILURE( "seconds-format", "buffer too short!", BINT( 256 ) );

      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    make_names ...                                                   */
/*---------------------------------------------------------------------*/
static obj_t
make_names( int range, char *fmt ) {
   obj_t names = (obj_t)create_vector( range );
   char buf[ 40 ];
   struct tm tm;
   int i;

   for( i = 0; i < range; i++ ) {
      tm.tm_wday = i;
      tm.tm_mon = i;
      strftime( buf, 40, fmt, &tm );
      VECTOR_SET( names, i, string_to_bstring( buf ) );
   }

   return names;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_day_name ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_day_name( int day ) {
   static obj_t names = BNIL;
   
   if( names == BNIL ) names = make_names( 7, "%A" );

   return VECTOR_REF( names, day-1 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_day_aname ...                                                */
/*---------------------------------------------------------------------*/
obj_t
bgl_day_aname( int day ) {
   static obj_t names = BNIL;
   
   if( names == BNIL ) names = make_names( 7, "%a" );

   return VECTOR_REF( names, day-1 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_month_name ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_month_name( int month ) {
   static obj_t names = BNIL;
   
   if( names == BNIL ) names = make_names( 12, "%B" );

   return VECTOR_REF( names, month-1 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_month_aname ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_month_aname( int month ) {
   static obj_t names = BNIL;
   
   if( names == BNIL ) names = make_names( 12, "%b" );

   return VECTOR_REF( names, month-1 );
}
