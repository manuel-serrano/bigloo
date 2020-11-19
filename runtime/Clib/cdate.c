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
#if( BGL_HAVE_LOCALTIME_R )   
   struct tm res;
   struct tm *tm = localtime_r( &s, &res );
#else
   struct tm *tm = localtime( &s );
#endif
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
/*    static void                                                      */
/*    tm_date ...                                                      */
/*---------------------------------------------------------------------*/
static void
tm_date( struct tm *tm, obj_t date ) {
#if( !BGL_HAVE_GMTOFF )
   date->date.timezone = bgl_timezone();
#else
   date->date.tm.tm_gmtoff = tm->tm_gmtoff;
#endif
   
   date->date.tm.tm_sec = tm->tm_sec;
   date->date.tm.tm_min = tm->tm_min;
   date->date.tm.tm_hour = tm->tm_hour;
   date->date.tm.tm_mday = tm->tm_mday;
   date->date.tm.tm_mon = tm->tm_mon;
   date->date.tm.tm_year = tm->tm_year;
   date->date.tm.tm_wday = tm->tm_wday;
   date->date.tm.tm_yday = tm->tm_yday;
   date->date.tm.tm_isdst = tm->tm_isdst;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_to_date ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_seconds_to_date( long s ) {
   time_t sec = (time_t)s;
   obj_t date = GC_MALLOC_ATOMIC( BGL_DATE_SIZE );
   
   date->date.header = MAKE_HEADER( DATE_TYPE, 0 );

#if( BGL_HAVE_LOCALTIME_R )   
   localtime_r( &sec, &(date->date.tm) );
#else   
   BGL_MUTEX_LOCK( date_mutex );
   tm_date( localtime( &sec ), date );
   BGL_MUTEX_UNLOCK( date_mutex );
#endif
   
   date->date.time = sec;
   date->date.nsec = 0;
#if( !BGL_HAVE_GMTOFF )
   date->date.timezone = bgl_timezone();   
#endif
   
   return BREF( date );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_to_gmtdate ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_seconds_to_gmtdate( long s ) {
   time_t sec = (time_t)s;
   obj_t date = GC_MALLOC_ATOMIC( BGL_DATE_SIZE );
   
   date->date.header = MAKE_HEADER( DATE_TYPE, 0 );

#if( BGL_HAVE_GMTIME_R )
   gmtime_r( &sec, &(date->date.tm) );
#else
   BGL_MUTEX_LOCK( date_mutex );
   tm_date( gmtime( &sec ), date );
   BGL_MUTEX_UNLOCK( date_mutex );
#endif
   
   date->date.time = sec;
   date->date.nsec = 0;
#if( !BGL_HAVE_GMTOFF )
   date->date.timezone = bgl_timezone();   
#endif
   
   return BREF( date );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_milliseconds_to_gmtdate ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_milliseconds_to_gmtdate( BGL_LONGLONG_T msec ) {
   time_t sec = msec / MILLIBASE;
   obj_t date = GC_MALLOC_ATOMIC( BGL_DATE_SIZE );
   
   date->date.header = MAKE_HEADER( DATE_TYPE, 0 );

#if( BGL_HAVE_GMTIME_R )
   gmtime_r( &sec, &(date->date.tm) );
#else
   BGL_MUTEX_LOCK( date_mutex );
   tm_date( gmtime( &sec ), date );
   BGL_MUTEX_UNLOCK( date_mutex );
#endif
   
   date->date.time = sec;
   date->date.nsec = (msec - ((BGL_LONGLONG_T) sec * MILLIBASE)) * 1000000;
#if( !BGL_HAVE_GMTOFF )
   date->date.timezone = 0;
#endif
   
   return BREF( date );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_nanoseconds_to_date ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_nanoseconds_to_date( BGL_LONGLONG_T nsec ) {
   time_t sec = nsec / NANOBASE;
   obj_t date = GC_MALLOC_ATOMIC( BGL_DATE_SIZE );
   
   date->date.header = MAKE_HEADER( DATE_TYPE, 0 );

#if( BGL_HAVE_LOCALTIME_R )
   localtime_r( &sec, &(date->date.tm) );
#else   
   BGL_MUTEX_LOCK( date_mutex );
   tm_date( localtime( &sec ), date );
   BGL_MUTEX_UNLOCK( date_mutex );
#endif

   date->date.nsec = (nsec - ((BGL_LONGLONG_T) sec * NANOBASE));
   date->date.time = nsec / NANOBASE;
   
   return BREF( date );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_milliseconds_to_date ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_milliseconds_to_date( BGL_LONGLONG_T msec ) {
   time_t sec = msec / MILLIBASE;
   obj_t date = GC_MALLOC_ATOMIC( BGL_DATE_SIZE );

   date->date.header = MAKE_HEADER( DATE_TYPE, 0 );

#if( BGL_HAVE_LOCALTIME_R )
   localtime_r( &sec, &(date->date.tm) );
#else   
   BGL_MUTEX_LOCK( date_mutex );
   tm_date( localtime( &sec ), date );
   BGL_MUTEX_UNLOCK( date_mutex );
#endif

   date->date.nsec = (msec - ((BGL_LONGLONG_T) sec * MILLIBASE)) * 1000000;
   date->date.time = msec / MILLIBASE;

   return BREF( date );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_update_date ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_update_date( obj_t obj, BGL_LONGLONG_T ns, int s, int m, int hr, int mday, int mon, int year, long tz, bool_t istz, int isdst ) {
   obj_t date = CREF( obj );
#if( !BGL_HAVE_GMTIME_R )   
   struct tm *tm;
#endif
      
   date->date.tm.tm_sec = s + (long)(ns / (BGL_LONGLONG_T)1000000000);
   date->date.tm.tm_min = m; 
   date->date.tm.tm_hour = hr;
   date->date.tm.tm_mday = mday;
   date->date.tm.tm_mon = mon - 1;
   date->date.tm.tm_year = year - 1900;
   date->date.tm.tm_isdst = isdst;
   date->date.nsec = (ns % (BGL_LONGLONG_T)1000000000);

   if( istz ) {
      /* build a UTC time when a timezone is specified */
#if( BGL_HAVE_TIMEGM )
      date->date.time = timegm( &(date->date.tm) );
#else
      static char *tze = 0;

      if( !tze ) tze = getenv( "TZ" );
      setenv( "TZ", "UTC", 1 );
      date->date.time = mktime( &(date->date.tm) );
      if( tze) {
	 setenv( "TZ", tze, 1 ) ;
      } else {
	 unsetenv( "TZ" );
      }
#endif      
      date->date.time -= tz;
#if( BGL_HAVE_GMTOFF )
      date->date.tm.tm_gmtoff = tz;
#else      
      date->date.timezone = tz;
#endif      
   } else {
      date->date.time = mktime( &(date->date.tm) );
#if( !BGL_HAVE_GMTOFF )
      date->date.timezone = bgl_get_timezone( date->date.time );
#endif
   }

   return obj;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_date ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_date( BGL_LONGLONG_T ns, int s, int m, int hr, int mday, int mon, int year, long tz, bool_t istz, int isdst ) {
   obj_t date;
      
   date = GC_MALLOC_ATOMIC( BGL_DATE_SIZE );
   date->date.header = MAKE_HEADER( DATE_TYPE, istz );

/*    fprintf( stderr, "make_date ns=%lld s=%d m=%d hr=%d mday=%d mon=%d year=%d tz=%d istz=%d isdst=%d\n", */
/* 	    ns,  s,  m,  hr,  mday,  mon,  year, tz, istz, isdst )   ; */
   return bgl_update_date( BREF( date ), ns, s, m, hr, mday, mon, year, tz, istz, isdst );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_date_to_gmtdate ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_date_to_gmtdate( obj_t date ) {
   if( BGL_DATE_ISGMT( date ) ) {
      return date;
   } else {
      time_t sec = BGL_DATE_TIME( date );

#if( BGL_HAVE_GMTIME_R )
      gmtime_r( &sec, &(BGL_DATE( date ).tm) );
#else
      BGL_MUTEX_LOCK( date_mutex );
      tm_date( gmtime( &sec ), CREF( date ) );
      BGL_MUTEX_UNLOCK( date_mutex );
#endif
   
#if( BGL_HAVE_GMTOFF )
      BGL_DATE( date ).tm.tm_gmtoff = 0;
#else   
      BGL_DATE( date ).timezone = 0;
#endif
   
      BGL_DATE( date ).time = sec;

      CREF( date )->header = MAKE_HEADER( DATE_TYPE, 1 );
      return date;
   }
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_date_to_seconds ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_date_to_seconds( obj_t date ) {
   return BGL_DATE( date ).time;
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
/*    BGL_RUNTIME_DEF BGL_LONGLONG_T                                   */
/*    bgl_date_to_milliseconds ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BGL_LONGLONG_T
bgl_date_to_milliseconds( obj_t date ) {
   return (BGL_LONGLONG_T)bgl_date_to_seconds( date ) * MILLIBASE +
      BGL_DATE( date ).nsec / 1000000;
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
/*    bgl_current_milliseconds ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BGL_LONGLONG_T
bgl_current_milliseconds() {
#if( BGL_HAVE_TIMEVAL )   
   struct timeval tv;
   if( gettimeofday( &tv, 0 ) == 0 ) {
      return (BGL_LONGLONG_T)(tv.tv_sec) * MILLIBASE +
	 (BGL_LONGLONG_T)(tv.tv_usec / 1000);
   } else {
      C_SYSTEM_FAILURE( BGL_ERROR,
			"current-milliseconds",
			strerror( errno ),
			BUNSPEC );
   }
#else
   return (BGL_LONGLONG_T)(time( 0L ) ) * MILLIBASE;
#endif
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
#if( BGL_HAVE_LOCALTIME_R )   
   struct tm res;
#endif

   buffer = (char *)GC_MALLOC_ATOMIC( len + 1 );
   
#if( BGL_HAVE_LOCALTIME_R )   
   p = localtime_r( &sec, &res );
#else
   BGL_MUTEX_LOCK( date_mutex );
   p = localtime( &sec );
   BGL_MUTEX_UNLOCK( date_mutex );
#endif
   
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
