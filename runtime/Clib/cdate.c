/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cdate.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Feb  4 11:51:17 2003                          */
/*    Last change :  Wed Dec 24 08:22:38 2008 (serrano)                */
/*    Copyright   :  2003-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C implementation of time & date                                  */
/*=====================================================================*/
#include <string.h>
#include <bigloo.h>
#include <time.h>
#include <ctype.h>
#include <sys/time.h>

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
      date_mutex = bgl_make_mutex( date_mutex_name );
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    tm_to_date ...                                                   */
/*---------------------------------------------------------------------*/
static obj_t
tm_to_date( struct tm *tm ) {
   obj_t date;

   date = GC_MALLOC_ATOMIC( BGL_DATE_SIZE );
   date->date_t.header = MAKE_HEADER( DATE_TYPE, 0 );

   date->date_t.timezone = BGL_TIMEZONE;
      
   date->date_t.sec = tm->tm_sec;
   date->date_t.min = tm->tm_min;
   date->date_t.hour = tm->tm_hour;
      
   date->date_t.mday = tm->tm_mday;
   date->date_t.mon = tm->tm_mon + 1;
   date->date_t.year = tm->tm_year + 1900;
   date->date_t.wday = tm->tm_wday + 1;
   date->date_t.yday = tm->tm_yday + 1;

   date->date_t.isdst = tm->tm_isdst;

   return BREF( date );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_to_date ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_seconds_to_date( long sec ) {
   obj_t res;

   bgl_mutex_lock( date_mutex );
   res = tm_to_date( localtime( (time_t *)&sec ) );
   bgl_mutex_unlock( date_mutex );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_date ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_date( int s, int m, int hr, int mday, int mon, int year, long tz, bool_t istz, int isdst ) {
   struct tm tm;
   time_t t;

   tm.tm_sec = s;
   tm.tm_min = m;
   tm.tm_hour = hr;
   tm.tm_mday = mday;
   tm.tm_mon = mon - 1;
   tm.tm_year = year - 1900;
   tm.tm_isdst = isdst;

   t = mktime( &tm );

   if( istz ) t = t - BGL_TIMEZONE + tz;

   return bgl_seconds_to_date( t );
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_date_to_seconds ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_date_to_seconds( obj_t date ) {
   struct tm t;
   time_t n;

   t.tm_sec = BGL_DATE( date ).sec;
   t.tm_min = BGL_DATE( date ).min;
   t.tm_hour = BGL_DATE( date ).hour;
   t.tm_mday = BGL_DATE( date ).mday;
   t.tm_mon = BGL_DATE( date ).mon - 1;
   t.tm_year = BGL_DATE( date ).year - 1900;
   t.tm_isdst = BGL_DATE( date ).isdst;

   n = mktime( &t );
   
   return (long)n;
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
      return (BGL_LONGLONG_T)(tv.tv_sec) * 1000000 +
	 (BGL_LONGLONG_T)(tv.tv_usec);
   } else {
      C_SYSTEM_FAILURE( BGL_ERROR,
			"current-microseconds",
			strerror( errno ),
			BUNSPEC );
   }
#else
   return (BGL_LONGLONG_T)(bgl_current_seconds() ) * 1000000;
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
   
   bgl_mutex_lock( date_mutex );
   s = ctime( (time_t *)&sec );
   res = string_to_bstring_len( s, strlen( s ) - 1 );
   bgl_mutex_unlock( date_mutex );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_seconds_format ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bgl_seconds_format( long sec, obj_t fmt ) {
   char *buffer;
   struct tm *p;
   int len = (int)STRING_LENGTH( fmt ) + 256;

   buffer = (char *)GC_MALLOC_ATOMIC( len + 1 );
   
   bgl_mutex_lock( date_mutex );
   p = localtime( (time_t *)&sec );
   bgl_mutex_unlock( date_mutex );
   
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
