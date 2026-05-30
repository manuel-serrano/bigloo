<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/date.md                  -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Dates                                                         -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/date.scm")
,(example-path "../test/src/date.bgl")

Dates
=====

Predicates
----------

### date? ###
Returns `#t` if and only if `obj` is a _date_ as returned
by `make-date`, `current-date`, `seconds->date`, or
`seconds->gmtdate`. It returns `#f` otherwise.

### make-date ###
Creates a `date` object from the integer values passed as argument.
The argument `timezone`, if provided, is expressed in minute.

The argument `dst` is either `-1` when the information is not
available, `0` when daylight saving is disabled, `1` when daylight
saving is enabled.

### date-copy ###
Copies a `date` with optional modifications.

### date-update! ###
Updates an existing date.

### date->gmtdate! ###
<!-- [:@C] -->
Updates the date to switch to an UTC representation. Returns the modified
date object.

### current-date ###
Returns a `date` object representing the current date.

### current-seconds ###
Returns an `elong` integer representing the current epoch (i.e., the
date since 0:00:00 UTC on the morning of 1 January 1970, expressed
in seconds (resp. in micro seconds).

### current-microseconds ###
Returns the number of microseconds since epoch.

### current-milliseconds ###
Returns the number of milliseconds since epoch.

### current-nanoseconds ###
Returns the number of nanoseconds since epoch.

### date->seconds ###
Converts from `date` into a number of seconds since epoch.

### date->nanoseconds ###
Converts from `date` into a number of nanoseconds since epoch.

### date->milliseconds ###
Converts from `date` into a number of milliseconds since epoch.

### seconds->date ###
Converts a number of seconds since eopch into a `date` object.

### milliseconds->date ###
Converts a number of milliseconds since eopch into a `date` object.

### nanoseconds->date ###
Converts a number of nanoseconds since eopch into a `date` object.

### seconds->gmtdate ###
Converts a number of seconds since eopch into a gmt `date` object.

### milliseconds->gmtdate ###
Converts a number of milliseconds since eopch into a gmt `date` object.

@deffnx {bigloo procedure} nanoeconds->date
@deffnx {bigloo procedure} milliseconds->date
@end deffn

@deffn {bigloo procedure} date->string date
@deffnx {bigloo procedure} date->utc-string date
@deffnx {bigloo procedure} seconds->string elong
@deffnx {bigloo procedure} seconds->utc-string elong
Construct a textual representation of the date passed in argument
@end deffn

@deffn {bigloo procedure} date-second date
Returns the number of seconds of a date, in the range `0...59}.
@end deffn

@deffn {bigloo procedure} date-nanosecond date
@deffnx {bigloo procedure} date-millisecond date
Returns the number of nano/milli seconds of a date (to be added to 
`date-second}).
@end deffn

@deffn {bigloo procedure} date-minute date
Returns the minute of a date, in the range `0...59}.
@end deffn

@deffn {bigloo procedure} date-hour date
Returns the hour of a date, in the range `0...23}.
@end deffn

@deffn {bigloo procedure} date-day date
Returns the day of a date, in the range `1...31}.
@end deffn

@deffn {bigloo procedure} date-wday date
@deffnx {bigloo procedure} date-week-day date
Returns the week day of a date, in the range `1...7}.
@end deffn

@deffn {bigloo procedure} date-yday date
@deffnx {bigloo procedure} date-year-day date
Returns the year day of a date, in the range `1...366}.
@end deffn

@deffn {bigloo procedure} date-month date
Returns the month of a date, in the range `1...12}.
@end deffn

@deffn {bigloo procedure} date-year date
Returns the year of a date.
@end deffn

@deffn {bigloo procedure} date-timezone date
Returns the timezone (in seconds) of a date.
@end deffn

@deffn {bigloo procedure} date-is-dst date
Returns `-1} if the information is not available, `0} is the
date does not contain daylight saving adjustment, `1} if it
contains a daylight saving adjustment.
@end deffn

@deffn {bigloo procedure} integer->second
Converts a Bigloo fixnum integer into a second number.
@end deffn

@deffn {bigloo procedure} day-seconds
Returns the number of seconds contained in one day.
@end deffn

@deffn {bigloo procedure} day-name int
@deffnx {bigloo procedure} day-aname int
Return the name and the abbreviated name of a week day.
@end deffn

@deffn {bigloo procedure} month-name int
@deffnx {bigloo procedure} month-aname int
Return the name and the abbreviated name of a month.
@end deffn

@deffn {bigloo procedure} date-month-length date
Return the length of the month of `date}.
@end deffn

@deffn {bigloo procedure} leap-year? int
Returns `#t} if and only if the year `int} is a leap year. 
Returns `#f} otherwise.
@end deffn

@deffn {bigloo procedure} rfc2822-date->date string
@deffnx {bigloo procedure} rfc2822-parse-date input-port
Parses RFC2822 string representing a date. These functions produce
a Bigloo date object.
@end deffn

@deffn {bigloo procedure} date->rfc2822-date date
Converts a Bigloo date into a string representation compliant with the RFC2822
format.
@end deffn

@deffn {bigloo procedure} iso8601-date->date string
@deffnx {bigloo procedure} iso8601-parse-date input-port
Parses ISO8601 string representing a date. These functions produce
a Bigloo date object.
@end deffn

@deffn {bigloo procedure} date->iso8601-date date
Converts a Bigloo date into a string representation compliant with the iso8601
format.
@end deffn



