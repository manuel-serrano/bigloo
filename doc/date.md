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

Date objects are representation of dates. Two dates are `equal?` is
they represent the same epoch time (i.e., the same number of seconds
since January 1, 1970). For instance:

```bigloo
(let ((d1 (make-date :hour 8 :day 15 :month 1 :year 2015 :timezone 0))
      (d2 (make-date :hour 10 :day 15 :month 1 :year 2015 :timezone 7200)))
   (equal? d1 d2) &rarr; #t
```

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
Returns the current date as a `date` object.

### current-timezone ###
Returns the current timezone of the host.

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
Converts a number of seconds since eopch into a `date` object in
the current timezone.

### milliseconds->date ###
Converts a number of milliseconds since eopch into a `date` object in
the current timezone.

### nanoseconds->date ###
Converts a number of nanoseconds since eopch into a `date` object
in the current timezone.

### seconds->gmtdate ###
<!-- [:@C] -->
Converts a number of seconds since eopch into a gmt `date` object.

### milliseconds->gmtdate ###
<!-- [:@C] -->
Converts a number of milliseconds since eopch into a gmt `date` object.

### date-second ###
Returns the number of seconds of a date, in the range `0`...`59`.

### date-millisecond ###
Returns the number of milliseconds of a date.

### date-nanosecond ###
Returns the number of nanoseconds of a date.

### date-minute ###
Returns the minute of a date, in the range `0...59`.

### date-hour ###
Returns the hour of a date, in the range `0...23`.

### date-day ###
Returns the day of a date, in the range `1...31`.

### date-week-day ###
Returns the week day of a date, in the range `1...71.

### date-year-day ###
Returns the year day of a date, in the range `1...366`.

### date-month ###
Returns the month of a date, in the range `1...12`.

### date-year ###
Returns the year of a date.

### date-timezone ###
Returns the timezone (in seconds) of a date.

### date-is-dst ###
Returns `-1` if the information is not available, `0` is the
date does not contain daylight saving adjustment, `1` if it
contains a daylight saving adjustment.

### integer->second ###
Converts a Bigloo fixnum integer into a second number.

### day-seconds ###
Returns the number of seconds contained in one day.

### day-name ###
Return the name of a week day.

### day-aname ###
Return the abbreviated name of a week day.

### month-name ###
Return the name of a month.

### month-aname ###
Return the abbreviated name of a month.

### date-month-length ###
Return the length of the month of `date`.

### leap-year? ###
Returns `#t` if and only if the year is a leap year. 
Returns `#f` otherwise.

String Conversions
------------------

### date ###
Returns the current date as a string.

### date->string ###
Returns a string readable interpreration of the date.

### date->utc-string ###
Returns a string readable interpreration of the date.

### seconds->string ###
Constructs a textual representation of the date expressed in seconds.

### seconds->utc-string ###
Constructs a textual representation of the date passed in utc seconds.

### rfc2822-parse-date ###
Parses the input-port to produce a date object from a RFC2822 
textual representation.

### rfc2822-date->date ###
Converts the RFC2822 string representing a date.

### date->rfc2822-date ###
Converts a Bigloo date into a string representation compliant with the RFC2822
format.

### iso8601-date->date ###
Converts the ISO8601 string representing a date. 

### iso8601-parse-date ###
Parses the input-port to produce a date object from an ISO8601 textual
representation.

### date->iso8601-date ###
Converts a Bigloo date into a string representation compliant with the iso8601
format.
