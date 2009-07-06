;<font size="-3"><pre>
;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Runtime/runtime.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul  8 09:13:09 1995                          */
;*    Last change :  Fri Nov  5 16:01:27 2004 (serrano)                */
;*    -------------------------------------------------------------    */
;*    An example of foreign function interface.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_runtime
   
   (extern (include "sys/time.h")
	   (include "sys/resource.h")
	   (macro RUSAGE_SELF::int "RUSAGE_SELF")
	   (type timeval  (struct (tv-sec::long "tv_sec")
				  (tv-usec::long "tv_usec"))
		 "struct timeval")
	   (type timezone (struct (tz-minuteswest::int "tz_minuteswest")
				  (tz-dsttime::int "tz_dsttime"))
		 "struct timezone")
	   (type rusage   (struct (ru-utime::timeval "ru_utime")
				  (ru-stime::timeval "ru_stime")
				  (ru-maxrss::int "ru_maxrss")
				  (ru-ixrss::int "ru_ixrss")
				  (ru-idrss::int "ru_idrss")
				  (ru-isrss::int "ru_isrss")
				  (ru_minflt::int "ru_minflt")
				  (ru_majflt::int "ru_majflt")
				  (ru_nswap::int "ru_nswap")
				  (ru_inblock::int "ru_inblock")
				  (ru_oublock::int "ru_oublock")
				  (ru_msgsnd::int "ru_msgsnd")
				  (ru_msgrcv::int "ru_msgrcv")
				  (ru_nsignals::int "ru_nsignals")
				  (ru_nvcsw::int "ru_nvcsw")
				  (ru_nivcsw::int "ru_nivcsw"))
		 "struct rusage")
	   (getrusage::int (int rusage*) "getrusage"))

   (export (runtime::pair ::procedure))) 

;*---------------------------------------------------------------------*/
;*    runtime ...                                                      */
;*---------------------------------------------------------------------*/
(define (runtime thunk)
   (let ((rusage (make-rusage*)))
      (let* ((i     (getrusage RUSAGE_SELF rusage))
             (tu    (rusage*-ru-utime rusage))
             (ts    (rusage*-ru-stime rusage))
             (usec  (timeval*-tv-sec tu))
             (umsec (timeval*-tv-usec tu))
             (ssec  (timeval*-tv-sec ts))
             (smsec (timeval*-tv-usec ts)))
         (let ((val (thunk)))
            (let ((i  (getrusage RUSAGE_SELF rusage))
                  (tu (rusage*-ru-utime rusage))
                  (ts (rusage*-ru-stime rusage)))
               (cons val
		     (cons (+ (- (timeval*-tv-sec tu) usec)
			      (/ (- (timeval*-tv-usec tu) umsec)
				 1000000))
			   (+ (- (timeval*-tv-sec ts) ssec)
			      (/ (- (timeval*-tv-usec ts) smsec)
				 1000000)))))))))


(display "?* ")
(let loop ((exp (read)))
   (if (eof-object? exp)
       'done
       (let ((t (runtime (lambda () (with-exception-handler
				       (lambda (e)
					  (error-notify e)
					  #unspecified)
				       (lambda ()
					  (eval exp)))))))
	  (print (car t) "  [" (car (cdr t)) " user + " (cdr (cdr t)) " sys]")
	  (display "?* ")
	  (loop (read)))))
;</pre></font>
