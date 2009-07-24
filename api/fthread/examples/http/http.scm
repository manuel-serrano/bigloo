;*=====================================================================*/
;*    serrano/prgm/project/bigloo/fthread/examples/http/http.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Wed Jul  1 14:43:06 2009 (cnicolas)               */
;*    Copyright   :  2002-2009 Manuel Serrano                          */
;*    -------------------------------------------------------------    */
;*    An HTTP server implemented with fair threads. This server        */
;*    accepts several concurent requests. It can serve HTML files or   */
;*    .scm files or .sh files.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module fairthread-http
   (library fthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    Global parameters ...                                            */
;*---------------------------------------------------------------------*/
(define *port-num* #f)
(define *root-directory* (pwd))
(define *thread-debug* 0)

;*--------------------------------------------------------------------*/
;*    make-connect-signal ...                                         */
;*--------------------------------------------------------------------*/
(define (make-connect-signal::socket s::socket . opt)
   (make-asynchronous-signal (lambda (sig) (socket-accept s))))

;*--------------------------------------------------------------------*/
;*    make-input-signal ...                                           */
;*--------------------------------------------------------------------*/
(define (make-input-signal::bbool p::input-port . opt)
   (make-asynchronous-signal (lambda (sig)
				(let ((c (peek-char p)))
				   (not (eof-object? c))))))

;*--------------------------------------------------------------------*/
;*    make-output-signal ...                                          */
;*--------------------------------------------------------------------*/
(define (make-output-signal p::output-port s::bstring . opt)
   (make-asynchronous-signal (lambda (sig)
			    (display-string s p)
			    (flush-output-port p))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (args-parse (cdr args)
      ((("-h" "--help") (help "This help message"))
       (args-parse-usage #f)
       (exit 0))
      (("-g" (help "Increase debug"))
       (set! *thread-debug* (+fx 1 *thread-debug*)))
      (("-p?port" (help "Port number"))
       (set! *port-num* (string->integer port)))
      (else
       (set! *root-directory* else)))
   (let* ((svr (make-socket-server))
          (thd (instantiate::fthread (body (make-http-server svr))
				     (name 'http-server)))
          (ts (thread-start! thd)))
      (scheduler-start!)
      (fprint (current-error-port) "Shuting down http server...")
      (socket-shutdown svr)))

;*---------------------------------------------------------------------*/
;*    make-socket-server ...                                           */
;*---------------------------------------------------------------------*/
(define (make-socket-server)
   (let ((s (if *port-num*
                (make-server-socket *port-num*)
                (make-server-socket))))
      (print "Http server started: " (socket-port-number s))
      s))

;*---------------------------------------------------------------------*/
;*    make-http-server ...                                             */
;*---------------------------------------------------------------------*/
(define (make-http-server s::socket)
   (lambda ()
      (let loop ((n 0))
         (let ((s2 (thread-await! (make-connect-signal s))))
	    (thread-start! (instantiate::fthread
			      (body (lambda () (http-eval s2 n)))))
            (thread-yield!)
            (loop (+fx n 1))))))

;*---------------------------------------------------------------------*/
;*    http-eval ...                                                    */
;*---------------------------------------------------------------------*/
(define (http-eval s::socket num::int)
   (define (readline)
      (if (thread-await! (make-input-signal (socket-input s)))
	  (let ((line (read-line (socket-input s))))
	     (write line)
	     (display* " " (string-length line))
	     (newline)
	     line)
	  ""))
   (let* ((lines (let loop ((line (readline)))
                    (thread-yield!)
                    (if (=fx (string-length line) 0)
                        '()
                        (cons (substring line 0 (-fx (string-length line) 1))
                              (loop (readline))))))
          (line (car lines)))
      (string-case line
         ("VERSION"
          (http-reply s "plain" "Web server V-1.0")
          (socket-shutdown s #f))
         ("v"
          (http-reply s "plain" "Web server V-1.0")
          (socket-shutdown s #f))
         ("TEST"
          (http-get "index.html" s)
          (socket-shutdown s #f))
         ((: "GET " (+ (out " \n\t\"")))
          (http-get (the-substring 4 (the-length)) s)
          (socket-shutdown s #f))
         ((: "GET \"" (+ (out " \n\t\"")) "\"")
          (http-get (the-substring 5 (-fx (the-length) 1)) s)
          (socket-shutdown s #f))
         (else
          (http-reply s "Unknown request -- \"" line "\"")
          (socket-shutdown s #f)))))
      
;*---------------------------------------------------------------------*/
;*    http-reply ...                                                   */
;*---------------------------------------------------------------------*/
(define (http-reply socket::socket kind . str)
   (let ((p (socket-output socket)))
      (thread-await! (make-output-signal p "HTTP/1.0 200 Ok\r
Server: test_httpd/%x\r
Connection: close\r
Content-type: text/"))
      (thread-await! (make-output-signal p kind))
      (thread-await! (make-output-signal p "\r\n\r\n"))
      ;; display all the accumulated strings
      (for-each (lambda (s) (thread-await! (make-output-signal p s))) str)
      ;; mark the end of output
      (thread-await! (make-output-signal p "\r\n"))))

;*---------------------------------------------------------------------*/
;*    http-get ...                                                     */
;*---------------------------------------------------------------------*/
(define (http-get file s::socket)
   (let* ((fname (if (char=? (string-ref file 0) (file-separator))
                     (string-append *root-directory* file)
                     file)))
      (case (string->symbol (suffix fname))
         ((scm)
          (http-get-scm fname s))
         ((sh)
          (http-get-sh fname s))
	 ((html htm)
	  (http-get-html fname s))
         (else
          (http-get-file fname s)))))

;*---------------------------------------------------------------------*/
;*    http-get-scm ...                                                 */
;*---------------------------------------------------------------------*/
(define (http-get-scm file s::socket)
   (let ((proc (run-process "bigloo" "-i" "-s" file output: pipe:)))
      (http-get-input (process-output-port proc) s)))

;*---------------------------------------------------------------------*/
;*    http-get-sh ...                                                  */
;*---------------------------------------------------------------------*/
(define (http-get-sh file s::socket)
   (let ((proc (run-process "sh" "-f" file output: pipe:)))
      (http-get-input (process-output-port proc) s)))

;*---------------------------------------------------------------------*/
;*    http-get-html ...                                                */
;*---------------------------------------------------------------------*/
(define (http-get-html fname s::socket)
   (if (file-exists? fname)
       (let ((p (open-input-file fname)))
          (http-get-input p s)
          (close-input-port p))
       (http-reply s "plain" "Can't find file \"" fname "\"")))

;*---------------------------------------------------------------------*/
;*    http-get-file ...                                                */
;*---------------------------------------------------------------------*/
(define (http-get-file fname s::socket)
   (if (file-exists? fname)
       (let ((p (open-input-file fname)))
          (http-get-input p s)
          (close-input-port p))
       (http-reply s "plain" "Can't find file \"" fname "\"")))
      
;*---------------------------------------------------------------------*/
;*    http-get-input ...                                               */
;*---------------------------------------------------------------------*/
(define (http-get-input p::input-port s::socket)
   (let ((kind "plain")
	 (newline (string #\Return #\Newline)))
      (define (readlines)
	 (if (thread-await! (make-input-signal p))
	     (read-line p)))
      (let loop ((res '())
		 (num 0))
	 (let ((v (readlines)))
	    (if (and (not (pair? res)) v (> (string-length v) 0))
	       ; Check the first line of output for <html> or <?xml...
	       (if (char=? #\< (string-ref v 0))
		   (set! kind "html")))
	    
	    (thread-yield!)
	    (if (not v)
		(apply http-reply s kind (reverse! res))
		(loop (cons* newline v res)
		      (+ num (string-length v))))))))
