;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Pp/circle.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Vladimir Tsyshevsky                               */
;*    Creation    :  Sat Aug 14 08:52:29 1999                          */
;*    Last change :  Mon Jan 14 13:58:47 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The circular displayer.                                          */
;*    -------------------------------------------------------------    */
;*    This module implements two library functions:                    */
;*                                                                     */
;*       display-circle <obj> . <port>                                 */
;*       write-circle <obj> . <port>                                   */
;*                                                                     */
;*    They both display potentially recursive objects. They use a      */
;*    CommonLisp like format to represent recursive references.        */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Input And Output@                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pp_circle
   
   (import  __error
            __hash
            __r4_output_6_10_3)
   
   (use     __type
            __bigloo
	    __param
            __tvector
            __structure
            __tvector
            __hash
	    __object
	    __ucs2
	    __unicode
	    __date
	    __thread
	    __bexit
	    __bignum
	    __rgc
	    __bit
	    
            __r4_numbers_6_5
            __r4_numbers_6_5_fixnum
            __r4_numbers_6_5_flonum
            __r4_numbers_6_5_flonum_dtoa
            __r4_characters_6_6
            __r4_equivalence_6_2
            __r4_booleans_6_1
            __r4_symbols_6_4
            __r4_strings_6_7
            __r4_pairs_and_lists_6_3
            __r4_input_6_10_2
            __r4_control_features_6_9
            __r4_vectors_6_8
            __r4_ports_6_10_1
	    
            __evenv)
   
   (extern  (export c-debugging-print "dprint"))
   
   (java    (export jvm-debugging-print "dprint"))
   
   (export  (write-circle ::obj
			  #!optional
			  (port::output-port (current-output-port)))
	    (display-circle ::obj
			    #!optional
			    (port::output-port (current-output-port)))
	    (c-debugging-print ::obj)
	    (jvm-debugging-print::string ::obj ::int)))

;*---------------------------------------------------------------------*/
;*    display-circle ...                                               */
;*---------------------------------------------------------------------*/
(define (display-circle o #!optional (port::output-port (current-output-port)))
   (circle-write/display o port #t))

;*---------------------------------------------------------------------*/
;*    write-circle ...                                                 */
;*---------------------------------------------------------------------*/
(define (write-circle o #!optional (port::output-port (current-output-port)))
   (circle-write/display o port #f))

;*---------------------------------------------------------------------*/
;*    for ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (for range . body)
   (let ((for (gensym 'for))
	 (stop (gensym 'stop)))
      `(let ((,stop ,(caddr range)))
	  (let ,for ((,(car range) ,(cadr range)))
	       (when (<fx ,(car range) ,stop)
		  ,@body
		  (,for (+fx ,(car range) 1)))))))

;*---------------------------------------------------------------------*/
;*    write/display ...                                                */
;*    -------------------------------------------------------------    */
;*    As we can't be sure that this module is initialized before       */
;*    used (for example when an error occurs during module             */
;*    intialization process), we can't use symbol to set               */
;*    flag. So is flag is equal to #t it means that                    */
;*    `write-display' is used to display and if flags is equal         */
;*    to #f it writes.                                                 */
;*---------------------------------------------------------------------*/
(define (circle-write/display obj port flag)
   (let* ((cache         '())
          (cache-count   0)
          (next-cardinal (let ((serial -1))
			    (lambda ()
			       (set! serial (+fx 1 serial))
			       serial))))

      (define (cache-lookup obj)
	 (cond
	    ((hashtable? cache)
	     (hashtable-get cache obj))
	    (else
	     (assq obj cache))))
      
      ;; first stage: register object components
      (define (register obj)
	 ;; do not register objects unique by their nature
	 (if (not (or (number? obj)
		      (symbol? obj)
		      (string? obj)
		      (ucs2-string? obj)
		      (date? obj)
		      (cnst? obj)
		      (null? obj)
		      (class? obj)))
	     (let ((match (cache-lookup obj)))
		(if match
                    (set-cdr! match #t)
		    (let ((entry (cons obj #f)))
                       (cond ((hashtable? cache)
                              (hashtable-put! cache obj entry))
                             ((>fx cache-count 64)
                              (let ((h (create-hashtable
					  :eqtest eq?
					  :bucket-expansion 2.0)))
				 (for-each (lambda (cell)
					      (hashtable-put! h (car cell) cell))
				    cache)
				 (set! cache h)
				 (set! cache-count -1)
				 (hashtable-put! cache obj entry)))
                             (else
                              (set! cache (cons entry cache))
                              (set! cache-count (+fx cache-count 1))))
		       (cond
			  ((pair? obj)
			   (register (car obj))
			   (register (cdr obj)))
			  ((vector? obj)
			   (for (i 0 (vector-length obj))
			      (register (vector-ref obj i))))
			  ((struct? obj)
			   (for (i 0 (struct-length obj))
			      (register (struct-ref obj i))))
			  ((object? obj)
			   (let* ((klass (object-class obj))
				  (fields (class-all-fields klass)))
			      (for (i 0 (vector-length fields))
				 (let* ((f (vector-ref fields i))
					(gv (class-field-accessor f)))
				    (register (gv obj))))))
			  ((cell? obj)
			   (register (cell-ref obj)))))))))

      (define (putchar ch)
	 ($display-char ch port))

      (define (space)
	 (putchar #\space))

      (define (sharp)
	 (putchar #\#))
      
      ;; do register the value
      (register obj)
      ;; do output 
      (let output-component ((obj obj))
	 (let loop-matched ((obj obj)
			    (match (cache-lookup obj)))
	    (if match
                (let ((value (cdr match)))
                   (cond
		      ((fixnum? value)
		       ;; emit reference like #0#
		       (sharp)
		       (output-component value)
		       (sharp))
		      (value
		       ;;; emit target as #0=<the object>
		       (let ((serial (next-cardinal)))
			  (set-cdr! match serial)
			  (sharp)
			  (output-component serial)
			  (putchar #\=)
			  ;; emit object itself
			  (loop-matched obj #f)))
		      (else
		       ;; object referenced just once, just emit it
		       (loop-matched obj #f))))
		(cond
		   ;; fixnum display
		   ((fixnum? obj)
		    ($display-fixnum obj port))
		   ;; char display
		   ((char? obj)
		    (if flag
			($display-char obj port)
			($write-char obj port)))
		   ;; symbol display
		   ((symbol? obj)
		    (if flag
			(display-symbol obj port)
			(write-symbol obj port)))
		   ;; string display
		   ((string? obj)
		    (if flag
			(display-string obj port)
			(write-string (string-for-read obj) port)))
		   ;; pair display
		   ((pair? obj)
		    (putchar #\()
		    (let loop ((l obj))
		       (output-component (car l))
		       (let ((rest (cdr l)))
			  (if (null? rest)
			      (putchar #\))
			      (let ((match (cache-lookup rest)))
				 (if (or (not (pair? rest))
					 (and match (cdr match)))
				     (begin
					(display-string " . " port)
					(loop-matched rest match)
					(putchar #\)))
				     (begin
					(space)
					(loop rest))))))))
		   ;; classes
		   ((class? obj)
		    (display obj port))
		   ;; vector display
		   ((vector? obj)
		    (sharp)
		    (let ((tag (vector-tag obj)))
		       (if (>fx tag 0)
			   (if (<fx tag 100)
			       (begin
				  (if (>fx tag 10) (putchar #\0))
				  (putchar #\0))
			       (write tag port))))
		    (let ((len (vector-length obj)))
		       (putchar #\()
		       (let loop ((i 0))
			  (if (not (=fx i len))
			      (begin
				 (output-component (vector-ref obj i))
				 (let ((next (+fx 1 i)))
				    (if (not (=fx next len))
					(space))
				    (loop (+fx 1 i))))))
		       (putchar #\))))
		   ;; struct display
		   ((struct? obj)
		    (display-string #"#{" port)
		    (write (struct-key obj) port)
		    (space)
		    (let ((len (struct-length obj)))
		       (let loop ((i 0))
			  (if (not (=fx i len))
			      (begin
				 (output-component (struct-ref obj i))
				 (let ((next (+fx 1 i)))
				    (if (not (=fx next len))
					(space))
				    (loop (+fx 1 i)))))))
		    (putchar #\}))
		   ;; cell display
		   ((cell? obj)
		    (display-string #"#<cell:" port)
		    (output-component (cell-ref obj))
		    (display-string #">" port))
		   ;; object display
		   ((object? obj)
		    (object-print obj port (lambda (x . p)
					      (output-component x))))
		   ;; ucs2 string display
		   ((ucs2-string? obj)
		    (if flag
			(display-ucs2string obj port)
			(write-ucs2string obj port)))
		   ;; ucs2 display
		   ((ucs2? obj)
		    (if flag
			($display-ucs2 obj port)
			($write-ucs2 obj port)))
		   ;; flonum display
		   ((flonum? obj)
		    (display-flonum obj port))
		   ;; date
		   ((date? obj)
		    (if flag
			(display obj port)
			(write obj port)))
		   ;; mutex
		   ((mutex? obj)
		    (display-string "#<mutex:" port)
		    (display (mutex-name obj) port)
		    (display-string ">" port))
		   ;; condition variable
		   ((condition-variable? obj)
		    (display-string "#<condition-variable:" port)
		    (display (condition-variable-name obj) port)
		    (display-string ">" port))
		   ;; default displayer
		   (else
		    (write obj port)))))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    c-debugging-print ...                                            */
;*---------------------------------------------------------------------*/
(define (c-debugging-print obj)
   (let ((port (current-output-port)))
      (write-circle obj port)
      (newline port)
      obj))

;*---------------------------------------------------------------------*/
;*    jvm-debugging-print ...                                          */
;*---------------------------------------------------------------------*/
(define (jvm-debugging-print obj printer-num)
   (with-output-to-string
      (lambda ()
	 ((case printer-num
	     ((1) write)
	     ((2) write-circle)
	     ((3) display-circle)
	     (else display))
	  obj (current-output-port)))))
   
