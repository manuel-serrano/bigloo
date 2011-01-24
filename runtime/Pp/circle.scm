;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Pp/circle.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Vladimir Tsyshevsky                               */
;*    Creation    :  Sat Aug 14 08:52:29 1999                          */
;*    Last change :  Sun Jan 23 07:10:36 2011 (serrano)                */
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
            __r4_output_6_10_3)
   
   (use     __type
            __bigloo
	    __param
            __tvector
            __structure
            __tvector
	    __object
	    __ucs2
	    __unicode
	    __date
	    __thread
	    __bexit
	    __bignum
	    
            __r4_numbers_6_5
            __r4_numbers_6_5_fixnum
            __r4_numbers_6_5_flonum
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
	  (next-cardinal (let ((serial -1))
			    (lambda ()
			       (set! serial (+fx 1 serial))
			       serial))))
      (define (register-object obj)
	 (let* ((class (object-class obj))
		(class-name (class-name class))
		(fields (class-fields class)))
	    (if (class-fields? fields)
		(let loop ((fields fields)
			   (class class))
		   (cond
		      ((null? fields)
		       (let ((super (class-super class)))
			  (if (class? super)
			      ;; we have to register
			      ;; the super class fields
			      (loop (class-fields super) super))))
		      ((eq? fields #unspecified)
		       (loop '() class))
		      (else
		       (let* ((f (car fields))
			      (gv (class-field-accessor f)))
			  (if (class-field-indexed? f)
			      (let ((gl (class-field-len-accessor f)))
				 (let liip ((l (-fx (gl obj) 1)))
				    (if (=fx l -1)
					(loop (cdr fields) class)
					(begin
					   (register (gv obj l))
					   (liip (-fx l 1))))))
			      (begin
				 (register (gv obj))
				 (loop (cdr fields) class))))))))))
      ;; first stage: register object components
      (define (register obj)
	 ;; do not register objects unique by their nature
	 (if (not (or (number? obj)
		      (symbol? obj)
		      (string? obj)
		      (cnst? obj)
		      (null? obj)
		      (class? obj)))
	     (let ((match (assq obj cache)))
		(if match
		    (set-cdr! match #t)
		    (begin
		       (set! cache (cons (cons obj #f) cache))
		       (cond
			  ((pair? obj)
			   (register (car obj))
			   (register (cdr obj)))
			  ((vector? obj)
			   (let ((len (vector-length obj)))
			      (let loop-count ((count 0))
				 (if (not (>=fx count len))
				     (begin
					(register (vector-ref obj count))
					(loop-count (+fx count 1)))))))
			  ((struct? obj)
			   (let ((len (struct-length obj)))
			      (let loop-count ((count 0))
				 (if (not (>=fx count len))
				     (begin
					(register (struct-ref obj count))
					(loop-count (+fx count 1)))))))
			  ((object? obj)
			   (register-object obj))))))))
      ;; do register the value
      (register obj)
      ;; do output 
      (let* ((putchar (lambda (ch) ($display-char ch port)))
	     (space   (lambda () (putchar #\space)))
 	     (sharp   (lambda () (putchar #\#))))
	 (let output-component ((obj obj))
	    (let loop-matched ((obj   obj)
			       (match (assq obj cache)))
	       (if match
		   (let ((value (and match (cdr match))))
		      (cond
			 ((fixnum? value)
			  ;; emit reference like #0#
			  (sharp)
			  (output-component value)
			  (sharp))
			 (value
			  ;; emit target as #0=<the object>
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
				 (let ((match (assq rest cache)))
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
		       (write obj port))))))))
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
   
