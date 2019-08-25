;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Pp/pp.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 27 14:10:31 1993                          */
;*    Last change :  Sun Aug 25 09:12:32 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Le pretty-printer de Marc Feeley.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __pp

   (import  __error
	    __r4_output_6_10_3)
   
   (use     __type
	    __bigloo
	    __param
	    __object
	    __thread
	    __bexit
	    __bignum
	    __tvector
	    __structure
	    __tvector
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

   (export (pp::unspecified exp . port)
	   *pp-width*
	   *pp-case*))
 
;*---------------------------------------------------------------------*/
;*    the *pp-case* ...                                                */
;*---------------------------------------------------------------------*/
(define *pp-case* 'respect)

;*---------------------------------------------------------------------*/
;*    *pp-width* ...                                                   */
;*---------------------------------------------------------------------*/
(define *pp-width* 80)

;*---------------------------------------------------------------------*/
;*    pp ...                                                           */
;*---------------------------------------------------------------------*/
(define (pp exp . port)
   (let ((port (if (null? port)
		   (current-output-port)
		   (let ((port (car port)))
		      (if (not (output-port? port))
			  (error "pp" "not an output-port" port)
			  port)))))
      (generic-write exp #f *pp-width* (lambda (s) (display s port) #t))
      #unspecified))

;; les prefix des vecteurs
(define (vector-prefix obj)
   (let ((tag (vector-tag obj)))
      (if (=fx tag 0)
	  "#"
	  (if (>=fx tag 100)
	      (string-append "#" (number->string tag))
	      (if (>=fx tag 10)
		  (string-append "#0" (number->string tag))
		  (string-append "#00" (number->string tag)))))))

; 'generic-write' is a procedure that transforms a Scheme data value (or
; Scheme program expression) into its textual representation.  The interface
; to the procedure is sufficiently general to easily implement other useful
; formatting procedures such as pretty printing, output to a string and
; truncated output.
;
; Parameters:
;
;   OBJ       Scheme data value to transform.
;   DISPLAY?  Boolean, controls whether characters and strings are quoted.
;   WIDTH     Extended boolean, selects format:
;               #f = single line format
;               integer > 0 = pretty-print (value = max nb of chars per line)
;   OUTPUT    Procedure of 1 argument of string type, called repeatedly
;               with successive substrings of the textual representation.
;               This procedure can return #f to stop the transformation.
;
; The value returned by 'generic-write' is undefined.
;
; Examples:
;
;   (write obj)   = (generic-write obj #f #f display-string)
;   (display obj) = (generic-write obj #t #f display-string)
;
; where display-string = (lambda (s) (for-each write-char (string->list s)) #t)

(define (generic-write obj display? width output)

  (define (read-macro? l)
    (define (length1? l) (and (pair? l) (null? (cdr l))))
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((quote quasiquote unquote unquote-splicing) (length1? tail))
        (else                                        #f))))

  (define (read-macro-body l)
    (cadr l))

  (define (read-macro-prefix l)
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((quote)            "'")
        ((quasiquote)       "`")
        ((unquote)          ",")
        ((unquote-splicing) ",@"))))

  (define (out str col)
    (and col (output str) (+fx col (string-length str))))

  (define (wr obj col)
     
     (define (wr-expr expr col)
	(if (read-macro? expr)
	    (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
	    (wr-lst expr col)))
     
     (define (wr-lst l col)
	(if (pair? l)
	    (let loop ((l (cdr l)) (col (wr (car l) (out "(" col))))
	       (and col
		    (cond ((pair? l) (loop (cdr l) (wr (car l) (out " " col))))
			  ((null? l) (out ")" col))
			  (else      (out ")" (wr l (out " . " col)))))))
	    (out "()" col)))
     
     (cond ((match-case obj
	       ((comment (? fixnum?) (? string?))
		#t)
	       (else
		#f))
	    (let ((add (- *pp-width* (string-length (caddr obj)) 3)))
	       (if (<=fx add 0)
		   (out (caddr obj) col)
		   (out (string-append (caddr obj) (make-string add #\space))
			col))))
	   ((pair? obj)
	    (wr-expr obj col))
	   ((null? obj)
	    (wr-lst obj col))
	   ((vector? obj)
	    (wr-lst (vector->list obj)
		    (out (vector-prefix obj) col)))
	   ((boolean? obj)
	    (out (if obj "#t" "#f") col))
	   ((number? obj)
	    (cond
	       ((elong? obj)
		(out (string-append "#e" (number->string obj)) col))
	       ((llong? obj)
		(out (string-append "#l" (number->string obj)) col))
	       ((uint8? obj)
		(out (string-append "#u8:" (number->string obj)) col))
	       ((uint16? obj)
		(out (string-append "#u16:" (number->string obj)) col))
	       ((uint32? obj)
		(out (string-append "#u32:" (number->string obj)) col))
	       ((int8? obj)
		(out (string-append "#s8:" (number->string obj)) col))
	       ((int16? obj)
		(out (string-append "#s16:" (number->string obj)) col))
	       ((int32? obj)
		(out (string-append "#s32:" (number->string obj)) col))
	       (else
		(out (number->string obj) col))))
	   ((symbol? obj)
	    (let ((p (open-output-string)))
	       (if display?
		   (display obj p)
		   (write obj p))
	       (case *pp-case*
		  ((respect)
		   (out (close-output-port p) col))
		  ((upper)
		   (out (string-upcase (close-output-port p))
			col))
		  (else
		   (out (string-downcase (close-output-port p))
			col)))))
	   ((procedure? obj)
	    (out (with-output-to-string (lambda () (display obj))) col))
	   ((string? obj)
	    (let ((obj (string-for-read obj)))
	       (if display?
		   (out obj col)
		   (let loop ((i 0)
			      (j 0) 
			      (col (out (if (bigloo-strict-r5rs-strings)
					    "#\""
					    "\"")
					col)))
		      (if (and col (<fx j (string-length obj)))
			  (let ((c (string-ref obj j)))
			     (loop i (+fx j 1) col))
			  (out "\""
			       (out (substring obj i j)
				    col)))))))
	   ((char? obj)
	    (if display?
		(out (make-string 1 obj) col)
		(let ((p (open-output-string)))
		   (write obj p)
		   (out (close-output-port p) col))))
	   ((input-port? obj)
	    (out "#[input-port]" col))
	   ((output-port? obj)
	    (out "#[output-port]" col))
	   ((eof-object? obj)
	    (out "#[eof-object]" col))
	   ((object? obj)
	    (out (string-append "#|"
		    (symbol->string (class-name (object-class obj)))
		    "|") col))
	   (else
	    (out (let ((p (open-output-string)))
		    (write obj p)
		    (close-output-port p))
		 col))))

  (define (pp obj col)

    (define (spaces n col)
      (if (>fx n 0)
        (if (>fx n 7)
          (spaces (-fx n 8) (out "        " col))
          (out (substring "        " 0 n) col))
        col))

    (define (indent to col)
      (and col
           (if (<fx to col)
             (and (out (make-string 1 #\newline) col) (spaces to 0))
             (spaces (-fx to col) col))))

    (define (pr obj col extra pp-pair)
      (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
        (let ((result '())
              (left (min (+fx (-fx (-fx width col) extra) 1) max-expr-width)))
          (generic-write obj display? #f
            (lambda (str)
              (set! result (cons str result))
              (set! left (-fx left (string-length str)))
              (>fx left 0)))
          (if (>fx left 0) ; all can be printed on one line
            (out (reverse-string-append result) col)
            (if (pair? obj)
              (pp-pair obj col extra)
              (pp-list (vector->list obj)
		       (out (vector-prefix obj) col)
		       extra pp-expr))))
        (wr obj col)))

    (define (pp-expr expr col extra)
      (if (read-macro? expr)
        (pr (read-macro-body expr)
            (out (read-macro-prefix expr) col)
            extra
            pp-expr)
        (let ((head (car expr)))
          (if (symbol? head)
            (let ((proc (style head)))
              (if proc
                (proc expr col extra)
                (if (>fx (string-length (symbol->string! head))
                       max-call-head-width)
                  (pp-general expr col extra #f #f #f pp-expr)
                  (pp-call expr col extra pp-expr))))
            (pp-list expr col extra pp-expr)))))

    ; (head item1
    ;       item2
    ;       item3)
    (define (pp-call expr col extra pp-item)
      (let ((col* (wr (car expr) (out "(" col))))
        (and col
             (pp-down (cdr expr) col* (+fx col* 1) extra pp-item))))

    ; (item1
    ;  item2
    ;  item3)
    (define (pp-list l col extra pp-item)
      (let ((col (out "(" col)))
        (pp-down l col col extra pp-item)))

    ; (item1 item2 item3
    ;    item4)
    (define (pp-defun-form expr col extra pp-item)
       (let* ((col  (out "({}" col))
	      (col2 (wr (car expr) col))
	      (col3 (wr (cadr expr) col2)))
	  (pp-down (cddr expr) col3 (+fx col3 1) extra pp-item)))
       
    (define (pp-down l col1 col2 extra pp-item)
      (let loop ((l l) (col col1))
        (and col
             (cond ((pair? l)
                    (let ((rest (cdr l)))
                      (let ((extra (if (null? rest) (+fx extra 1) 0)))
                        (loop rest
                              (pr (car l) (indent col2 col) extra pp-item)))))
                   ((null? l)
                    (out ")" col))
                   (else
                    (out ")"
                         (pr l
                             (indent col2 (out "." (indent col2 col)))
                             (+fx extra 1)
                             pp-item)))))))

    (define (pp-general expr col extra named? pp-1 pp-2 pp-3)

      (define (tail1 rest col1 col2 col3)
        (if (and pp-1 (pair? rest))
          (let* ((val1 (car rest))
                 (rest (cdr rest))
                 (extra (if (null? rest) (+fx extra 1) 0)))
            (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
          (tail2 rest col1 col2 col3)))

      (define (tail2 rest col1 col2 col3)
        (if (and pp-2 (pair? rest))
          (let* ((val1 (car rest))
                 (rest (cdr rest))
                 (extra (if (null? rest) (+fx extra 1) 0)))
            (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
          (tail3 rest col1 col2)))

      (define (tail3 rest col1 col2)
        (pp-down rest col2 col1 extra pp-3))

      (let* ((head (car expr))
             (rest (cdr expr))
             (col* (wr head (out "(" col))))
        (if (and named? (pair? rest))
          (let* ((name (car rest))
                 (rest (cdr rest))
                 (col** (wr name (out " " col*))))
            (tail1 rest (+fx col indent-general) col** (+fx col** 1)))
          (tail1 rest (+fx col indent-general) col* (+fx col* 1)))))

    (define (pp-expr-list l col extra)
      (pp-list l col extra pp-expr))

    (define (pp-expr-defun l col extra)
      (pp-defun-form l col extra pp-expr))
    
    (define (pp-define expr col extra)
      (pp-general expr col extra #f pp-expr-list #f pp-expr)
      (out #"\n" 0))

    (define (pp-defun expr col extra)
      (pp-general expr col extra #t pp-expr-defun #f pp-expr)
      (out #"\n" 0))

    (define (pp-let expr col extra)
       (pp-general expr col extra #f pp-expr-list #f pp-expr))
    
    ; (item1 item2
    ;     item3
    ;     item4)
    (define (pp-lambda expr col extra)
       (let* ((col (out "(" col))
	      (col2 (wr (car expr) col))
	      (col3 (out " " col2))
	      (col4 (wr (cadr expr) col3)))
	 (pp-down (cddr expr) col3 (+fx col 2) extra pp-expr)))
   
    (define (pp-comment expr col extra)
       (match-case expr
	  ((comment (and (? fixnum?) ?column) (and (? string?) ?string))
	   (let ((add (- *pp-width* (string-length string) 3)))
	      (if (=fx column 0)
		  (if (>fx add 0)
		      (out (string-append string (make-string add #\space))
			   0)
		      (out string 0))
		  (if (>fx add 0)
		      (out (string-append string (make-string add #\space))
			   col)
		      (out string col)))))
	  (else
	   (pp-general expr col extra #f pp-expr #f pp-expr))))
       
    (define (pp-if expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr))

    (define (pp-cond expr col extra)
      (pp-call expr col extra pp-expr-list))

    (define (pp-case expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr-list))
    
    (define (pp-and expr col extra)
      (pp-call expr col extra pp-expr))

    (define (pp-let expr col extra)
      (let* ((rest (cdr expr))
             (named? (and (pair? rest) (symbol? (car rest)))))
        (pp-general expr col extra named? pp-expr-list #f pp-expr)))

    (define (pp-begin expr col extra)
      (pp-general expr col extra #f #f #f pp-expr))

    (define (pp-do expr col extra)
      (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

    ; define formatting style (change these to suit your style)

    (define indent-general 2)

    (define max-call-head-width 5)

    (define max-expr-width 50)

    (define (style head)
      (case (if (eq? *pp-case* 'respect)
		(string->symbol (symbol->string! head))
		head)
        ((lambda)                                            pp-lambda)
        ((let* letrec)                                       pp-let)
	((define define-inline define-method define-generic) pp-define)
	((module)                                            pp-define)
	((defun de)                                          pp-defun)
        ((if set!)                                           pp-if)
        ((cond)                                              pp-cond)
        ((case)                                              pp-case)
        ((and or)                                            pp-and)
        ((let)                                               pp-let)
        ((begin)                                             pp-begin)
        ((do)                                                pp-do)
	((comment)                                           pp-comment)
        (else                                                #f)))

    (pr obj col 0 pp-expr))

  (if width
    (out (make-string 1 #\newline) (pp obj 0))
    (wr obj 0)))

; (reverse-string-append l) = (apply string-append (reverse l))

(define (reverse-string-append l)

  (define (rev-string-append l i)
    (if (pair? l)
      (let* ((str (car l))
             (len (string-length str))
             (result (rev-string-append (cdr l) (+fx i len))))
        (let loop ((j 0) (k (-fx (-fx (string-length result) i) len)))
          (if (<fx j len)
            (begin
              (string-set! result k (string-ref str j))
              (loop (+fx j 1) (+fx k 1))) 
            result)))
      (make-string i)))

  (rev-string-append l 0))

; (object->string obj) returns the textual representation of 'obj' as a
; string.
;
; note: (write obj) = (display (object->string obj))

(define (object->string obj)
  (let ((result '()))
    (generic-write obj #f #f (lambda (str) (set! result (cons str result)) #t))
    (reverse-string-append result)))

; (object->limited-string obj limit) returns a string containing the first
; 'limit' characters of the textual representation of 'obj'.

(define (object->limited-string obj limit)
  (let ((result '()) (left limit))
    (generic-write obj #f #f
      (lambda (str)
        (let ((len (string-length str)))
          (if (>fx len left)
            (begin
              (set! result (cons (substring str 0 left) result))
              (set! left 0)
              #f)
            (begin
              (set! result (cons str result))
              (set! left (-fx left len))
              #t)))))
    (reverse-string-append result)))

; (pretty-print obj port) pretty prints 'obj' on 'port'.  the current
; output port is used if 'port' is not specified.

(define (pretty-print obj . opt)
  (let ((port (if (pair? opt) (car opt) (current-output-port))))
    (generic-write obj #f 79 (lambda (s) (display s port) #t))))

; (pretty-print-to-string obj) returns a string with the pretty-printed
; textual representation of 'obj'.

(define (pretty-print-to-string obj)
  (let ((result '()))
    (generic-write obj #f 79 (lambda (str) (set! result (cons str result)) #t))
    (reverse-string-append result)))
