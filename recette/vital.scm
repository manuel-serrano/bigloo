;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/recette/vital.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec  3 17:11:11 2002                          */
;*    Last change :  Mon Oct  8 08:18:26 2018 (serrano)                */
;*    Copyright   :  2002-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Preliminary tests for Bigloo.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module vital
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-vital))
   (static  var))

;*---------------------------------------------------------------------*/
;*    var ...                                                          */
;*---------------------------------------------------------------------*/
(define var var)

;*---------------------------------------------------------------------*/
;*    bar ...                                                          */
;*---------------------------------------------------------------------*/
(define (bar . l)
   l)

(set! bar 'toto)

;*---------------------------------------------------------------------*/
;*    fooo ...                                                         */
;*---------------------------------------------------------------------*/
(define (fooo x)
   (if (integer? x)
       (begin
	  (set! x #f)
	  (integer? x))
       #t))

;*---------------------------------------------------------------------*/
;*    vital:labels ...                                                 */
;*---------------------------------------------------------------------*/
(define (vital:labels fun labels)
   (for-each (lambda (id) id) labels)
   (fun (lambda (id) id) labels))

;*---------------------------------------------------------------------*/
;*    vital:let ...                                                    */
;*---------------------------------------------------------------------*/
(define (vital:let x fun)
   (let ((let (if (> x 0) (fun (+ x 1)) (fun (- x)))))
      (if (> x 0) let (fun (- x)))))

;*---------------------------------------------------------------------*/
;*    vital:let* ...                                                   */
;*---------------------------------------------------------------------*/
(define (vital:let*)
   (let* ((x 1) (x (+ 1 x))) x))

;*---------------------------------------------------------------------*/
;*    vital:write ...                                                  */
;*---------------------------------------------------------------------*/
(define (vital:write s)
   (let ((p (open-output-string)))
      (write s p)
      (let* ((s (close-output-port p))
	     (p (open-input-string s)))
	 (let ((res (read p)))
	    (close-input-port p)
	    res))))

;*---------------------------------------------------------------------*/
;*    vital:write2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (vital:write2 s)
   (with-input-from-string (with-output-to-string
			      (lambda ()
				 (write s)))
      (lambda ()
	 (let ((res (read)))
	    res))))

;*---------------------------------------------------------------------*/
;*    vital:write3 ...                                                 */
;*---------------------------------------------------------------------*/
(define (vital:write3 s)
   (with-input-from-string (with-error-to-string
			      (lambda ()
				 (with-output-to-string
				    (lambda ()
				       (display 1 (current-error-port))
				       (write s)))))
      (lambda ()
	 (let ((res (read)))
	    res))))

;*---------------------------------------------------------------------*/
;*    vital:hoist ...                                                  */
;*---------------------------------------------------------------------*/
(define cur_ref (make-cell 0))

(define (vital:hoist)
   (let ((x1 (let ((x2 (cell-ref cur_ref)))
		(begin
		   (cell-set! cur_ref (+fx x2 1))
		   x2)))
	 (x2 (let ((x2 (cell-ref cur_ref)))
		(begin
		   (cell-set! cur_ref (+fx x2 1))
		   x2))))
      (eq? x1 x2)))

;*---------------------------------------------------------------------*/
;*    Un bug dans le soft-typing (un truc qui ne se compilait pas).    */
;*---------------------------------------------------------------------*/
(let ((revtype2
       (labels ((revtype (parent t flag)
                         (let ((rev (lambda (q)
                                       (revtype t q #f))))
                            (if flag (rev 1)))))
          revtype)))
   (revtype2 1 2 #f))

;*---------------------------------------------------------------------*/
;*    producer ...                                                     */
;*---------------------------------------------------------------------*/
(define (producer a b)
   (values (+ a 1) (+ b 1)))

;*---------------------------------------------------------------------*/
;*    producer2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (producer2 . args)
   (match-case args
      ((?a0)
       (values a0))
      ((?a0 ?a1)
       (values a0 a1))
      ((?a0 ?a1 ?a2)
       (values a0 a1 a2))
      ((?a0 ?a1 ?a2 ?a3)
       (values a0 a1 a2 a3))
      ((?a0 ?a1 ?a2 ?a3 ?a4)
       (values a0 a1 a2 a3 a4))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5)
       (values a0 a1 a2 a3 a4 a5))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6)
       (values a0 a1 a2 a3 a4 a5 a6))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7)
       (values a0 a1 a2 a3 a4 a5 a6 a7))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9 ?a10)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9 ?a10 ?a11)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9 ?a10 ?a11 ?a12)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9 ?a10 ?a11 ?a12 ?a13)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9 ?a10 ?a11 ?a12 ?a13 ?a14)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9 ?a10 ?a11 ?a12 ?a13 ?a14 ?a15)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
      ((?a0 ?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9 ?a10 ?a11 ?a12 ?a13 ?a14 ?a15 ?a16)
       (values a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16))
      (else
       (error 'producer2 "Illegal number of arguments" (length args)))))

;*---------------------------------------------------------------------*/
;*    producer3 ...                                                    */
;*---------------------------------------------------------------------*/
(define (producer3 . args)
   (apply values args))

;*---------------------------------------------------------------------*/
;*    Top level definition orders                                      */
;*---------------------------------------------------------------------*/
(define order_foo 0)
(define order_foo1 (begin (set! order_foo (+fx order_foo 1)) order_foo))
(define order_foo2 (begin (set! order_foo (+fx order_foo 1)) order_foo))
(begin
   (define order_foo3 (begin (set! order_foo (+fx order_foo 1)) order_foo))
   (define order_foo4 (begin (set! order_foo (+fx order_foo 1)) order_foo)))
(define order_foo5 (begin (set! order_foo (+fx order_foo 1)) order_foo))

(define order_bar 0)
(define order_bar1 1)
(define order_bar2 1)
(define order_bar3 1)
(define order_bar4 1)
(define order_bar5 1)
(define order_bar6 1)
(define order_bar7 1)
(define order_bar8 1)

(begin
   (begin
      (begin
	 (set! order_bar (+fx order_bar 1))
	 (set! order_bar1 order_bar)
	 (set! order_bar (+fx order_bar 1))
	 (set! order_bar2 order_bar))
      (begin
	 (set! order_bar (+fx order_bar 1))
	 (set! order_bar3 order_bar)
	 (set! order_bar (+fx order_bar 1))
	 (set! order_bar4 order_bar))
      (set! order_bar (+fx order_bar 1))
      (set! order_bar5 order_bar)
      (set! order_bar (+fx order_bar 1))
      (set! order_bar6 order_bar))
   (set! order_bar (+fx order_bar 1))
   (set! order_bar7 order_bar)
   (set! order_bar (+fx order_bar 1))
   (set! order_bar8 order_bar))

;*---------------------------------------------------------------------*/
;*    point                                                            */
;*---------------------------------------------------------------------*/
(define-struct point x y)

;*---------------------------------------------------------------------*/
;*    inline/class/generic                                             */
;*---------------------------------------------------------------------*/
(define-inline (inline x) x)
(define-inline (class x) x)
(define-inline (generic x) x)

;*---------------------------------------------------------------------*/
;*    test-args-parse ...                                              */
;*---------------------------------------------------------------------*/
(define (test-args-parse)
   (let ((res 0))
      (define (do-args-parse args)
	 (args-parse args
	    (("-Obench" (help "Benchmarking mode"))
	     (set! res (bit-or res 1))
	     (do-args-parse `("-O5" "-unsafe"
				    "-copt" "-O4"
				    "-static-bigloo"))) 
	    (("-O?opt" (help "-O"))
	     (set! res (bit-or res 2)))
	    (("-unsafe?opt" (help "unsafe"))
	     (set! res (bit-or res 4)))
	    (("-copt" ?opt (help "copt"))
	     (set! res (bit-or res 8)))
	    (("-static-bigloo" (help "foo"))
	     (set! res (bit-or res 16)))
	    ((("-h" "--help") (help "An help message"))
	     (set! res (bit-or res 32)))
	    ((("-v" "--version") ?version (help "The version"))
	     (set! res (bit-or res (string->number version))))))
      (do-args-parse '("-Obench" "-h" "--version" "64"))
      res))

(define (for-each-inline)
   (let ((v 0))
      (for-each (let ((j 0))
		   (lambda (x)
		      (set! v (+ j v))
		      (set! j (+ j 1))))
		'(a b c))
      v))

;*---------------------------------------------------------------------*/
;*    test-type-data-flow ...                                          */
;*---------------------------------------------------------------------*/
(define (test-type-data-flow x::int)
   (if (pair? x)
       (car x)
       #t))

;*---------------------------------------------------------------------*/
;*    test-type-data-flow2 ...                                         */
;*---------------------------------------------------------------------*/
(define (test-type-data-flow2 l::pair-nil)
   (if (> (length l) 10)
       (print 1 2 3 4 5 6 7 8 ))
   (if (pair? l)
       (car l)
       l))

;*---------------------------------------------------------------------*/
;*    test-type-data-flow3 ...                                         */
;*---------------------------------------------------------------------*/
(define (test-type-data-flow3 l::pair-nil)
   (define (for-each-rest pred l)
      (cond ((pair? l)
	     (let loop ((x (car l))
			(l1 (cdr l)))
		(pred x l1)
		(cond ((pair? l1)
		       (loop (car l1) (cdr l1))))))))
   (set! l (filter! pair? l))
   (for-each-rest (lambda (l1 rest)
		     (cond ((pair? rest)
			    (append! l1 (car rest)))))
		  l)
   (if (pair? l)
       (car l)
       l))

;*---------------------------------------------------------------------*/
;*    test-type-data-flow4 ...                                         */
;*---------------------------------------------------------------------*/
(define (test-type-data-flow4 l::pair-nil)
   (cond
      ((pair? l) 1)
      ((null? l) 2)
      (else (print "this" "branch" "cannot" "be" "selected"))))

;*---------------------------------------------------------------------*/
;*    cond-expand-foo ...                                              */
;*---------------------------------------------------------------------*/
(cond-expand
   ((or glop pas-glop bigloo)
    (define (cond-expand-foo x) x))
   (else
    (define (cond-expand-foo x) 4)))
 
;*---------------------------------------------------------------------*/
;*    bug-jvm ...                                                      */
;*---------------------------------------------------------------------*/
(define (bug-jvm scm-files)
   ;; This function was badly compiled up to bigloo3.5b. The JAS code
   ;; is incorrect and the bytecode verifier breaks. This function
   ;; cannot be executed hence, it is used in this file only as a
   ;; procedural value
   (begin
      (labels ((try-120 (g-119) (try-120 8)))
	 (try-120 9))
      6))

;*---------------------------------------------------------------------*/
;*    bug-dataflow-lub ...                                             */
;*---------------------------------------------------------------------*/
(define (bug-dataflow-lub x)
   (when (string? x)
      (cond
	 ((string-prefix? "/" x)
	  (set! x #f)))
      (cond
	 (x 111)
	 (else 222))))

;*---------------------------------------------------------------------*/
;*    test-vital ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-vital)
   (test-module "vital" "vital.scm")
   (test "true" #t (not #f))
   (test "true" #t (not (not 4)))
   (test "true" (not (eq? #t #f)) (not #f))
   (test "false" #f (not #t))
   (test "begin" (begin 4 5) 5)
   (test "eq?" 1 1)
   (test "< 0" -1 -1)
   (test "-" (- 2) -2)
   (test "*" (* 2 -1) -2)
   (test "eq? integer.1" (eq? 1 2) #f)
   (test "eq? integer.2" (eq? 1 1) #t)
   (test "eq? integer.3" (eq? '1 1) #t)
   (test "eq? integer.4" (eq? '1 '1) #t)
   (test "eq? integer.5" (eq? '1 '2) #f)
   (test "eq? char.1" (eq? #\a #\a) #t)
   (test "eq? char.2" (eq? #\| #\|) #t)
   (test "eq? char.3" (eq? #\a #\A) #F)
   (test "eq? char.4" (char->integer #\') 39)
   (test "eq? char.5" (integer->char 255) #a255)
   (test "eq? char.6" (char=? (integer->char 255) #a255) #t)
   (test "eq? char.7" (=fx (char->integer #\xea) #xea) #t)
   (test "eq? symbol.1" (eq? 'a 'a) #t)
   (test "eq? symbol.2" (eq? 'a 'A) #f)
   (test "eq? symbol.3" (eq? 'A 'A) #t)
   (test "eq? symbol.4" (eq? 'a 'b) #f)
   (test "eq? symbol.5" (eq? ':=   '|:=|) #f)
   (test "symbol.1" '|foo\|bar| (string->symbol "foo\|bar"))
   (test "symbol.2"
      (string=? (call-with-output-string (lambda (p) (write '|foo\|bar| p)))
	 "foo\|bar")
      #t)
   (test "eq? integer.1" (eq? 1 2) #f)
   (test "eq? integer.2" (eq? 1 1) #t)
   (test "eq? integer.3" (eq? '1 1) #t)
   (test "eq? integer.4" (eq? '1 '1) #t)
   (test "eq? integer.5" (eq? '1 '2) #f)
   (test "eqv? char.1" (eqv? #\a #\a) #t)
   (test "eqv? char.2" (eqv? #\| #\|) #t)
   (test "eqv? char.3" (eqv? #\a #\A) #F)
   (test "eqv? char.4" (eqv? (char->integer #\') 39) #t)
   (test "eqv? char.5" (eqv? (integer->char 255) #a255) #t)
   (test "eqv? char.6" (char=? (integer->char 255) #a255) #t)
   (test "eqv? symbol.1" (eqv? 'a 'a) #t)
   (test "eqv? symbol.2" (eqv? 'a 'A) #f)
   (test "eqv? symbol.3" (eqv? 'A 'A) #t)
   (test "eqv? symbol.4" (eqv? 'a 'b) #f)
   (test "equal.1?" (equal? (cons 1 2) (cons 1 2)) #t)
   (test "equal.2?" (equal? (cons 2 2) (cons 1 2)) #f)
   (test "equal.3?" (equal? (list #s8:1 #u8:2 #s16:-1 #u16:45 #s32:6 #u32:2 #s64:-56 #u64:1234) (list 1 2 -1 45 6 #l2 #l-56 #l1234)) #t)
   (test "define" bar 'toto)
   (test "application" ((lambda args 2)) 2)
   (test "application" ((lambda args 2) 1 2 3 4) 2)
   (test "set!" (fooo 5) #f)
   (test "set!" (let ((v (integer? var))) (set! var 4) (set! var 'toto) v) #f)
   (test "if.1" (if #t #t #f) #t)
   (test "if.2" (let ((if (lambda (x y z) z))) (if #t #t #f)) #f)
   (test "symbol.1" 'étè (string->symbol "étè"))
   (test "symbol.2" (symbol? (with-input-from-string "||" read)) #t)
   (test "symbol.3" (with-input-from-string "||" read) (string->symbol ""))
   (test "symbol.4" (symbol? (with-input-from-string "|'|" read)) #t)
   (test "symbol.5" (with-input-from-string "|'|" read) (string->symbol "'"))
   (test "symbol.5b" (with-input-from-string "|{|" read) (string->symbol "{"))
   (test "symbol.5c" (with-input-from-string "|(|" read) (string->symbol "("))
   (test "symbol.5d" (with-input-from-string "|()|" read) (string->symbol "()"))
   (test "symbol.6" (symbol? (with-input-from-string "|foo bar|" read)) #t)
   (test "symbol.7" (with-input-from-string "|foo bar|" read)
	 (string->symbol "foo bar"))
   (test "symbol.8" (string->symbol "new Function( \"return hop_service( \\\"/hop/mailto\\\", arguments )\" )")
	 (string->symbol (string-append "new Function( \"return hop_service( \\\"/hop/mailto\\\", arguments )\" )" "")))
   (test "symbol.9" (symbol? (string->symbol "01234")) #t)
   (test "symbol.10" (symbol? '|;|) #t)
   (test "symbol.11" (symbol? (string->symbol ";")) #t)
   (let ((s1 (symbol->string '|:=|))
	 (s2 (symbol->string '|:=|)))
      (test "symbol.12" (string=? s1 s2) #t))
   (let ((s1 (with-output-to-string (lambda () (write ':=))))
	 (s2 (with-output-to-string (lambda () (write '|:=|)))))
      (test "symbol.13" (string=? s1 s2) #f))
   (let ((s1 (with-output-to-string (lambda () (write '|(|))))
	 (s2 (with-output-to-string (lambda () (write (string->symbol "("))))))
      (test "symbol.14" (string=? s1 s2) #t))
   (let ((s (with-output-to-string (lambda () (write '(|;| k))))))
      (with-input-from-string s
	 (lambda ()
	    (let ((v (read)))
	       (test "symbol.15"
		     (and (pair? v)
			  (pair? (cdr v))
			  (null? (cddr v))
			  (equal? v (list (string->symbol ";") 'k)))
		     #t)))))
   (test "symbol.16" (symbol? '|(ab)|) #t)
   (test "symbol.17" '|(a,b)| (string->symbol "(a,b)"))
   (test "symbol.18" (symbol? '|:=|) #t)
   (test "symbol.19" '|.| (string->symbol "."))
   (let ((s1 (with-output-to-string (lambda () (write '|.|))))
	 (s2 (with-output-to-string (lambda () (write (string->symbol "."))))))
      (test "symbol.20" (string=? s1 s2) #t))
   (test "symbol.21" (symbol->string '.foo) ".foo")
   (test "symbol.22" '.foo (string->symbol ".foo"))
   (test "symbol.23" (string->symbol "`") '|`|)
   (test "gensym.1" (symbol? (gensym)) #t)
   (test "gensym.2" (symbol? (gensym 'foo)) #t)
   (test "gensym.3" (symbol? (gensym "foo")) #t)
   (test "gensym.4" (eq? (gensym) (gensym)) #f)
   (test "gensym.5" (eq? (gensym 'foo) (gensym 'foo)) #f)n
   (let ((s (gensym)))
      (test "gensym.6" (eq? s (string->symbol (symbol->string s))) #t))
   (let ((s "toto\"\\\ntiti"))
      (test "write.1" (vital:write s) s))
   (let ((s "toto\"\\\ntiti"))
      (test "write.2" (vital:write2 s) s))
   (let ((s #"toto\"\\\nti\tti"))
      (test "write.3" (vital:write2 s) s))
   (test "write.3" (vital:write3 "foobar") 1)
   (test "write.4" (vital:write (string->symbol "01234")) '|01234|)
   (test "flonum" (real? (string->obj (obj->string 0.5))) #t)
   (let ((o '(#s8:0 #s8:1 #s8:-1 #u8:0 #u8:1 #u8:250
	      #s16:0 #s16:1 #s16:-1 #u16:0 #u16:1 #u16:250
	      #s16:15000 #s16:-15000 #u16:30000
	      #s32:0 #s32:1 #s32:-1 #u32:0 #u32:1 #u32:250
	      #s32:15000 #s32:-15000 #u32:30000
	      #s32:70000 #s32:-70000 #u32:70000
	      #s64:0 #s64:1 #s64:-1 #u64:0 #u64:1 #u64:250
	      #s64:15000 #s64:-15000 #u64:30000
	      #s64:70000 #s64:-70000 #u64:70000)))
      (test "stding" (string->obj (obj->string o)) o))
   (let* ((append! (lambda (x y)
		      (if (null? x)
			  y
			  (do ((a x b)
			       (b (cdr x) (cdr b)))
			      ((null? b)
			       (set-cdr! a y)
			       x)))))
	  (l1      '(1 2 3))
	  (l2      '(4 5 6)))
      (test "do" (append! l1 l2) '(1 2 3 4 5 6)))
   (test "let" (let ((x (begin 1 2 3))) x) 3)
   (test "hoist" (vital:hoist) #f)
   (let* ((s (make-string 3 #\a))
	  (l (list s s)))
      (test "string->obj" (string->obj (obj->string l)) l))
   (let* ((s    "toto n'est pas content")
	  (sp   "titi non plus")
          (ptr  (make-weakptr sp))
	  (l    (list s s s sp sp s sp sp 'toto 'toto '#(1 2 3) #\a #\a #t #f
                      ptr ptr))
	  (v    `#(,l ,l ,l () ,(unspecified) 1.13 1.13 #a123
		      (point 1 2) (point 2 1) 0010 0011))
	  (vec  (make-vector 3 v))
	  (rep  (obj->string v)))
      (test "intext" (string->obj rep) v))
   (let* ((c    (cons 2 3))
          (ptr  (make-weakptr c)))
    (set-car! c ptr)
    (let ((ptr2 (string->obj (obj->string ptr))))
     (test "intext weakptr"
           (and (weakptr? ptr2)
                (let ((d (weakptr-data ptr2)))
                 (and (pair? d)
                      (eq? (car d) ptr2)
                      (eq? (cdr d) 3))))
           #t)))
   (test "top level forms" (list order_foo1
				 order_foo2
				 order_foo3
				 order_foo4
				 order_foo5)
	 '(1 2 3 4 5))
   (test "top level forms" (list order_bar1
				 order_bar2
				 order_bar3
				 order_bar4
				 order_bar5
				 order_bar6
				 order_bar7
				 order_bar8)
	 '(1 2 3 4 5 6 7 8))
   (test "multive-value-bind" (multiple-value-bind (a b c d)
				 (values 1 2 3 4)
				 (- a b c d))
	 -8)
   (test "multiple-value-bind"
	 (multiple-value-bind (x y) (producer 1 2) (+ x y))
	 5)
   (test "multiple-value-bind.0"
	 (multiple-value-bind (a0)
	    (producer2 0)
	    (list a0))
	 '(0))
   (test "multiple-value-bind.1"
	 (multiple-value-bind (a0 a1)
	    (producer2 0 1)
	    (list a0 a1))
	 '(0 1))
   (test "multiple-value-bind.2"
	 (multiple-value-bind (a0 a1 a2)
	    (producer2 0 1 2)
	    (list a0 a1 a2))
	 '(0 1 2))
   (test "multiple-value-bind.3"
	 (multiple-value-bind (a0 a1 a2 a3)
	    (producer2 0 1 2 3)
	    (list a0 a1 a2 a3))
	 '(0 1 2 3))
   (test "multiple-value-bind.4"
	 (multiple-value-bind (a0 a1 a2 a3 a4)
	    (producer2 0 1 2 3 4)
	    (list a0 a1 a2 a3 a4))
	 '(0 1 2 3 4))
   (test "multiple-value-bind.5"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5)
	    (producer2 0 1 2 3 4 5)
	    (list a0 a1 a2 a3 a4 a5))
	 '(0 1 2 3 4 5))
   (test "multiple-value-bind.6"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6)
	    (producer2 0 1 2 3 4 5 6)
	    (list a0 a1 a2 a3 a4 a5 a6))
	 '(0 1 2 3 4 5 6))
   (test "multiple-value-bind.7"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7)
	    (producer2 0 1 2 3 4 5 6 7)
	    (list a0 a1 a2 a3 a4 a5 a6 a7))
	 '(0 1 2 3 4 5 6 7))
   (test "multiple-value-bind.8"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8)
	    (producer2 0 1 2 3 4 5 6 7 8)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8))
	 '(0 1 2 3 4 5 6 7 8))
   (test "multiple-value-bind.9"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	    (producer2 0 1 2 3 4 5 6 7 8 9)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))
	 '(0 1 2 3 4 5 6 7 8 9))
   (test "multiple-value-bind.9d"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
	    (producer2 0 1 2 3 4 5 6 7 8 9 10)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
	 '(0 1 2 3 4 5 6 7 8 9 10))
   (test "multiple-value-bind.9e"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
	    (producer2 0 1 2 3 4 5 6 7 8 9 10 11)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
	 '(0 1 2 3 4 5 6 7 8 9 10 11))
   (test "multiple-value-bind.9f"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
	    (producer2 0 1 2 3 4 5 6 7 8 9 10 11 12)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12))
   (test "multiple-value-bind.9g"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)
	    (producer2 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13))
   (test "multiple-value-bind.9h"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
	    (producer2 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
   (test "multiple-value-bind.9i"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
	    (producer2 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
   (test "multiple-value-bind.9j"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
	    (producer2 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
   (test "multiple-value-bind.0b"
	 (multiple-value-bind (a0)
	    (producer3 0)
	    (list a0))
	 '(0))
   (test "multiple-value-bind.1b"
	 (multiple-value-bind (a0 a1)
	    (producer3 0 1)
	    (list a0 a1))
	 '(0 1))
   (test "multiple-value-bind.2b"
	 (multiple-value-bind (a0 a1 a2)
	    (producer3 0 1 2)
	    (list a0 a1 a2))
	 '(0 1 2))
   (test "multiple-value-bind.3b"
	 (multiple-value-bind (a0 a1 a2 a3)
	    (producer3 0 1 2 3)
	    (list a0 a1 a2 a3))
	 '(0 1 2 3))
   (test "multiple-value-bind.4b"
	 (multiple-value-bind (a0 a1 a2 a3 a4)
	    (producer3 0 1 2 3 4)
	    (list a0 a1 a2 a3 a4))
	 '(0 1 2 3 4))
   (test "multiple-value-bind.5b"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5)
	    (producer3 0 1 2 3 4 5)
	    (list a0 a1 a2 a3 a4 a5))
	 '(0 1 2 3 4 5))
   (test "multiple-value-bind.6b"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6)
	    (producer3 0 1 2 3 4 5 6)
	    (list a0 a1 a2 a3 a4 a5 a6))
	 '(0 1 2 3 4 5 6))
   (test "multiple-value-bind.7b"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7)
	    (producer3 0 1 2 3 4 5 6 7)
	    (list a0 a1 a2 a3 a4 a5 a6 a7))
	 '(0 1 2 3 4 5 6 7))
   (test "multiple-value-bind.8b"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8)
	    (producer3 0 1 2 3 4 5 6 7 8)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8))
	 '(0 1 2 3 4 5 6 7 8))
   (test "multiple-value-bind.9k"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
	    (producer3 0 1 2 3 4 5 6 7 8 9)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))
	 '(0 1 2 3 4 5 6 7 8 9))
   (test "multiple-value-bind.9l"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
	    (producer3 0 1 2 3 4 5 6 7 8 9 10)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
	 '(0 1 2 3 4 5 6 7 8 9 10))
   (test "multiple-value-bind.9m"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
	    (producer3 0 1 2 3 4 5 6 7 8 9 10 11)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
	 '(0 1 2 3 4 5 6 7 8 9 10 11))
   (test "multiple-value-bind.9n"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
	    (producer3 0 1 2 3 4 5 6 7 8 9 10 11 12)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12))
   (test "multiple-value-bind.9o"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)
	    (producer3 0 1 2 3 4 5 6 7 8 9 10 11 12 13)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13))
   (test "multiple-value-bind.9p"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
	    (producer3 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
   (test "multiple-value-bind.9q"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
	    (producer3 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
   (test "multiple-value-bind.9r"
	 (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
	    (producer3 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
	    (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16))
	 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
   (test "call-with-values" (call-with-values (lambda () (values 1 2 3 4))
					      (lambda (a b c d) (+ a b c d)))
	 10)
   (test "call-with-values" (call-with-values * -) -1)
   (cond-expand
      (bigloo-c
       (let ((b (let ((ptr-align (pragma::long "PTR_ALIGNMENT")))
		   (-fx (bit-lsh 1 (+fx ptr-align 3)) ptr-align))))
	  (test "<< >>"  (let ((n (bit-lsh 1 b))
			       (n-1 (bit-lsh 1 (-fx b 1)))
			       (n-2 (bit-lsh 1 (-fx b 2))))
			    (and (>fx n n-1) (>fx n-1 n-2)))
		#t)))
      (else #unspecified))
   (test "cond-expand" (cond-expand
			  (srfi-0 (* 1 2))
			  (else (+ 3 4)))
	 2)
   (test "cond-expand" (eval '(cond-expand
				 (srfi-0 (* 1 2))
				 (else (+ 3 4))))
	 2)
   (test "cond-expand" (cond-expand
			  (srfi-that-does-not-exists (* 1 2))
			  (else (+ 3 4)))
	 7)
   (test "cond-expand(fail)" (eval '(cond-expand
				       (srfi-that-does-not-exists (* 1 2))
				       (else (+ 3 4))))
	 7)
   (test "cond-expand(and)" (cond-expand
			       ((and bigloo srfi-0) 8)
			       (else -1))
	 8)
   (test "cond-expand(and)" (cond-expand
			       ((and bigloo srfi-0 (not bigloo-eval)) 9)
			       (else -1))
	 9)
   (test "cond-expand(and eval)" (eval '(cond-expand
					   ((and bigloo bigloo-eval srfi-0) 4)
					   (else -1)))
	 4)
   (test "cond-expand(or)" (cond-expand
			      ((or bigloo-eval bigloo) 2)
			      (else -1))
	 2)
   (test "cond-expand(or)" (cond-expand
			      ((or bigloo-eval bigloo) 2)
			      (else -1))
	 2)
   (test "cond-expand(or)" (let ((v 0))
			      (cond-expand
				 ((or bigloo1.1 bigloo1.2) (set! v (+ 1 v)))
				 (else (set! v (+ 1 v))))
			      v)
	 1)
   (test "cond-expand(or)" (eval '(let ((v 0))
				     (cond-expand
					((or bigloo1.1 bigloo1.2) (set! v (+ 1 v)))
					(else (set! v (+ 1 v))))
				     v))
	 1)
   (test "cond-expand(or-eval)" (eval '(cond-expand
					  ((or bigloo-eval bigloo) 5)
					  (else -1)))
	 5)
   (test "cond-expand(or)" (cond-expand
			      ((or bigloo bigloo-eval) 2)
			      (else -1))
	 2)
   (test "cond-expand(or-eval)" (eval '(cond-expand
					  ((or bigloo bigloo-eval) 5)
					  (else -1)))
	 5)
   (test "cond-expand(or)" (cond-expand-foo 3) 3)
   (test "cond-expand" (cond-expand ((and bigloo bigloo) 1) (else 2)) 1)
   (test "cond-expand" (cond-expand (xxx 1) (else 2)) 2)
   (test "cond-expand" (cond-expand ((and bigloo xxx) 1) (else 2)) 2)
   (test "cond-expand" (cond-expand ((and xxx bigloo) 1) (else 2)) 2)
   (test "manling" (bigloo-demangle (bigloo-mangle "toto")) "toto")
   (test "manling" (bigloo-demangle (bigloo-mangle "toto!")) "toto!")
   (let ((s "toto!&^%_23_2342234___"))
      (test "manling" (bigloo-demangle (bigloo-mangle s)) s))
   (test "mangling" (bigloo-mangled? "BgLtoto") #f)
   (test "mangling" (bigloo-mangled? (bigloo-mangle "BgL_toto")) #t)
   (let* ((obj (list 1 2 3 4))
	  (v   (vector 2 3 obj 5)))
      (set-car! obj v)
      (set-car! (cdr obj) obj)
      (test "cycles.1" (let ((port (open-output-string)))
			(write-circle obj port)
			(let* ((str (close-output-port port))
			       (port (open-input-string str)))
			   (let ((new-obj (read port)))
			      (close-input-port port)
			      (let ((port (open-output-string)))
				 (write-circle new-obj port)
				 (close-output-port port)))))
	    (let ((port (open-output-string)))
	       (write-circle obj port)
	       (close-output-port port))))
   (let ((s "#0=(#0# #1=(#1#))"))
      (test "cycles.2"
	 (call-with-output-string
	    (lambda (op)
	       (write-circle (call-with-input-string s read) op)))
	 s))
   (let ((s "#0=(#0# #1=(#1# #2=(#2#)) #2#)"))
      (test "cycles.3"
	 (call-with-output-string
	    (lambda (op)
	       (write-circle (call-with-input-string s read) op)))
	 s))
   (test "args-parse" (test-args-parse) (bit-or 31 (bit-or 32 64)))
   (cond-expand
      (bigloo-.net
       #t)
      (else
       (test "process" (do ((i 0 (+ i 1)))
			   ((>= i 5)
			    (let loop ((proc (process-list)))
			       (if (pair? proc)
				   (begin
				      (process-wait (car proc))
				      (loop (process-list)))
				   #t)))
			   (run-process *bigloo-path* "-version"
					output: null:))
	     #t)))
   (let ((f (make-file-name "misc" "FILE-EXISTS")))
      (test "file-exists?" (let ((v1 (file-exists? f)))
			      (cons v1
				    (let ((p (open-output-file f)))
				       (close-output-port p)
				       (file-exists? f))))
	    (cons #f #t))
      (test "delete-file" (begin
			     (delete-file f)
			     (file-exists? f))
	    #f))
   (let ((f (make-file-name "misc" "FILE-EXISTS.bin")))
      (test "file-exists?.2" (let ((v1 (file-exists? f)))
			      (cons v1
				    (let ((p (open-output-binary-file f)))
				       (close-binary-port p)
				       (file-exists? f))))
	    (cons #f #t))
      (test "delete-file.2" (begin
			     (delete-file f)
			     (file-exists? f))
	    #f)
      (test "delete-file.3" (delete-file f)
	    #f))
   (let* ((f (make-file-name "misc" "FILE-EXISTS.bin"))
	  (p (open-output-binary-file f)))
      (close-binary-port p)
      (test "delete-file.4" (delete-file f)
	    #t))
   (test "make-directory.1" (begin
			       (cond
				  ((and (file-exists? "dummydir")
				        (directory? "dummydir"))
				   (delete-directory "dummydir"))
				  ((file-exists? "dummydir")
				   (delete-file "dummydir")))
			       (make-directory "dummydir"))
	 #t)
   (test "make-directory.2" (make-directory "dummydir") #f)
   (test "directory?.1" (and (file-exists? "dummydir")
			     (directory? "dummydir"))
	 #t)
   (test "directory?.2" (or (file-exists?
			     (make-file-path "dummydir" "doesnotexist"))
	 		    (directory?
			     (make-file-path "dummydir" "doesnotexist")))
	 #f)
   (test "make-directory.3" (make-directory (make-file-path "dummydir" "foo"))
	 #t)
   (test "delete-directory.1" (begin
				 (delete-directory "dummydir")
				 (or (file-exists? "dummydir")
				     (directory? "dummydir")))
	 #t)
   (test "delete-directory.2" (begin
				 (delete-directory
				  (make-file-path "dummydir" "foo"))
				 (delete-directory "dummydir")
				 (or (file-exists? "dummydir")
				     (directory? "dummydir")))
	 #f)
   (test "delete-directory.3" (delete-directory (make-file-path "dummydir" "foo"))
	 #f)
   (let ((path "dummydir"))
      (make-directory path)
      (test "delete-directory.4" (delete-directory path)
	    #t))
   (test "for-each" (for-each-inline) 3)
   (test "do" (do ((i 1 2) (j 0 i)) ((positive? j) j)) 1)
   (test "type" (test-type-data-flow 5) #t)
   (test "type2.a" (test-type-data-flow2 '()) '())
   (test "type2.b" (test-type-data-flow2 '(1)) 1)
   (test "type3" (test-type-data-flow3 '((1 2))) '(1 2))
   (test "type4" (test-type-data-flow4 '(1 2)) 1)
   (test "type4.b" (test-type-data-flow4 '()) 2)
   (test "char.1" (char->integer #\010) 8)
   (test "char.2" (char->integer #\null) 0)
   (test "char.2" (char->integer #a008) 8)
   (test "labels" (vital:labels map '(1 2 3)) '(1 2 3))
   (test "let" (vital:let 10 +) 11)
   (test "let*" (vital:let*) 2)
   (test "bug-jvm" (procedure? bug-jvm) #t)
   (test "dataflow-lub.1" (bug-dataflow-lub "foo") 111)
   (test "dataflow-lub.2" (bug-dataflow-lub #f) #f)
   (test "dataflow-lub.3" (bug-dataflow-lub "/foo") 222)
   (test "cnst.1" (int8? #!key) #f)
   (test "cnst.2" (int8? (car (list '#!key))) #f)
   (test "cnst.3" (int8? #!optional) #f)
   (test "cnst.4" (int8? (car (list '#!optional))) #f)
   (test "cnst.5" (int8? #!rest) #f)
   (test "cnst.6" (int8? (car (list '#!rest))) #f)
   (test "cnst.7" (int8? #s8:1) #t)
   (test "cnst.8" (int8? (car (list #s8:1))) #t)
   (test "cnst.9" (eq? (car (list #!optional)) #!optional) #t))
     
