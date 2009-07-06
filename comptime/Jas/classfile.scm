(module jas_classfile
   (import jas_lib)
   (export
    (class type
       code::bstring
       (vect (default #f)) )

    (class basic::type)

    (class vect::type type::type)

    (class fun::type tret::type targs)
    
    (class classe::type
       flags                 ;; int
       name                  ;; qualified name "java.lang.Object"
       (pool (default #f)) ) ;; pool index to a class info

    (class field-or-method
       flags                     ;; int
       name::bstring             ;; name of the field/method
       owner                     ;; user classe name
       usertype                  ;; symbolic type 
       (type (default #f))       ;; type::type 
       (pname (default #f))      ;; pool index to a UTF8 unqualified name
       (descriptor (default #f)) ;; pool index to a UTF8 encoded type
       (pool (default #f))       ;; pool index to a field/method info
       (attributes (default '())) )

    (class field::field-or-method)

    (class method::field-or-method)

    (class attribute
       type
       name
       size
       info )

    (class classfile
       (current-method (default 'as))
       (globals (default '()))
       (pool (default '()))
       (pool-size (default 1))
       (pooled-names (default (make-hashtable)))
       (flags (default #f))
       (me (default #f))
       (super (default #f))
       (interfaces (default '()))
       (fields (default '()))
       (methods (default '()))
       (attributes (default '())) )

    (jas-error   ::classfile ::bstring ::obj)
    (jas-warning ::classfile ::bstring ::obj)

    (as-type::type   ::classfile ::obj)
    (as-funtype::fun ::classfile ::obj ::obj)
    (type-size::int  ::type)

    (as-assign                  ::classfile ::symbol ::obj)
    (declared-class::classe     ::classfile ::symbol)
    (declared-field::field      ::classfile ::symbol)
    (declared-method::method    ::classfile ::symbol)
    (pool-name::int             ::classfile ::bstring)
    (pool-int::int              ::classfile ::int)
    (pool-elong::int            ::classfile ::elong)
    (pool-float::int            ::classfile ::float)
    (pool-long::int             ::classfile ::long)
    (pool-llong::int            ::classfile ::llong)
    (pool-double::int           ::classfile ::double)
    (pool-class::int            ::classfile ::classe)
    (pool-class-by-name::int    ::classfile ::symbol)
    (pool-class-by-reftype::int ::classfile ::type)
    (pool-string::int           ::classfile ::bstring)
    (pool-field::int            ::classfile ::field)
    (pool-method::int           ::classfile ::method)
    (pool-interface-method::int ::classfile ::method)
    (pool-local-method classfile::classfile method::method)))

;;
;; Errors
;;
(define (jas-error classfile::classfile msg::bstring arg::obj)
   (error (classfile-current-method classfile) msg arg) )

(define (jas-warning classfile::classfile msg::bstring arg::obj)
   (print " WARNING ** : " (classfile-current-method classfile) " "
	  " " msg " "  arg)
   '(jas-error classfile msg arg) )

;;
;;  Types
;;
(define basic-encoded-type
   `((void    . ,(instantiate::basic (code "V")))
     (boolean . ,(instantiate::basic (code "Z")))
     (char    . ,(instantiate::basic (code "C")))
     (byte    . ,(instantiate::basic (code "B")))
     (short   . ,(instantiate::basic (code "S")))
     (int     . ,(instantiate::basic (code "I")))
     (long    . ,(instantiate::basic (code "J")))
     (float   . ,(instantiate::basic (code "F")))
     (double  . ,(instantiate::basic (code "D"))) ))

(define pourquoi_tant_de_haine (cdr (assq 'byte basic-encoded-type)))

(type-vect-set! pourquoi_tant_de_haine
		(instantiate::vect
		       (code "[B")
		       (type pourquoi_tant_de_haine) ))

(define (as-type classfile typedecl)
   (define (declared-class? name)
      (let ( (value (declared? classfile name)) )
	 (if (classe? value)
	     value
	     #f )))
   (match-case typedecl
      ((vector ?elt-typedecl)
       (let ( (elt-type (as-type classfile elt-typedecl)) )
	  (get-vect-type elt-type) ))
      ((function ?tret . ?targs)
       (as-funtype classfile tret targs) )
      (else
       (cond
	  ((assq typedecl basic-encoded-type) => cdr)
	  ((declared-class? typedecl) => (lambda (x) x))
	  (else (jas-error classfile "bad type" typedecl)) ))))

(define (get-vect-type type)
   (with-access::type type (vect code)
      (if vect
	  vect
	  (let ( (r (instantiate::vect
		       (code (string-append "[" code))
		       (type type) )))
	     (set! vect r)
	     r ))))

(define (as-funtype classfile tret targs)
   (let ( (tret (as-type classfile tret))
	  (targs (map (lambda (t) (as-type classfile t)) targs)) )
      (instantiate::fun
	 (code (string-append "("
			      (apply string-append (map type-code targs))
			      ")" (type-code tret)))
	 (tret tret)
	 (targs targs) )))


(define (type-size type)
   (let ( (code (type-code type)) )
      (let ( (n (string-length code)) )
	 (if (=fx n 1)
	     (case (string-ref code 0)
		((#\V) 0)
		((#\J #\D) 2)
		(else 1) )
	     1 ))))

;;
;; Functions managing user global names
;;
(define (as-assign classfile name value)
   (if (getprop name 'jas-global-value)
       (jas-error classfile "redefinition of global" name)
       (with-access::classfile classfile (globals)
	  (putprop! name 'jas-global-value value)
	  (set! globals (cons name globals)) )))

(define (declared? classfile name)
;   (with-access::classfile classfile (globals)
;      (let ( (slot (assq name globals)) )
;	 (if slot (cdr slot) #f) ))
   (getprop name 'jas-global-value) )

(define (declared classfile name)
   (or (declared? classfile name)
       (jas-error classfile "undefined global name" name) ))

(define (declared-class classfile name)
   (let ( (value (declared classfile name)) )
      (if (classe? value)
	  value
	  (jas-error classfile "not a class" name) )))

(define (declared-field classfile name)
   (let ( (value (declared classfile name)) )
      (if (field? value)
	  value
	  (jas-error classfile "not a field" name) )))

(define (declared-method classfile name)
   (let ( (value (declared classfile name)) )
      (if (method? value)
	  value
	  (jas-error classfile "not a method" name) )))

;;
;; Pool functions
;;
(define (pool-size-item tag)
   (if (memq tag '(5 6))
       2
       1 ))

(define (pool-add classfile item)
   (with-access::classfile classfile (pool pool-size)
      (let ( (r pool-size) )
	 (set! pool (cons item pool))
	 (set! pool-size (+fx pool-size (pool-size-item (car item))))
	 r )))

(define (pool-get classfile tag val)
   (with-access::classfile classfile (pool pool-size)
      (define (search l n)
	 (if (null? l)
	     0
	     (let ( (xtag (caar l)) (xval (cdar l)) )
		(let ( (here (-fx n (pool-size-item xtag))) )
		   (if (and (eq? xtag tag) (equal? xval val))
		       here
		       (search (cdr l) here) )))))
      (search pool pool-size) ))

(define (pool-get-special! classfile tag val)
   (with-access::classfile classfile (pooled-names)
      (let ( (n (hashtable-get pooled-names val)) )
	 (if n
	     n
	     (with-access::classfile classfile (pool pool-size)
		(let ( (r pool-size) )
		   (set! pool (cons (cons 1 val) pool))
		   (set! pool-size (+fx pool-size 1))
		   (hashtable-put! pooled-names val r)
		   r ))))))

(define (pool-get! classfile tag val)
   (let ( (r (pool-get classfile tag val)) )
      (if (= r 0)
	  (pool-add classfile (cons tag val))
	  r )))

;; All kind of pool item
(define (pool-name classfile name)
   (pool-get-special! classfile 1 name) )

(define (pool-int classfile n)
   (pool-get! classfile 3 (w2 n)) )

(define (pool-elong classfile n)
   (pool-get! classfile 5 (w4elong n)) )

(define (pool-float classfile n)
   (pool-get! classfile 4 (f2 n)) )

(define (pool-long classfile n)
   (pool-get! classfile 5 (w4 n)) )

(define (pool-llong classfile n)
   (pool-get! classfile 5 (w4llong n)) )

(define (pool-double classfile n)
   (pool-get! classfile 6 (f4 n)) )

(define (pool-class classfile classe)
   (with-access::classe classe (pool name)
      (if pool
	  pool
	  (let ( (pname (pool-name classfile name)) )
	     (let ( (r (pool-get! classfile 7 (list pname))) )
		(set! pool r)
		r )))))

(define (pool-class-by-name classfile name)
   (pool-class classfile (declared-class classfile name)) )

(define (pool-class-by-reftype classfile reftype)
   (if (vect? reftype)
       (let ( (pname (pool-name classfile (type-code reftype))) )
	  (pool-get! classfile 7 (list pname)) )
       (pool-class classfile reftype) ))

(define (pool-string classfile str)
   (pool-get! classfile 8 (list (pool-name classfile str))) )

(define (pool-field classfile field)
   (pool-field-method classfile field 9) )

(define (pool-method  classfile method)
   (pool-field-method classfile method 10) )

(define (pool-interface-method classfile field)
   (pool-field-method classfile field 11) )

(define (pool-field-method classfile fm tag)
   (with-access::field-or-method fm (pool owner name type pname descriptor)
      (if pool
	  pool
	  (let* ( (pn (pool-name classfile name))
		  (pt (pool-name classfile (type-code type)))
		  (c  (pool-class-by-name classfile owner))
		  (d (pool-get! classfile 12 (list pn pt)))
		  (r (pool-get! classfile tag (list c d))) )
	     (set! pname pn)
	     (set! descriptor pt)
	     (set! pool r)
	     r ))))

(define (pool-local-method classfile::classfile method::method)
   ;; local method may not need a CONSTANT_Methodref...
   (pool-field-method-local classfile method 10) )

(define (pool-field-method-local classfile fm tag)
   (with-access::field-or-method fm (pool name type pname descriptor)
      (unless pool
	 (set! pname (pool-name classfile name))
	 (set! descriptor (pool-name classfile (type-code type))) )))
