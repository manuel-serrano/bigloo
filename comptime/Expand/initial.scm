;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Expand/initial.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:41:05 1994                          */
;*    Last change :  Wed Mar 13 06:52:06 2019 (serrano)                */
;*    Copyright   :  1994-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Initial compiler expanders.                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_install
   (include "Tools/location.sch")
   (import  expand_if
	    expand_lambda
	    expand_define
	    expand_expander
	    expand_exit
	    expand_garithmetique
	    expand_iarithmetique
	    expand_farithmetique
	    expand_let
	    expand_case
	    expand_struct
	    expand_map
	    expand_assert
	    expand_object
	    expand_multiple-values
	    tools_misc
	    tools_location
	    tools_error
	    tools_args
	    engine_param
	    expand_expander
	    type_type
	    ast_ident
	    ast_let
	    ast_labels
	    ast_node
	    ast_var
	    ast_sexp
	    ast_private)
   (export  (install-initial-expander)
	    (%append-2-define)))

;*---------------------------------------------------------------------*/
;*    installedp ...                                                   */
;*---------------------------------------------------------------------*/
(define installedp #f)

;*---------------------------------------------------------------------*/
;*    install-initial-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (install-initial-expander)
   (unless installedp
      (set! installedp #t)
      (install-expanders)))

;*---------------------------------------------------------------------*/
;*    install-expanders ...                                            */
;*---------------------------------------------------------------------*/
(define (install-expanders)
   ;; In order to be able to install O-macros,
   ;; we first of all, we perform Oenv initialization
   (initialize-Oenv!)
   (initialize-Genv!)

   ;; #meta
   (install-compiler-expander '|#meta|
      (lambda (x e)
	 (expand-meta x (internal-begin-expander e))))
   
   ;; if
   (install-compiler-expander 'if expand-if)
   
   ;; or
   (install-compiler-expander 'or expand-or)
   
   ;; and
   (install-compiler-expander 'and expand-and)
   
   ;; not
   (install-compiler-expander 'not expand-not)
   
   ;; lambda
   (install-compiler-expander 'lambda expand-lambda)
   
   ;; define
   (install-compiler-expander 'define expand-define)
   
   ;; define-inline
   (install-compiler-expander 'define-inline expand-inline)
   
   ;; define-generic
   (install-compiler-expander 'define-generic expand-generic)
   
   ;; define-method
   (install-compiler-expander 'define-method expand-method)
   
   ;; define-struct
   (install-compiler-expander 'define-struct expand-struct)
   
   ;; set!
   (install-compiler-expander 'set! expand-set!)
   
   ;; set-exit
   (install-compiler-expander 'set-exit expand-set-exit)
   
   ;; jump-exit
   (install-compiler-expander 'jump-exit expand-jump-exit)
   
   ;; bind-exit
   (install-compiler-expander 'bind-exit expand-bind-exit)
   
   ;; unwind-protect
   (install-compiler-expander 'unwind-protect expand-unwind-protect)

   ;; with-handler
   (install-compiler-expander 'with-handler expand-with-handler)
   
   ;; multiple-value-bind
   (install-compiler-expander 'multiple-value-bind expand-mvalue-bind)

   ;; private
   (install-compiler-expander (private-stamp)
      (lambda (x e)
	 (map! (lambda (x) (e x e)) (cddr x))
	 x))

   ;; error 
   (install-O-comptime-expander
    'error
    (lambda (x::obj e::procedure)
       (let ((loc (find-location x)))
	  (if (and (location? loc) *error-localization*)
	      (match-case x
		 ((?- ?l1 ?l2 ?l3)
		  `(begin
		      (error/c-location ,(e l1 e)
					,(e l2 e)
					,(e l3 e)
					,(location-full-fname loc)
					,(location-pos loc))
		      (error #f #f #f)))
		 ((?- . ?list)
		  `(error ,@(map (lambda (l) (e l e)) list))))
	      `(error ,@(map (lambda (l) (e l e)) (cdr x)))))))
   
   ;; warning
   (install-O-comptime-expander
    'warning
    (lambda (x::obj e::procedure)
       (let ((loc (find-location x)))
	  (if (and (location? loc) *error-localization*)
	      `(warning/c-location ,(location-full-fname loc)
				   ,(location-pos loc)
				   ,@(map (lambda (l) (e l e))
					  (cdr x)))
	      `(warning ,@(map (lambda (l) (e l e)) (cdr x)))))))
   
   ;; append
   (install-O-comptime-expander
    'append
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?l1 ?l2)
	   (e `(append-2 ,l1 ,l2) e))
	  ((?- . ?lists)
	   `(append ,@(map (lambda (l) (e l e)) lists)))
	  (else
	   (error #f "Illegal `append' form" x)))))
   ;; append-2
   (install-O-comptime-expander
    'append-2
    (lambda (x::obj e::procedure)
	  (match-case x
	     ((?- ?l1 ?l2)
	      (if (>=fx *optim* 2)
		  (e `(,%append-2-id ,l1 ,l2) e)
		  (map (lambda (x) (e x e)) x)))
	     (else
	      (error #f "Illegal `append-2' form" x)))))
   
   ;; append!
   (install-O-comptime-expander
    'append!
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?l1 ?l2)
	   (e `(append-2! ,l1 ,l2) e))
	  ((?- . ?lists)
	   `(append! ,@(map (lambda (l) (e l e)) lists)))
	  (else
	   (error #f "Illegal `append!' form" x)))))
   
   ;; eappend
   (install-O-comptime-expander
    'eappend
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?l1 ?l2)
	   (e `(eappend-2 ,l1 ,l2) e))
	  ((?- . ?lists)
	   `(eappend ,@(map (lambda (l) (e l e)) lists)))
	  (else
	   (error #f "Illegal `eappend' form" x)))))
   
   ;; string-length
   (install-O-comptime-expander
    'string-length
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?s)
	   (if (string? s)
	       (string-length s)
	       `(string-length ,(e s e))))
	  (else
	   (error #f "Illegal 'string-length' form" x)))))
   
   ;; cons
   (install-O-comptime-expander
    'cons
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?a ?d)
	   `($cons ,(e a e) ,(e d e)))
	  (else
	   (error #f "Illegal `cons' form" x)))))
   
   ;; map
   (install-O-comptime-expander 'map expand-map)
   (install-G-comptime-expander 'map
				(lambda (x::obj e::procedure)
				   (map-check x e ''())))
   ;; map!
   (install-O-comptime-expander 'map! expand-map!)
   (install-G-comptime-expander 'map!
				(lambda (x::obj e::procedure)
				   (map-check x e ''())))

   ;; append-map
   (install-O-comptime-expander 'append-map expand-append-map)
   (install-G-comptime-expander 'append-map
				(lambda (x::obj e::procedure)
				   (map-check x e ''())))

   ;; for-each
   (install-O-comptime-expander 'for-each expand-for-each)
   (install-G-comptime-expander 'for-each
				(lambda (x::obj e::procedure)
				   (map-check x e #unspecified)))
   
   ;; filter and filter!
   (install-O-comptime-expander 'filter expand-filter)
   (install-G-comptime-expander 'filter
				(lambda (x::obj e::procedure)
				   (map-check x e '())))
   (install-G-comptime-expander 'filter!
				(lambda (x::obj e::procedure)
				   (map-check x e #unspecified)))
   
   ;; any / every
   (install-O-comptime-expander 'any expand-any)
   (install-G-comptime-expander 'any
				(lambda (x::obj e::procedure)
				   (map-check x e #f)))
   (install-O-comptime-expander 'every expand-every)
   (install-G-comptime-expander 'every
				(lambda (x::obj e::procedure)
				   (map-check x e #t)))

   ;; reduce
   (install-O-comptime-expander 'reduce expand-reduce)

   ;; find 
   (install-O-comptime-expander 'find expand-find)

   ;; if
   (install-O-comptime-expander 'if
      (lambda (x::obj e::procedure)
	 (let ((res (map (lambda (x) (e x e)) x)))
	    (tprint "if rest=" res)
	    (match-case res
	       ((?- ?test #t #f)
		test)
	       (else
		res)))))
   
   ;; equal?
   (install-O-comptime-expander
    'equal?
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?a1 ?a2)
	   (cond
	      ((and (number? a1) (number? a2))
	       (= a1 a2))
	      ((or (number? a1) (number? a2))
	       (e `(eqv? ,a1 ,a2) e))
	      ((or (char? a1)
		   (char? a2)
		   (and (pair? a1)
			(eq? (car a1) 'quote)
			(or (fixnum? (cadr a1))
			    (char? (cadr a1))
			    (symbol? (cadr a1))
			    (keyword? (cadr a1))))
		   (and (pair? a2)
			(eq? (car a2) 'quote)
			(or (fixnum? (cadr a2))
			    (char? (cadr a2))
			    (symbol? (cadr a2))
			    (keyword? (cadr a2)))))
	       (e `(eq? ,a1 ,a2) e))
	      (else
	       `(equal? ,(e a1 e) ,(e a2 e)))))
	  (else
	   (error #f "Illegal `equal?' form" x)))))
   
   ;; eqv?
   (install-O-comptime-expander
    'eqv?
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?a1 ?a2)
	   (if (and (number? a1) (number? a2))
	       (= a1 a2)
	       `(,(if (or (char? a1)
			  (char? a2)
			  (and (pair? a1)
			       (eq? (car a1) 'quote)
			       (or (fixnum? (cadr a1))
				   (char? (cadr a1))
				   (symbol? (cadr a1))
				   (keyword? (cadr a1))))
			  (and (pair? a2)
			       (eq? (car a2) 'quote)
			       (or (fixnum? (cadr a2))
				   (char? (cadr a2))
				   (symbol? (cadr a2))
				   (keyword? (cadr a2)))))
		      'eq?
		      'eqv?)
		 ,(e a1 e)
		 ,(e a2 e))))
	  (else
	   (error #f "Illegal `eqv?' form" x)))))

   ;; number testing
   (install-O-comptime-expander 'fixnum?
      (lambda (x e)
	 (match-case x
	    ((?- (? fixnum?))
	     #t)
	    ((?- (? number?))
	     #f)
	    (else
	     (map (lambda (x) (e x e)) x)))))
   (install-O-comptime-expander 'flonum?
      (lambda (x e)
	 (match-case x
	    ((?- (? flonum?))
	     #t)
	    ((?- (? number?))
	     #f)
	    (else
	     (map (lambda (x) (e x e)) x)))))

   ;; arithmetic procedures
   (install-O-comptime-expander '+
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g+ x e) (expand-i+ x e))))
   (install-O-comptime-expander '-
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g- x e) (expand-i- x e))))
   (install-O-comptime-expander '*
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g* x e) (expand-i* x e))))
   (install-O-comptime-expander '/
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g/ x e) (expand-i/ x e))))
   (install-O-comptime-expander '=
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g= x e) (expand-i= x e))))
   (install-O-comptime-expander '<
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g< x e) (expand-i< x e))))
   (install-O-comptime-expander '>
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g> x e) (expand-i> x e))))
   (install-O-comptime-expander '<=
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g<= x e) (expand-i<= x e))))
   (install-O-comptime-expander '>=
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-g>= x e) (expand-i>= x e))))
   (install-O-comptime-expander 'max
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-gmax x e) (expand-maxfx x e))))
   (install-O-comptime-expander 'min
      (lambda (x e)
	 (if *arithmetic-genericity* (expand-gmin x e) (expand-minfx x e))))
   
   (install-G-comptime-expander '+ (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '* (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '/ (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '- (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '= (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '> (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '< (lambda (x::obj e::procedure)
				      (call-check x 'number? "number" e)))
   (install-G-comptime-expander '>= (lambda (x::obj e::procedure)
				       (call-check x 'number? "number" e)))
   (install-G-comptime-expander '<= (lambda (x::obj e::procedure)
				       (call-check x 'number? "number" e)))
   (install-G-comptime-expander 'cos (lambda (x::obj e::procedure)
					(call-check x 'number? "number" e)))
   (install-G-comptime-expander 'sin (lambda (x::obj e::procedure)
					(call-check x 'number? "number" e)))
   (install-G-comptime-expander 'min (lambda (x::obj e::procedure)
					(call-check x 'number? "number" e)))
   (install-G-comptime-expander 'max (lambda (x::obj e::procedure)
					(call-check x 'number? "number" e)))

   ;; bit-ops
   (install-O-comptime-expander 'bit-lsh expand-bit-lsh)
   (install-O-comptime-expander 'bit-lshu32 expand-bit-lshu32)
   (install-O-comptime-expander 'bit-rsh expand-bit-rsh)
   (install-O-comptime-expander 'bit-rshs32 expand-bit-rshs32)
   (install-O-comptime-expander 'bit-rshu32 expand-bit-rshu32)
   (install-O-comptime-expander 'bit-ursh expand-bit-ursh)
   (install-O-comptime-expander 'bit-urshu32 expand-bit-urshu32)
   (install-O-comptime-expander 'bit-ors32 expand-bit-ors32)

   ;; eq?
   (install-O-comptime-expander 'eq? expand-eq?)
   
   ;; fx
   (install-O-comptime-expander '+fx expand-+fx)
   (install-O-comptime-expander '-fx expand--fx)
   (install-O-comptime-expander '*fx expand-*fx)
   (install-O-comptime-expander '/fx expand-/fx)
   (install-O-comptime-expander 'maxfx expand-maxfx)
   (install-O-comptime-expander 'minfx expand-minfx)

   ;; fl
   (install-O-comptime-expander '+fl expand-+fl)
   (install-O-comptime-expander '-fl expand--fl)
   (install-O-comptime-expander '*fl expand-*fl)
   (install-O-comptime-expander '/fl expand-/fl)
   (install-O-comptime-expander 'maxfl expand-fmax)
   (install-O-comptime-expander 'minfl expand-fmin)
   (install-O-comptime-expander 'atanfl expand-fatan)
   
   ;; sqrtfl
   (install-O-comptime-expander
    'sqrtfl
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?n)
	   (if *unsafe-range*
	       `(sqrtfl-ur ,(e n e))
	       `(sqrtfl ,(e n e))))
	  (else
	   (error #f "Illegal `sqrtfl' call" x)))))
   ;; atan-2fl
   (install-O-comptime-expander
    'atan-2fl
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?n ?m)
	   (if *unsafe-range*
	       `(atan-2fl-ur ,(e n e) ,(e m e))
	       `(atan-2fl ,(e n e) ,(e m e))))
	  (else
	   (error #f "Illegal `atan-2fl' call" x)))))
   ;; let*
   (install-compiler-expander 'let* expand-let*)
   
   ;; let
   (install-compiler-expander 'let expand-let)
   (install-compiler-expander (let-sym) expand-let)
   
   ;; letrec
   (install-compiler-expander 'letrec expand-letrec)

   ;; letrec*
   (install-compiler-expander 'letrec* expand-letrec)
   
   ;; labels
   (install-compiler-expander 'labels expand-labels)
   (install-compiler-expander (labels-sym) expand-labels)
   
   ;; case
   (install-compiler-expander 'case expand-case)
   
   ;; read
   (install-O-comptime-expander
    'read
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?port)
	   `(read ,(e port e)))
	  ((?- ?port ?value)
	   `(read ,(e port e) ,(e value e)))
	  ((?-)
	   `(read ((@ current-input-port __r4_ports_6_10_1))))
	  (else
	   (error #f "Illegal `read' form" x)))))
   
   ;; read/rp
   (install-O-comptime-expander
    'read/rp
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?g ?port . ?opts)
	   (if (>=fx (bigloo-compiler-debug) 2)
	       (let ((gr (gensym)))
		  `(let ((,gr ,(e g e)))
		      (if (correct-arity? ,gr ,(+fx 1 (length opts)))
			  (,gr ,(e port e) ,@(map (lambda (x) (e x e)) opts))
			  (error 'read/rp "Grammar arity mismatch" ,gr))))
	       `(,(e g e) ,(e port e) ,@(map (lambda (x) (e x e)) opts))))
	  (else
	   (error #f "Illegal `read/rp' form" x)))))
   
   ;; vector
   (install-O-comptime-expander
    'vector
    (lambda (x::obj e::procedure)
       (let ((args (cdr x))
	     (v    (mark-symbol-non-user!
		    (gensym 'v))))
	  (e `(let ((,v ($create-vector ,(length args))))
		 ,@(let loop ((i    0)
			      (args args)
			      (res  '()))
		      (if (null? args)
			  (reverse! res)
			  (loop (+fx i 1)
				(cdr args)
				(cons
				 (epairify
				  `(vector-set-ur! ,v ,i ,(car args)) x)
				 res))))
		 ,v)
	     e))))

   ;; vector-for-each
   (install-O-comptime-expander 'vector-for-each expand-vector-for-each)
   (install-O-comptime-expander 'vector-map expand-vector-map)
   (install-O-comptime-expander 'vector-map! expand-vector-map!)

   ;; make-vector
   (install-O-comptime-expander
    'make-vector
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?n)
	   `($make-vector ,(e n e) ,(e '(unspecified) e)))
	  ((?- ?n ?init)
	   `($make-vector ,(e n e) ,(e init e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))

   ;; vector-copy
   (install-O-comptime-expander
    'vector-copy
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?vec ?start ?stop)
	   (set-car! x 'vector-copy3)
	   (e x e))
	  (else
	   (map (lambda (x) (e x e)) x)))))

   ;; typed vectors
   (let ((evref (lambda (x::obj e::procedure)
		   (match-case x
		      ((?op ?vec ?k)
		       (let ((evec (e vec e))
			     (ek (e k e)))
			  (if *unsafe-range*
			      `(,(symbol-append '$ op) ,evec ,ek)
			      `(,op ,evec ,ek))))
		      (else
		       (error #f "Illegal `vector-ref' form" x)))))
	 (evset! (lambda (x::obj e::procedure)
		    (match-case x
		       ((?op ?vec ?k ?obj)
			(let ((evec (e vec e))
			      (ek   (e k e))
			      (eobj (e obj e)))
			   (if *unsafe-range*
			       `(,(symbol-append '$ op) ,evec ,ek ,eobj)
			       `(,op ,evec ,ek ,eobj))))
		       (else
			(error #f "Illegal `vector-set!' form" x))))))
      (install-O-comptime-expander 's8vector-ref evref)
      (install-O-comptime-expander 's8vector-set! evset!)
      (install-O-comptime-expander 'u8vector-ref evref)
      (install-O-comptime-expander 'u8vector-set! evset!)
      (install-O-comptime-expander 's16vector-ref evref)
      (install-O-comptime-expander 's16vector-set! evset!)
      (install-O-comptime-expander 'u16vector-ref evref)
      (install-O-comptime-expander 'u16vector-set! evset!)
      (install-O-comptime-expander 's32vector-ref evref)
      (install-O-comptime-expander 's32vector-set! evset!)
      (install-O-comptime-expander 'u32vector-ref evref)
      (install-O-comptime-expander 'u32vector-set! evset!)
      (install-O-comptime-expander 's64vector-ref evref)
      (install-O-comptime-expander 's64vector-set! evset!)
      (install-O-comptime-expander 'u64vector-ref evref)
      (install-O-comptime-expander 'u64vector-set! evset!)
      (install-O-comptime-expander 'f32vector-ref evref)
      (install-O-comptime-expander 'f32vector-set! evset!)
      (install-O-comptime-expander 'f64vector-ref evref)
      (install-O-comptime-expander 'f64vector-set! evset!))
   
   ;; string-append
   (install-O-comptime-expander
    'string-append
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?str1)
	   (e str1 e))
	  ((?- ?str1 ?str2)
	   `($string-append ,(e str1 e) ,(e str2 e)))
	  ((?- ?str1 ?str2 ?str3)
	   `($string-append-3 ,(e str1 e) ,(e str2 e) ,(e str3 e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   (install-G-comptime-expander
    'string-append
    (lambda (x::obj e::procedure)
       (call-check x 'string? "string" e)))

   ;; symbol-append
   (install-O-comptime-expander
    'symbol-append
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?sym1)
	   (e sym1 e))
	  ((?- (quote ?sym1) (quote ?sym2))
	   `',(symbol-append sym1 sym2))
	  ((?- ?sym1 ?sym2)
	   (e `(string->symbol
		(string-append (symbol->string ,sym1) (symbol->string ,sym2)))
	      e))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   (install-G-comptime-expander
    'symbol-append
    (lambda (x::obj e::procedure)
       (call-check x 'symbol? "symbol" e)))
   
   ;; substring
   (install-O-comptime-expander
    'substring
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?s ?min ?max)
	   (let ((s   (e s e))
		 (min (e min e))
		 (max (e max e)))
	      (if *unsafe-range*
		  `(substring-ur ,s ,min ,max)
		  `(substring ,s ,min ,max))))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   
   ;; substring-at?
   (install-O-comptime-expander
    'substring-at?
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?s1 ?s2 ?o)
	   (e `($prefix-at? ,s1 ,s2 ,o) e))
	  ((?- ?s1 ?s2 ?o ?l)
	   (e `($substring-at? ,s1 ,s2 ,o ,l) e))
	  (else
	   (error #f "Illegal `substring-at?' form" x)))))
   
   ;; substring-ci-at?
   (install-O-comptime-expander
    'substring-ci-at?
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?s1 ?s2 ?o)
	   (e `($prefix-ci-at? ,s1 ,s2 ,o) e))
	  ((?- ?s1 ?s2 ?o ?l)
	   (e `($substring-ci-at? ,s1 ,s2 ,o ,l) e))
	  (else
	   (error #f "Illegal `substring-ci-at?' form" x)))))
   
   ;; blit-string!
   (install-O-comptime-expander
    'blit-string!
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?s1 ?o1 ?s2 ?o2 ?l)
	   (let ((s1 (e s1 e))
		 (o1 (e o1 e))
		 (s2 (e s2 e))
		 (o2 (e o2 e))
		 (l  (e l e)))
	      (if *unsafe-range*
		  `(blit-string-ur! ,s1 ,o1 ,s2 ,o2 ,l)
		  `(blit-string! ,s1 ,o1 ,s2 ,o2 ,l))))
	  (else
	   (error #f "Illegal `blit-string!' form" x)))))
   
   ;; integer->char
   (install-O-comptime-expander
    'integer->char
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- (and (? fixnum?) ?n))
	   (integer->char n))
	  ((?- ?n)
	   (if *unsafe-range*
	       `(integer->char-ur ,(e n e))
	       `(integer->char ,(e n e))))
	  (else
	   (error #f "Illegal `integer->char' call" x)))))

   ;; string->integer
   (install-O-comptime-expander
      'string->integer
      (lambda (x::obj e::procedure)
	 (match-case x
	    ((?- ?str)
	     (e `(strtol ,str 0 10) e))
	    ((?- ?str (and (? fixnum?) ?r))
	     (if (and (>=fx r 2) (<=fx r 36))
		 (e `(strtol ,str 0 ,r) e)
		 (error #f "Illegal `string->integer' radix" r)))
	    ((?- ?str (and (? fixnum?) ?r) (and (? fixnum?) ?start))
	     (if (and (>=fx r 2) (<=fx r 36))
		 (e `(strtol ,str ,start ,r) e)
		 (error #f "Illegal `string->integer' radix" r)))
	    (else
	     (map (lambda (x) (e x e)) x)))))
   
   ;; char->integer
   (install-O-comptime-expander
    'char->integer
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- (and (? char?) ?n))
	   (char->integer n))
	  ((?- ?n)
	   `(char->integer ,(e n e)))
	  (else
	   (error #f "Illegal `integer->char' call" x)))))
   
   ;; fixnum->flonum
   (install-O-comptime-expander
      'fixnum->flonum
      (lambda (x::obj e::procedure)
	 (match-case x
	    ((?- (and (? fixnum?) ?n))
	     (fixnum->flonum n))
	    ((?- ?n)
	     `(fixnum->flonum ,(e n e)))
	    (else
	     (error #f "Illegal `fixnum->flonum' call" x)))))

   ;; flonum->fixnum
   (install-O-comptime-expander
      'flonum->fixnum
      (lambda (x::obj e::procedure)
	 (match-case x
	    ((?- (and (? flonum?) ?n))
	     (flonum->fixnum n))
	    ((?- ?n)
	     `(flonum->fixnum ,(e n e)))
	    (else
	     (error #f "Illegal `flonum->fixnum' call" x)))))
   
   ;; cons*
   (install-O-comptime-expander
    'cons*
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?x1)
	   (e x1 e))
	  ((?- ?x1 ?x2)
	   `($cons ,(e x1 e) ,(e x2 e)))
	  ((?- ?x1 ?x2 . ?rest)
	   `($cons ,(e x1 e)
		   ,(e (epairify
			`(cons* ,x2 ,@rest)
			x)
		       e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   
   ;; apply
   (install-O-comptime-expander
    'apply
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?function ?one-arg)
	   `(apply ,(e function e)
		   ,(e one-arg e)))
	  ((?- ?function . ?args)
	   `(apply ,(e function e)
		   ,(e (epairify
			`((@ cons*
			     __r4_pairs_and_lists_6_3)
			  ,@args)
			x)
		       e)))
	  (else
	   (error #f
		  "Illegal `apply' form"
		  x)))))
   
   ;; newline
   (install-O-comptime-expander
    'newline
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?-)
	   `((@ newline-1 __r4_output_6_10_3)
	     ((@ current-output-port __r4_ports_6_10_1))))
	  ((?- ?port)
	   `((@ newline-1 __r4_output_6_10_3)
	     ,(e port e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   
   ;; display
   (install-O-comptime-expander
    'display
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?o)
	   `(,(disp o) ,(e o e) ((@ current-output-port __r4_ports_6_10_1))))
	  ((?- ?o ?port)
	   `(,(disp o) ,(e o e) ,(e port e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   
   ;; display-substring
   (install-O-comptime-expander
      'display-substring
      (lambda (x::obj e::procedure)
	 (match-case x
	    ((?- ?o ?i ?j ?p)
	     (if *unsafe-range*
		 `($display-substring ,(e o e) ,(e i e) ,(e j e) ,(e p e))
		 (map (lambda (x) (e x e)) x)))
	    (else
	     (error #f "Illegal `display-substring' call" x)))))
   
   ;; write-char
   (install-O-comptime-expander
    'write-char
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?obj)
	   `((@ write-char-2 __r4_output_6_10_3)
	     ,(e obj e)
	     ((@ current-output-port __r4_ports_6_10_1))))
	  ((?- ?obj ?port)
	   `((@ write-char-2 __r4_output_6_10_3) ,(e obj e) ,(e port e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   
   ;; write-byte
   (install-O-comptime-expander
    'write-byte
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?obj)
	   `((@ write-byte-2 __r4_output_6_10_3)
	     ,(e obj e)
	     ((@ current-output-port __r4_ports_6_10_1))))
	  ((?- ?obj ?port)
	   `((@ write-byte-2 __r4_output_6_10_3) ,(e obj e) ,(e port e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   
   ;; print
   (install-O-comptime-expander
    'print
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?-)
	   `((@ newline-1 __r4_output_6_10_3)
	     ((@ current-output-port __r4_ports_6_10_1))))
	  ((?- . ?obj)
	   (let ((p (mark-symbol-non-user! (gensym 'port))))
	      (e `(,(let-sym)
		   ((,p ((@ current-output-port __r4_ports_6_10_1))))
		   ,@(map
		      (lambda (y) (epairify `(,(disp y) ,y ,p) x))
		      obj)
		   ((@ newline-1 __r4_output_6_10_3) ,p))
		 e))))))
   
   ;; fprint
   (install-O-comptime-expander
    'fprint
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?port)
	   (e `((@ newline-1 __r4_output_6_10_3)
		,port) e))
	  ((?- ?port . ?obj)
	   (let ((aux (mark-symbol-non-user! (gensym 'port))))
	      (e `(,(let-sym)
		   ((,aux ,port))
		   ,@(map (lambda (y)
			     (epairify `(,(disp y) ,y ,aux) x))
			  obj)
		   ((@ newline-1 __r4_output_6_10_3) ,aux))
		 e)))
	  (else
	   (map (lambda (x) (e x e)) x)))))

   ;; with-input-from-file
   (install-O-comptime-expander
    'with-input-from-file
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?expr read-string)
	   (e `(file->string ,expr) e))
	  (else
	   (map (lambda (x) (e x e)) x)))))
   
   ;; inexact->exact
   (install-G-comptime-expander
    'inexact->exact
    (lambda (x::obj e::procedure)
       (call-check x 'number? "number" e)))
   
   ;; exact->inexact
   (install-G-comptime-expander
    'exact->inexact
    (lambda (x::obj e::procedure)
       (call-check x 'number? "number" e)))
   
   ;; values
   (install-O-comptime-expander 'values expand-O-values)
   
   ;; call-with-values
   (install-O-comptime-expander 'call-with-values expand-O-call-with-values)
   
   ;; assert
   (install-compiler-expander 'assert expand-assert)

   ;; define-class
   (install-compiler-expander 'define-class expand-define-class)
   (install-compiler-expander 'define-final-class expand-define-class)
   (install-compiler-expander 'define-abstract-class expand-define-class)
   
   ;; with-access
   (install-compiler-expander 'with-access expand-with-access)
   
   ;; instantiate
   (install-compiler-expander 'instantiate expand-instantiate)
   
   ;; co-instantiate
   (install-compiler-expander 'co-instantiate expand-co-instantiate)
   
   ;; duplicate
   (install-compiler-expander 'duplicate expand-duplicate)
   
   ;; widen!
   (install-compiler-expander 'widen! expand-widen!)
   
   ;; shrink!
   (install-compiler-expander 'shrink! expand-shrink!)
   
   ;; cond-expand
   (install-compiler-expander 'cond-expand expand-compile-cond-expand)
   
   ;; profile
   (install-compiler-expander
    'profile
    (lambda (x e)
       (match-case x
	  ((?- (and (? symbol?) ?lbl) . ?exprs)
	   (if (or (not (number? *profile-mode*)) (= *profile-mode* 0))
	       (e (expand-progn exprs) e)
	       (let* ((la  `(lambda () ,@exprs))
		      (lam (if (epair? x)
			       (econs (car la) (cdr la) (cer x))
			       la))
		      (val (let ((sym (gensym 'value)))
			      (mark-symbol-non-user! sym)
			      sym))
		      (aux `(,(let-sym)
			     ((,lbl ,lam))
			     (GC-profile-push ,(symbol->string lbl) ,lbl)
			     (let ((,val (,lbl)))
				(GC-profile-pop)
				,val)))
		      (res (if (epair? x)
			       (econs (car aux) (cdr aux) (cer x))
			       aux)))
		  (e aux e))))
	  (else
	   (error "profile" "Illegal form" x)))))
   
   ;; profile
   (install-compiler-expander
    'profile/gc
    (lambda (x e)
       (match-case x
	  ((?- (and (? symbol?) ?lbl) . ?exprs)
	   (if (or (not (number? *profile-mode*)) (= *profile-mode* 0))
	       (e (expand-progn exprs) e)
	       (let* ((la  `(lambda () ,@exprs))
		      (lam (if (epair? x)
			       (econs (car la) (cdr la) (cer x))
			       la))
		      (val (let ((sym (gensym 'value)))
			      (mark-symbol-non-user! sym)
			      sym))
		      (aux `(,(let-sym)
			     ((,lbl ,lam))
			     (GC-collect-profile-push ,(symbol->string lbl)
						      ,lbl)
			     (let ((,val (,lbl)))
				(GC-profile-pop)
				,val)))
		      (res (if (epair? x)
			       (econs (car aux) (cdr aux) (cer x))
			       aux)))
		  (e aux e))))
	  (else
	   (error "profile" "Illegal form" x)))))
   
   ;; mmap-set!
   (install-O-comptime-expander
    'mmap-set!
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?vec ?k ?obj)
	   (let ((evec (e vec e))
		 (ek   (e k e))
		 (eobj (e obj e)))
	      (if *unsafe-range*
		  `(mmap-set-ur! ,evec ,ek ,eobj)
		  `(mmap-set! ,evec ,ek ,eobj))))
	  (else
	   (error #f "Illegal `mmap-set!' form" x)))))
   (install-G-comptime-expander
    'mmap-set!
    (lambda (x::obj e::procedure)
       (bound-check x 'mmap-length '$mmap-bound-check? e)))
   
   ;; mmap-ref
   (install-O-comptime-expander
    'mmap-ref
    (lambda (x::obj e::procedure)
       (match-case x
	  ((?- ?vec ?k)
	   (let ((evec (e vec e))
		 (ek   (e k e)))
	      (if *unsafe-range*
		  `(mmap-ref-ur ,evec ,ek)
		  `(mmap-ref ,evec ,ek))))
	  (else
	   (error #f "Illegal `mmap-ref' form" x)))))
   (install-G-comptime-expander
    'mmap-ref
    (lambda (x::obj e::procedure)
       (bound-check x 'mmap-length '$mmap-bound-check? e)))

   ;; pregexp
   (let ((pregexp-expander (lambda (x::obj e::procedure)
			       (match-case x
				  ((?k (and (? string?) ?regexp) . ?rest)
				   ;; the pregexp functions are not re-entrant!
				   ;; they change their argument when its
				   ;; a U-regexp. Hence, we have to copy it
				   ;; first!
				   (let ((id (gensym 'pregexp)))
				      (add-O-macro-toplevel!
					 (e `(define ,id (pregexp ,regexp)) e))
				      `(,k ,id
					  ,@(map (lambda (x) (e x e)) rest))))
				  ((?k . ?rest)
				   `(,k ,@(map (lambda (x) (e x e)) rest)))
				  (else
				   (error #f "Illegal `pregexp' form" x))))))
     (install-O-comptime-expander 'pregexp-match-positions pregexp-expander)
     (install-O-comptime-expander 'pregexp-match pregexp-expander)
     (install-O-comptime-expander 'pregexp-replace pregexp-expander)
     (install-O-comptime-expander 'pregexp-replace* pregexp-expander)
     (install-O-comptime-expander 'pregexp-split pregexp-expander)))
			 
;*---------------------------------------------------------------------*/
;*    call-check ...                                                   */
;*---------------------------------------------------------------------*/
(define (call-check x pred tname e)
   (let* ((fun (car x))
	  (actuals (cdr x))
	  (formals (map (lambda (x) (mark-symbol-non-user! (gensym))) actuals))
	  (msg (mark-symbol-non-user! (gensym)))
	  (loc (find-location x)))
      (epairify-rec
       `(,(let-sym) (,@(map (lambda (f v) (list f (e v e))) formals actuals)
		       (,msg ,(string-append (symbol->string fun) ": argument not a "
					     tname)))
		    ,(let loop ((args formals))
			(if (null? args)
			    (cons fun formals)
			    `(,(if-sym) (,pred ,(car args))
					,(loop (cdr args))
					,(err/loc loc fun msg (car args))))))
       x)))

;*---------------------------------------------------------------------*/
;*    bound-check ...                                                  */
;*---------------------------------------------------------------------*/
(define (bound-check x flen pred e)
   (match-case x
      ((?fun ?aobj ?aoff . ?rest)
       (let ((fobj (mark-symbol-non-user! (gensym)))
	     (foff (mark-symbol-non-user! (gensym)))
	     (len (mark-symbol-non-user! (gensym)))
	     (loc (find-location x)))
	  (epairify-rec
	   `(,(let-sym) ((,fobj ,(e aobj e))
			 (,foff ,(e aoff e)))
			(,(let-sym)
			 ((,len (,flen ,fobj)))
			 (,(if-sym) (,pred ,foff ,len)
				    (,fun ,fobj ,foff ,@(map (lambda (x) (e x e)) rest))
				    ,(err/loc loc fun "index out of bound" foff))))
	   x)))
      (else
       (error #f "Illegal expression" x))))

;*---------------------------------------------------------------------*/
;*    map-check ...                                                    */
;*---------------------------------------------------------------------*/
(define (map-check x e null-val)
   (match-case x
      ((?name ?-)
       (user-warning name "used with only two arguments" x)
       null-val)
      ((?op (and ?fun (lambda ?args . ?-)) ?actual)
       (let ((arity (local-arity args))
	     (tmp (mark-symbol-non-user! (gensym)))
	     (ufun (mark-symbol-non-user! (gensym)))
	     (msg-list (mark-symbol-non-user! (gensym)))
	     (loc (find-location x))
	     (msge (string-append (symbol->string op) ": argument not a list")))
	  (if (or (= arity 1) (= arity -1))
	      (epairify-rec 
	       `(,(let-sym) ((,tmp ,(e actual e))
			     (,ufun ,(e fun e)))
			    (,(if-sym) (list? ,tmp)
				       (,op ,ufun ,tmp)
				       ((@ error  __error) #f ,msge ,tmp)))
	       x)
	      (error op "Illegal function arity" x))))
      ((?op ?fun . ?actuals)
       (let ((formals (map (lambda (x) (mark-symbol-non-user! (gensym)))
			   actuals))
	     (lformals (map (lambda (x) (mark-symbol-non-user! (gensym)))
			    actuals))
	     (ufun (mark-symbol-non-user! (gensym)))
	     (msg-list (mark-symbol-non-user! (gensym)))
	     (loc (find-location x)))
	  (epairify-rec 
	   `(,(let-sym) (,@(map (lambda (f v) (list f (e v e))) formals actuals)
			   (,ufun ,(e fun e))
			   (,msg-list ,(string-append (symbol->string op)
						      ": argument not a list")))
			(if (correct-arity? ,ufun ,(length actuals))
			    ,(let loop ((args formals))
				(if (null? args)
				    (if (>fx (length actuals) 1)
					`(,(let-sym)
					  ,(map (lambda (lf f)
						   `(,lf (length ,f)))
						lformals formals)
					  (,(if-sym) (= ,@lformals)
						     (,op ,ufun ,@formals)
						     ,(err/loc loc
							       op
							       "Various list length"
							       `(list ,@lformals))))
					`(,op ,ufun ,@formals))
				    `(,(if-sym) (list? ,(car args))
						,(loop (cdr args))
						((@ error  __error) #f ,msg-list ,(car args)))))
			    ,(err/loc loc
				      op
				      "Incorrect function arity"
				      (length actuals))))
	   x)))
      (else
       (error (car x) "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    err/loc ...                                                      */
;*---------------------------------------------------------------------*/
(define (err/loc loc proc msg obj)
   (if (location? loc)
       `((@ error/location  __error) ',proc
				     ,msg
				     ,obj
				     ,(location-full-fname loc)
				     ,(location-pos loc))
       `((@ error  __error) ',proc ,msg ,obj)))
   
;*---------------------------------------------------------------------*/
;*    disp ...                                                         */
;*---------------------------------------------------------------------*/
(define (disp obj)
   (cond
      ((string? obj)
       '(@ display-string __r4_output_6_10_3))
      ((and (pair? obj)
	    (eq? (car obj) 'quote)
	    (symbol? (cadr obj)))
       '(@ display-symbol __r4_output_6_10_3))
      ((fixnum? obj)
       '(@ display-fixnum __r4_output_6_10_3))
      ((flonum? obj)
       '(@ display-flonum __r4_output_6_10_3))
      ((char? obj)
       '(@ write-char-2 __r4_output_6_10_3))
      (else
       '(@ display-2 __r4_output_6_10_3))))

;*---------------------------------------------------------------------*/
;*    append-2-id ...                                                  */
;*---------------------------------------------------------------------*/
(define %append-2-id (gensym 'append-2))

;*---------------------------------------------------------------------*/
;*    %append-2-define ...                                             */
;*    -------------------------------------------------------------    */
;*    APPEND is special. It cannot be typed                            */
;*                                                                     */
;*      append: pair-nil x pair-nil -> pair-nil                        */
;*                                                                     */
;*    because one may use expressions such as:                         */
;*                                                                     */
;*      (append '(1 2 3) 4)                                            */
;*                                                                     */
;*    Hence, APPEND breaks the type inference because it introduces    */
;*    "obj" type in the CFA. To work around that problem we override   */
;*    the global definition with a local function which can be         */
;*    infered as returning a pair-nil value.                           */
;*---------------------------------------------------------------------*/
(define (%append-2-define)
   `(define (,%append-2-id l1::pair-nil l2)
       (let ((head (cons '() l2)))
	  (labels ((loop (prev tail)
			 (if (pair? tail)
			     (let ((new-prev (cons (car tail) l2)))
				(set-cdr! prev new-prev)
				(loop new-prev (cdr tail)))
			     '())))
	     (loop head l1)
	     (cdr head)))))
