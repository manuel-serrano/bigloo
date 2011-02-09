;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Bernard Serpette                                  */
;*    Creation    :  Fri Jul  2 10:01:28 2010                          */
;*    Last change :  Wed Feb  9 07:59:03 2011 (serrano)                */
;*    Copyright   :  2010-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    New Bigloo interpreter                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate
   
   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __dsssl
	    __bit
	    __param
	    __bexit
	    __object
	    __thread
	    
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
	    __r5_control_features_6_4
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3

	    __pp
	    __reader
	    __progn
	    __expand
	    __evenv
	    __evcompile
	    __everror
	    __evmodule

	    __evaluate_types
	    __evaluate_avar
	    __evaluate_fsize
	    __evaluate_uncomp
	    __evaluate_comp)

   (export  (evaluate2 sexp env loc)
	    (get-evaluation-contexte)
	    (set-evaluation-contexte! v)))

(define (get-evaluation-contexte)
   (let ( (s (find-state)) )
      (let ( (bp (vector-ref s 0)) )
	 (let ( (r (make-vector bp "")) )
	    (let rec ( (i 0) )
	       (when (<fx i bp)
		  (vector-set! r i (vector-ref s i))
		  (rec (+fx i 1)) ))
	    r ))))

(define (set-evaluation-contexte! v)
   (let ( (s (find-state)) )
      (let ( (bp (vector-ref v 0)) )
	 (let rec ( (i 0) )
	    (when (<fx i bp)
	       (vector-set! s i (vector-ref v i))
	       (rec (+fx i 1)) )))))

;*---------------------------------------------------------------------*/
;*    evaluate2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (evaluate2 sexp env loc)
   (let ( (ast (convert sexp env loc)) )
      (when (> (bigloo-debug) 10) (pp (uncompile ast)))
      (analyse-vars ast)
      (let ( (n (frame-size ast)) )
	 (let ( (f (compile ast)) )
	    (let ( (s (find-state)) )
	       (let ( (bp (vector-ref s 0)) )
		  (unwind-protect (f s)
				  (vector-set! s 0 bp) )))))))

;;
;; Taken elsewhere...
;;


;*---------------------------------------------------------------------*/
;*    get-location ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-location exp loc)
   (or (get-source-location exp) loc))

;;
;; Convert cons to Ast
;;
(define (convert e globals loc)
   (conv e '() globals #f 'nowhere loc #t) )

(define (conv-var v locals)
   (let rec ( (l locals) )
      (if (null? l)
	  #f
	  (let ( (rv (car l)) )
	     (if (eq? v (ev_var-name rv))
		 rv
		 (rec (cdr l)) )))))

(define (conv-begin l locals globals tail? where loc top?)
   (let ( (loc (get-location l loc)) )
      (match-case l
	 (() (instantiate::ev_litt
		(value #unspecified)))
	 ((?e) (conv e locals globals tail? where (get-location e loc) top?))
	 ((?e1 . ?r) (instantiate::ev_prog2
			(e1 (conv e1 locals globals #f where (get-location e1 loc) top?))
			(e2 (conv-begin r locals globals tail? where loc top?)) ))
	 (else (evcompile-error loc "eval" "bad syntax" l)) )))

(define (conv e locals globals tail? where loc top?)
   (define (rconv e) (conv e locals globals tail? where (get-location e loc) #f))
   (define (uconv e) (conv e locals globals #f where (get-location e loc) #f))
   (define (conv-lambda formals body where)
      (define (split-formals l)
	 (let rec ( (r l) (flat '()) (arity 0) )
	    (cond
	       ((null? r) (values (reverse! flat) arity))
	       ((not (pair? r)) (values (reverse! (cons (untype-ident r) flat)) (-fx -1 arity)))
	       (else (rec (cdr r) (cons (untype-ident (car r)) flat) (+fx arity 1))) )))
      (multiple-value-bind (vars arity) (split-formals (dsssl-formals->scheme-formals formals error))
	(let ( (vars (map (lambda (v) (instantiate::ev_var (name v))) vars))
	       (body (make-dsssl-function-prelude e formals body error))
	       (nloc (get-location body loc)) )
	   (instantiate::ev_abs
	      (loc loc)
	      (where where)
	      (arity arity)
	      (vars vars)
	      (body (conv body (append vars locals) globals #t where nloc #f)) ))))
   (match-case e
      ((atom ?x)
       (if (symbol? x)
	   (or (conv-var x locals)
	       (instantiate::ev_global
		  (loc loc)
		  (name x)
		  (mod (if (evmodule? globals) globals ($eval-module)))) )
	   (instantiate::ev_litt
	      (value x)) ))
      ((module . ?bah)
       (if top?
	   (conv (expand (evmodule e (get-location e loc))) locals globals where #f loc #t)
	   (evcompile-error loc "eval" "Illegal non toplevel module declaration" e) ))
      ((@ (and ?id (? symbol?)) (and ?mod (? symbol?)))
       (instantiate::ev_global
	  (loc loc)
	  (name id)
	  (mod mod)))
      (((and (? symbol?)
	     (? (lambda (x) (conv-var x locals)))
	     ?fun)
	. ?args)
       (let ( (fun (uconv fun)) (args (map uconv args)) )
	  (instantiate::ev_app
	     (loc loc)
	     (fun fun)
	     (args args)
	     (tail? tail?)) ))
      ((trap ?e)
       (instantiate::ev_trap
	  (e (uconv e))) )
      ((quote ?v)
       (instantiate::ev_litt
	  (value v)) )
      ((if ?p ?t ?e)
       (instantiate::ev_if
	  (p (uconv p))
	  (t (rconv t))
	  (e (rconv e))) )
      (((kwote or) . ?args)
       (instantiate::ev_or
	  (args (map uconv args))) )
      (((kwote and) . ?args)
       (instantiate::ev_and
	  (args (map uconv args))) )
      ((begin . ?l)
       (conv-begin l locals globals tail? where loc top?) )
      ((let ?binds . ?body)
       (let ( (vars (map (lambda (b)
			    (instantiate::ev_var
			       (name (untype-ident (car b)))))
			 binds)) )
	  (instantiate::ev_let
	     (vars vars)
	     (vals (map (lambda (b) (uconv (cadr b))) binds))
	     (body (conv-begin body (append vars locals) globals tail? where loc #f)) )))
      ((let* ?binds . ?body)
       (define (conv-vals l vars locals loc)
	  (if (null? l)
	      '()
	      (let ( (loc (get-location l loc)) )
		 (cons (conv (cadar l) locals globals #f where loc #f)
		       (conv-vals (cdr l) (cdr vars) (cons (car vars) locals) loc) ))))
       (let ( (vars (map (lambda (b)
			    (instantiate::ev_var
			       (name (untype-ident (car b)))))
			 binds))
	      (bloc (get-location binds loc)) )
	  (instantiate::ev_let*
	     (vars vars)
	     (vals (conv-vals binds vars locals bloc))
	     (body (conv-begin body (append (reverse vars) locals) globals tail? where loc #f)) )))
      ((letrec ?binds . ?body)
       (let* ( (vars (map (lambda (b)
			    (instantiate::ev_var
			       (name (untype-ident (car b)))))
			 binds))
	       (locals (append vars locals))
	       (bloc (get-location binds loc)) )
	     (instantiate::ev_letrec
		(vars vars)
		(vals (map (lambda (b) (conv (cadr b) locals globals #f where (get-location b bloc) #f)) binds))
		(body (conv-begin body locals globals tail? where loc #f) ))))
      ((set! (@ (and ?id (? symbol?)) (and ?mod (? symbol?))) ?e)
       (instantiate::ev_setglobal
	  (loc loc)
	  (name id)
	  (mod mod)
	  (e (uconv e))))
      ((set! ?v ?e)
       (let ( (cv (conv-var v locals)) (e (uconv e)) )
	  (if cv
	      (instantiate::ev_setlocal
		 (v cv)
		 (e e))
	      (instantiate::ev_setglobal
		 (loc loc)
		 (name v)
		 (mod (if (evmodule? globals) globals ($eval-module)))
		 (e e)) )))
      ((set! . ?-)
       (evcompile-error loc "eval" "Illegal form" e))
      ((define ?gv (lambda ?formals ?body))
       (instantiate::ev_defglobal
	  (loc loc)
	  (name (untype-ident gv))
	  (mod (if (evmodule? globals) globals ($eval-module)))
	  (e (conv-lambda formals body gv)) ))
      ((define ?gv ?ge)
       (instantiate::ev_defglobal
	  (loc loc)
	  (name (untype-ident gv))
	  (mod (if (evmodule? globals) globals ($eval-module)))
	  (e (uconv ge)) ))
      ((bind-exit (?v) . ?body)
       (let ( (var (instantiate::ev_var (name v))) )
	  (instantiate::ev_bind-exit
	     (var var)
	     (body (conv-begin body (cons var locals) globals #f where loc #f)) )))
      ((unwind-protect ?e . ?body)
       (instantiate::ev_unwind-protect
	  (e (uconv e))
	  (body (conv-begin body locals globals #f where loc #f)) ))
      ((with-handler ?h . ?body)
       (instantiate::ev_with-handler
	  (handler (uconv h))
	  (body (conv-begin body locals globals #f where loc #f)) ))
      ((lambda ?formals ?body)
       (conv-lambda formals body 'nowhere) )
      ((?f . ?args)
       (let ( (fun (uconv f)) (args (map uconv args)) )
	  (instantiate::ev_app
	     (loc loc)
	     (fun fun)
	     (args args)
	     (tail? tail?)) ))
      (else (evcompile-error loc "eval" "bad syntax" e)) ))


