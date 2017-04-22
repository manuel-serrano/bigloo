;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/walk.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  6 17:48:38 2013                          */
;*    Last change :  Fri Apr 21 18:47:51 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Ast walkers                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_walk

   (import type_type
	   ast_var
	   ast_node)

   (export (generic walk0 n::node p::procedure)
	   (generic walk1 n::node p::procedure a0)
	   (generic walk2 n::node p::procedure a0 a1)
	   (generic walk3 n::node p::procedure a0 a1 a2)
	   (generic walk4 n::node p::procedure a0 a1 a2 a3)
	   (generic walk0*::pair-nil n::node p::procedure)
	   (generic walk1*::pair-nil n::node p::procedure a0)
	   (generic walk2*::pair-nil n::node p::procedure a0 a1)
	   (generic walk3*::pair-nil n::node p::procedure a0 a1 a2)
	   (generic walk4*::pair-nil n::node p::procedure a0 a1 a2 a3)
	   (generic walk0!::node n::node p::procedure)
	   (generic walk1!::node n::node p::procedure a0)
	   (generic walk2!::node n::node p::procedure a0 a1)
	   (generic walk3!::node n::node p::procedure a0 a1 a2)
	   (generic walk4!::node n::node p::procedure a0 a1 a2 a3)
	   
	   (macro define-walk-method)))

;*---------------------------------------------------------------------*/
;*    define-walk-method ...                                           */
;*    -------------------------------------------------------------    */
;*    (define-walk-method (optim! this::While x y) BODY)               */
;*    =>                                                               */
;*    (define-method (optim! this::While x y)                          */
;*      (define (default-walk! n x y)                                  */
;*         (walk2! n optim! x y))                                      */
;*      (define (walk! n x y)                                          */
;*         (optim! n x y))                                             */
;*      BODY)                                                          */
;*---------------------------------------------------------------------*/
(define-macro (define-walk-method args . body)
   
   (define (parse-ident id::symbol)
      (let* ((string (symbol->string id))
	     (len (string-length string)))
	 (let loop ((walker  0))
	    (cond
	       ((=fx walker len)
		(values id #f))
	       ((and (char=? (string-ref string walker) #\:)
		     (<fx walker (-fx len 1))
		     (char=? (string-ref string (+fx walker 1)) #\:))
		(values (string->symbol (substring string 0 walker))
		   (string->symbol (substring string (+fx walker 2)))))
	       (else
		(loop (+fx walker 1)))))))

   (define (id-without-type id)
      (multiple-value-bind (id type)
	 (parse-ident id)
	 id))

   (define (id-type id)
      (multiple-value-bind (id type)
	 (parse-ident id)
	 type))

   (let* ((name (car args))
	  (sname (symbol->string name))
	  (i (string-contains sname "::"))
	  (c (string-ref sname (-fx (or i (string-length sname)) 1)))
	  (nb-method-args (-fx (length args) 2))
	  (short-walk (case c
			 ((#\!) 'walk!)
			 ((#\*) 'walk*)
			 (else 'walk)))
	  (tname (case c
		    ((#\!) (string->symbol (string-append sname "::node")))
		    ((#\*) (string->symbol (string-append sname "::pair-nil")))
		    (else name)))
	  (long-walk (case c
			((#\!)
			 (string->symbol (format "walk~a!" nb-method-args)))
			((#\*)
			 (string->symbol (format "walk~a*" nb-method-args)))
			(else
			 (string->symbol (format "walk~a" nb-method-args)))))
	  (define-gen/met (if (eq? (id-type (cadr args)) 'node)
			      'define-generic
			      'define-method))
	  (default-walk (symbol-append 'default- short-walk)))
      `(,define-gen/met	(,tname ,@(cdr args))
	  (define (call-default-walker)
	     (,long-walk ,(id-without-type (cadr args)) ,(id-without-type name)
		,@(map id-without-type (cddr args))))
	  (define (,default-walk ,(id-without-type (cadr args)) ,@(cddr args))
	     (,long-walk ,(id-without-type (cadr args)) ,(id-without-type name)
		,@(map id-without-type (cddr args))))
	  (define (,short-walk ,(id-without-type (cadr args)) ,@(cddr args))
	     (,name ,@(map id-without-type (cdr args))))
	  ,@body)))

;*---------------------------------------------------------------------*/
;*    generic walks ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (walk0 n::node p::procedure)
   (error "walk0" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1 n::node p::procedure arg0)
   (error "walk1" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2 n::node p::procedure arg0 arg1)
   (error "walk2" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3 n::node p::procedure arg0 arg1 arg2)
   (error "walk3" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4 n::node p::procedure arg0 arg1 arg2 arg3)
   (error "walk4" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))

(define-generic (walk0*::pair-nil n::node p::procedure)
   (error "walk0*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1*::pair-nil n::node p::procedure arg0)
   (error "walk1*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2*::pair-nil n::node p::procedure arg0 arg1)
   (error "walk2*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3*::pair-nil n::node p::procedure arg0 arg1 arg2)
   (error "walk3*" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4*::pair-nil n::node p::procedure arg0 arg1 arg2 arg3)
   (error "walk4!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))

(define-generic (walk0!::node n::node p::procedure)
   (error "walk0!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk1!::node n::node p::procedure arg0)
   (error "walk1!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk2!::node n::node p::procedure arg0 arg1)
   (error "walk2!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk3!::node n::node p::procedure arg0 arg1 arg2)
   (error "walk3!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))
(define-generic (walk4!::node n::node p::procedure arg0 arg1 arg2 arg3)
   (error "walk4!" "Internal Error: forgot Node type"
      (with-output-to-string (lambda () (write-circle n)))))

;*---------------------------------------------------------------------*/
;*    gen-walks ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (gen-walks class . fields)
   
   (define (field-name f)
      (match-case f
	 ((? symbol?)
	  f)
	 (((? symbol?))
	  (car f))
	 ((=> ?- ?- ?field)
	  field)))
   
   (define (visit f nb-args)
      (match-case f
	 ((? symbol?)
	  `(p ,f ,@(map (lambda (i)
			   (string->symbol (format "arg~a" i)))
		      (iota nb-args))))
	 (((? symbol?))
	  `(for-each (lambda (f)
			(p f 
			   ,@(map (lambda (i)
				     (string->symbol (format "arg~a" i)))
				(iota nb-args))))
	      ,(car f)))
	 ((=> ?get ?set ?field)
	  `(for-each (lambda (f)
			(p (,get f)
			   ,@(map (lambda (i)
				     (string->symbol (format "arg~a" i)))
				(iota nb-args))))
	      ,field))))
   
   (define (visit* f nb-args)
      (match-case f
	 ((? symbol?)
	  `(p ,f ,@(map (lambda (i)
			   (string->symbol (format "arg~a" i)))
		      (iota nb-args))))
	 (((? symbol?))
	   `(append-map (lambda (f)
			   (p f 
			      ,@(map (lambda (i)
					(string->symbol (format "arg~a" i)))
				   (iota nb-args))))
	       ,(car f)))
	 ((=> ?get ?set ?field)
	  `(append-map (lambda (f)
			  (p (,get f)
			     ,@(map (lambda (i)
				       (string->symbol (format "arg~a" i)))
				  (iota nb-args))))
	      ,field))))
   
   (define (visit! f nb-args)
      (match-case f
	 ((? symbol?)
	  `(set! ,f (p ,f ,@(map (lambda (i)
				    (string->symbol (format "arg~a" i)))
			       (iota nb-args)))))
	 (((? symbol?))
	  `(let loop ((fields ,(car f)))
	      (unless (null? fields)
		 (set-car! fields
		    (p (car fields)
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args))))
		 (loop (cdr fields)))))
	 ((=> ?get ?set ?field)
	  `(let loop ((fields ,field))
	      (unless (null? fields)
		 (,set (car fields)
		    (p (,get (car fields))
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args))))
		 (loop (cdr fields)))))))

   (define (withaccess body)
      `(,(symbol-append 'with-access:: class) n ,(map field-name fields)
           ;;; the body of the with-access form
	   ,body))
      
   (define (gen-method nb-args)
      `(define-method (,(string->symbol (format "walk~a" nb-args))
		       ,(symbol-append 'n:: class)
		       p
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args)))
	  ,(when (pair? fields)
	     (withaccess
		`(begin ,@(map (lambda (f) (visit f nb-args)) fields))))))

   (define (gen-method* nb-args)
      `(define-method (,(string->symbol (format "walk~a*" nb-args))
		       ,(symbol-append 'n:: class)
		       p
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args)))
	  ,(if (null? fields)
	       ''()
	       (withaccess
		  `(append ,@(map (lambda (f) (visit* f nb-args)) fields))))))

   (define (gen-method! nb-args)
      `(define-method (,(string->symbol (format "walk~a!" nb-args))
		       ,(symbol-append 'n:: class)
		       p
		       ,@(map (lambda (i)
				 (string->symbol (format "arg~a" i)))
			    (iota nb-args)))
	  ,(when (pair? fields)
	     (withaccess
		`(begin
		    ,@(map (lambda (f) (visit! f nb-args)) fields))))
	  n))
   
   `(begin
       ,@(map (lambda (nb) (gen-method nb)) (iota 4))
       ,@(map (lambda (nb) (gen-method* nb)) (iota 4))
       ,@(map (lambda (nb) (gen-method! nb)) (iota 4))))

;*---------------------------------------------------------------------*/
;*    default walk                                                     */
;*---------------------------------------------------------------------*/
(gen-walks node)
(gen-walks sequence (nodes))
(gen-walks app fun (args))
(gen-walks app-ly fun arg)
(gen-walks funcall fun (args))
(gen-walks extern (expr*))
(gen-walks cast arg)
(gen-walks setq var value)
(gen-walks conditional test true false)
(gen-walks fail proc msg obj)
(gen-walks set-ex-it body)
(gen-walks jump-ex-it exit value)
(gen-walks retblock body)
(gen-walks return value)
(gen-walks make-box value)
(gen-walks box-ref var)
(gen-walks box-set! var value)
(gen-walks sync mutex prelock body)
(gen-walks switch
   test
   (=> cdr set-cdr! clauses))
(gen-walks let-fun
   (=> (lambda (v) (sfun-body (variable-value v)))
      (lambda (v b) (sfun-body-set! (variable-value v) b))
      locals)
   body)
(gen-walks let-var
   (=> cdr set-cdr! bindings)
   body)

