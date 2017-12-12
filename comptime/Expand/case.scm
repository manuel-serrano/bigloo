;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/case.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul  3 10:13:16 1992                          */
;*    Last change :  Tue Dec 12 11:30:10 2017 (serrano)                */
;*    Copyright   :  1992-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    On macro-expanse ce satane `case'                                */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_case
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    engine_param
	    type_type
	    ast_ident)
   (export  (expand-case ::obj ::procedure)))
	    
;*---------------------------------------------------------------------*/
;*    expand-case ...                                                  */
;*    -------------------------------------------------------------    */
;*    Le case des constantes a ete rajoute en partie pour la           */
;*    compilation de ML car je ne pense pas que lors d'une compilation */
;*    Scheme cela serve beaucoup. Neanmoins, je n'ai pas voulu trop    */
;*    cabler que les caracteres sont des constantes, c'est pourquoi    */
;*    j'ai fait un effort pour laisser le case des char (meme s'il     */
;*    pourrait etre inclus dans celui des constantes).                 */
;*---------------------------------------------------------------------*/
(define (expand-case x e)
   (trace expand "expand-case: " x #\Newline)
   (match-case x
      ((?- ?value . ?clauses)
       (case (case-type x clauses)
	  ((integer)
	   (trace expand "expand-case [integer]" #\Newline)
	   (do-typed-case 'long value clauses e))
	  ((uint32)
	   (trace expand "expand-case [integer]" #\Newline)
	   (do-typed-case 'uint32 value clauses e))
	  ((int32)
	   (trace expand "expand-case [integer]" #\Newline)
	   (do-typed-case 'int32 value clauses e))
	  ((char)
	   (trace expand "expand-case [char]" #\Newline)
	   (do-typed-case 'char value clauses e))
	  ((cnst)
	   (trace expand "expand-case [cnst]" #\Newline)
	   (do-cnst-case value clauses e))
	  ((symbol)
	   (trace expand "expand-case [symbol]" #\Newline)
	   (do-symbol/keyword-case 'symbol value clauses e))
	  ((keyword)
	   (trace expand "expand-case [keyword]" #\Newline)
	   (do-symbol/keyword-case 'keyword value clauses e))
	  (else
	   (trace expand "expand-case [else]" #\Newline)
	   (do-generic-case value clauses e))))
   (else
    (error "case" "Illegal `case' form" x))))

;*---------------------------------------------------------------------*/
;*    case-type ...                                                    */
;*    < datum+ x sexp+ >+ --> integer @ char @ symbol @ etherogeneous  */
;*    -------------------------------------------------------------    */
;*    On cherche a savoir si on va poouvoir coder ce case comme un     */
;*    `switch' ou s'il va falloir le coder comme un `if'. On ne peut   */
;*    utiliser un `switch' que si tous les datums sont des constantes. */
;*    -------------------------------------------------------------    */
;*    On profite de cette fonction pour s'assurer que chacune des      */
;*    clauses a la bonne syntaxe.                                      */
;*---------------------------------------------------------------------*/
(define (case-type x clauses)
   
   (define (type-match? type1 type2)
      (or (null? type1)
	  (null? type2)
	  (and (not (eq? type1 'fail-type))
	       (or (eq? type1 type2)
		   (and (eq? type1 'cnst) (eq? type2 'char))
		   (and (eq? type1 'char) (eq? type2 'cnst))))))
   
   (define (general type1 type2)
      (cond
	 ((eq? type1 type2) type1)
	 ((eq? type1 'cnst) type1)
	 ((null? type2) type1)
	 (else type2)))
   
   (define (one-type datum)
      (cond
	 ((fixnum? datum) 'integer)
	 ((uint32? datum) 'uint32)
	 ((int32? datum) 'int32)
	 ((char? datum) 'char)
	 ((cnst? datum) 'cnst)
	 ((symbol? datum) 'symbol)
	 ((keyword? datum) 'keyword)
	 (else 'fail-type)))
   
   (define (datum-type datums)
      (let loop ((datums datums)
		 (type '()))
	 (cond
	    ((null? datums)
	     type)
	    ((not (pair? datums))
	     (error 'case "Illegal `case' form" x)
	     #f)
	    (else
	     (let ((dtype (one-type (car datums))))
		(if (type-match? dtype type)
		    (loop (cdr datums) (general dtype type))
		    'fail-type))))))
   
   (let loop ((clauses clauses)
	      (type    '()))
      (if (null? clauses)
	  type
	  (match-case (car clauses)
	     ((else . ?exps)
	      (if (or (not (null? (cdr clauses)))
		      (null? exps))
		  (error "case" "Illegal `case' form" x)
		  type))
	     (((and (not ()) ?datum) . ?exps)
	      (if (null? exps)
		  (error "case" "Illegal `case' form" x)
		  (let ((dtype (datum-type datum)))
		     (if (type-match? dtype type)
			 (loop (cdr clauses) (general dtype type))
			 'etherogeneous))))
	     (else
	      (error "case" "Illegal `case' form" x))))))

;*---------------------------------------------------------------------*/
;*    do-typed-case ...                                                */
;*    type x sexp x < datum+ x sexp+ >+ x (sexp x sexp --> sexp)       */    
;*---------------------------------------------------------------------*/
(define (do-typed-case type value clauses e)
   (let* ((else-body (let loop ((clauses clauses))
			(if (null? clauses)
			    (list #unspecified)
			    (match-case (car clauses)
					(()
					 #unspecified)
					((else . ?body)
					 (map (lambda (x) (e x e)) body))
					(else
					 (loop (cdr clauses)))))))
	  (else-name (mark-symbol-non-user! (gensym "case_else")))
	  (aux       (mark-symbol-non-user! (gensym 'aux))))
      (let ((case `(case ,aux
		      ,@(let loop ((clauses clauses))
			   (if (null? clauses)
			       (begin
				  `((else ,#unspecified)))
			       (match-case (car clauses)
				  (() 
				   `((else #unspecified)))
				  ((else . ?body)
				   `((else (,else-name))))
				  ((?datums . ?body)
				   (if (null? body)
				       (error "case"
					      "Illegal `case' clause"
					      (car clauses))
				       (let* ((nbody (map (lambda (x) (e x e))
							  body))
					      (ebody (epairify-rec nbody body))
					      (nclause `(,datums ,@ebody)))
					  (cons (epairify nclause (car clauses))
						(loop (cdr clauses))))))
				  (else
				   (error "case"
					  "Illegal `case' form"
					  clauses))))))))
	 (type-test aux type value case else-body else-name e))))

;*---------------------------------------------------------------------*/
;*    do-cnst-case ...                                                 */
;*    sexp x < datum+ x sexp+ >+ x (sexp x sexp --> sexp)              */    
;*    -------------------------------------------------------------    */
;*    On transforme un case sur des constantes en case sur des         */
;*    entiers.                                                         */
;*---------------------------------------------------------------------*/
(define (do-cnst-case value clauses e)
   (let* ((aux   (mark-symbol-non-user! (gensym 'aux)))
	  (value `(let ((,aux ,value))
		    (if (cnst? ,aux)
			(cnst->integer ,aux)
			;; on met -1 car les constantes ne peuvent
			;; pas avoir des valeurs negatives.
			-1))))
      (let loop ((c clauses))
	 (if (null? c)
	     (do-typed-case 'long value clauses e)
	     (let ((clause (car c)))
		(if (not (eq? (car clause) 'else))
		    (set-car! clause (map cnst->integer (car clause))))
		(loop (cdr c)))))))

;*---------------------------------------------------------------------*/
;*    do-symbol/keyword-case ...                                       */
;*    sexp x < datum+ x sexp+ >+ x (sexp x sexp --> sexp)              */  
;*---------------------------------------------------------------------*/
(define (do-symbol/keyword-case type value clauses e)
   (if *optim-symbol-case*
       (do-optim-symbol/keyword-case type value clauses e)
       (do-generic-symbol/keyword-case value clauses e)))

;*---------------------------------------------------------------------*/
;*    do-optim-symbol/keyword-case ...                                 */
;*---------------------------------------------------------------------*/
(define (do-optim-symbol/keyword-case type value clauses e)
   (define (get-number-of-elements clauses)
      (let loop ((clauses clauses)
		 (res 0))
	 (if (null? clauses)
	     res
	     (match-case (car clauses)
		(()
		 res)
		((else . ?body)
		 res)
		(((and ?datums (?- . (?- ???-))) . ?body)
		 (loop (cdr clauses) (+fx (length datums) res)))
		(((?datums) . ?body)
		 (loop (cdr clauses) (+fx 1 res)))))))
   (let ((num-els (get-number-of-elements clauses)))
      (if (<fx num-els 10)
	  ;; not worse spending efforts
	  (do-generic-symbol/keyword-case value clauses e)
	  ;; not implemented yet
	  (do-generic-symbol/keyword-case value clauses e))))

;*---------------------------------------------------------------------*/
;*    do-generic-symbol/keyword-case ...                               */
;*---------------------------------------------------------------------*/
(define (do-generic-symbol/keyword-case value clauses e)
   `(let ((case-value ,(e value e)))
       ,(let loop ((clauses clauses))
	   (if (null? clauses)
	       #unspecified
	       (match-case (car clauses)
		  (()
		   #unspecified)
		  ((else . ?body)
		   (if (null? body)
		       #f
		       (e (expand-progn body) e)))
		  (((and ?datums (?- . (?- ???-))) . ?body)
		   (cond
		      ((null? body)
		       (error "case" "Illegal `case' clause" (car clauses)))
		      ((<fx (length datums) 10)
		       `(if ,(e `(or ,@(map (lambda (d) `(c-eq? case-value ',d)) datums)) e)
			    ,(e (expand-progn body) e)
			    ,(loop (cdr clauses))))
		      (else
		       `(if ,(e `(memv case-value ',datums) e)
			    ,(e (expand-progn body) e)
			    ,(loop (cdr clauses))))))
		  (((?datum) . ?body)
		   (if (null? body)
		       (error "case" "Illegal `case' clause" (car clauses))
		       `(if ,(e `(eqv? case-value ',datum) e)
			    ,(e (expand-progn body) e)
			    ,(loop (cdr clauses))))))))))

;*---------------------------------------------------------------------*/
;*    type-test ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-test aux type value case-form else-body else-name e)
   (cond
      ((eq? type 'char)
       `(labels ((,else-name () ,@else-body))
	   (let ((,aux ,(e value e)))
	      (if (c-char? ,aux)
		  ,case-form
		  (,else-name)))))
      ((eq? type 'long)
       `(labels ((,else-name () ,@else-body))
	   (let ((,aux ,(e value e)))
	      (if (c-fixnum? ,aux)
		  ,case-form
		  (,else-name)))))
      ((eq? type 'uint32)
       `(labels ((,else-name () ,@else-body))
	   (let ((,aux ,(e value e)))
	      (if (uint32? ,aux)
		  ,case-form
		  (,else-name)))))
      ((eq? type 'int32)
       `(labels ((,else-name () ,@else-body))
	   (let ((,aux ,(e value e)))
	      (if (int32? ,aux)
		  ,case-form
		  (,else-name)))))
      ((eq? type 'symbol)
       `(labels ((,else-name () ,@else-body))
	   (let ((,aux ,(e value e)))
	      (if (c-symbol? ,aux)
		  ,case-form
		  (,else-name)))))
      ((eq? type 'keyword)
       `(labels ((,else-name () ,@else-body))
	   (let ((,aux ,(e value e)))
	      (if (c-keyword? ,aux)
		  ,case-form
		  (,else-name)))))
      (else
       (error "type-test" "Unknown `case' type" type))))

;*---------------------------------------------------------------------*/
;*    do-generic-case ...                                              */
;*    sexp x < datum+ x sexp+ >+ x (sexp x sexp --> sexp)              */  
;*---------------------------------------------------------------------*/
(define (do-generic-case value clauses e)
   (e `(let ((case-value ,value))
	  ,(let loop ((clauses clauses))
	      (if (null? clauses)
		  #unspecified
		  (match-case (car clauses)
		     (()
		      #unspecified)
		     ((else . ?body)
		      (if (null? body)
			  #f
			  (expand-progn body)))
		     (((and ?datums (?- . (?- ???-))) . ?body)
		      (cond
			 ((null? body)
			  (error "case" "Illegal `case' clause" (car clauses)))
			 ((and (list? datums) (<fx (length datums) 10))
			  ;; unroll memv
			  `(if (or ,@(map (lambda (d) `(eqv? case-value ',d))
					datums))
			       ,(expand-progn body)
			       ,(loop (cdr clauses))))
			 (else
			  `(if (memv case-value ',datums)
			       ,(expand-progn body)
			       ,(loop (cdr clauses))))))
		     (((?datum) . ?body)
		      (if (null? body)
			  (error "case" "Illegal `case' clause" (car clauses))
			  `(if (eqv? case-value ',datum)
			       ,(expand-progn body)
			       ,(loop (cdr clauses)))))))))
      e))
