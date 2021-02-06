;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Eval/expdstruct.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 30 11:48:02 1992                          */
;*    Last change :  Sun Aug 25 09:14:36 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Structure expansion                                              */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/struct.texi@                              */
;*       @node Structures@                                             */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __expander_struct
   
   (import  __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __param
	    __object
	    __thread
	    
	    __match_normalize
	     
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
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __progn
	    __expand)
   
   (use     __type
	    __evenv
	    __bit)
   
   (export  (expand-eval-define-struct x e)))

;*---------------------------------------------------------------------*/
;*    expand-eval-define-struct ...                                    */
;*---------------------------------------------------------------------*/
(define (expand-eval-define-struct x e)
   (match-case x
      ((?- ?name . ?slots)
       (match-define-structure! x)
       (let* ((len        (length slots))
	      (slots-name (map (lambda (s)
				  (match-case s
				     ((?name ?dv)
				      name)
				     ((? symbol?)
				      s)
				     (else
				      (expand-error "define-struct"
					 "Illegal `define-struct' form"
					 x))))
			       slots))
	      (slots-val?  #f)
	      (slots-val   (map (lambda (s)
				   (match-case s
				      ((?- ?dv)
				       (set! slots-val? #t)
				       dv)
				      ((? symbol?)
				       ''())
				      (else
				       (expand-error "define-struct"
					  "Illegal `define-struct' form"
					  x))))
				slots)))
          (cons
           'begin
           (cons
            ;; on genere l'inline make-???
            (e (evepairify
		`(define-inline (,(symbol-append 'make- name) . init)
		    ,(if slots-val?
			 `(if (pair? init)
			      (if (not (null? (cdr init)))
				  (apply ,name init)
				  (make-struct ',name ,len (car init)))
			      (,name ,@slots-val))
			 `(if (pair? init)
			      (if (not (null? (cdr init)))
				  (apply ,name init)
				  (make-struct ',name ,len (car init)))
			      (make-struct ',name ,len '()))))
		x)
	       e)
            (cons
             (e (evepairify
		 `(define-inline (,name ,@slots-name)
		     (let ((new (make-struct ',name ,len '())))
			,@(let loop ((slots slots-name)
				     (res   '()))
			     (if (null? slots)
				 res
				 (loop (cdr slots)
				       (cons `(,(symbol-append name
							       '-
							       (car slots)
							       '-set!)
					       new
					       ,(car slots))
					     res))))
			new))
		 x)
		e)
             (cons
              ;; on genere le predicat STRUCT?
              (e (evepairify
		  `(define-inline (,(symbol-append name '?) o)
		      (if (struct? o)
			  (eq? (struct-key o) ',name)
			  #f))
		  x)
                 e)
              ;; on genere les fonctions d'access aux slots
              (let loop ((i     0)
                         (slots slots-name)
                         (res   '((unspecified))))
                 (if (=fx i len)
                     res
                     (let ((pr (car slots)))
                        (loop (+fx i 1)
                              (cdr slots)
                              (cons
                               ;; la lecture
                               (e (evepairify
				   `(define-inline
				       (,(symbol-append name '- pr) s)
				       (if (,(symbol-append name '?) s)
					   (struct-ref s ,i)
					   (expand-error
					      "struct-ref:not an instance of"
					      ,(symbol->string name)
					      s)))
				   x)
                                  e)
                               (cons
                                ;; l'ecriture
                                (e (evepairify
				    `(define-inline
					(,(symbol-append name '- pr '-set!) s v)
					(if (,(symbol-append name '?) s)
					    (struct-set! s ,i v)
					    (expand-error
					     "struct-set!:not an instance of"
					     ,(symbol->string name)
					     s)))
				    x)
                                   e)
                                res))))))))))))
      (else
       (expand-error "define-struct" "Illegal `define-struct' form" x))))
 
