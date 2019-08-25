
(module __match_normalize

   (import  __error
	    __match_s2cfun
	    __param
	    __bexit
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __tvector
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
	    __r4_output_6_10_3
	    __evenv)
   
   (export  (normalize-pattern pat)
	    (match-define-structure! exp)
	    (match-define-record-type! exp)
	    (extend-r-macro-env name fun)))
 
;;;===========================================================4
;;; The standardizer converts patterns with an 
;;; extended syntax into pattern within the reduced pattern set.
;;; The extended language use the following approximate grammar:
;;; pat     := ( patlist ) 
;;;          | ?- | ?x | <constant>
;;; patlist := ( pat . patlist ) | <nothing> | pat ...
;;;          | ??- | ??x | ???- | ???x
;;; As it stands it is not very convenient but a good syntax, with
;;; the simplicity of the backquote facility, has yet to be invented.
;;; We nevertheless offer the three-dots convention of extend-syntax
;;; which meaning is a sequence of the preceding pattern.

;;; You can also define your own macro-patterns which are expanded before
;;; being used (see defmacro-pattern below).
;;; To define macro-pattern use the following macro: variables
;;; will be bound to the arguments of the pattern (see examples below).
;;; A macro-pattern is simply rewritten into another pattern.

;;; Extended on March 10, to recognize patterns such as:
;;; 
;;; (!1 (f x) (or (^1 a) (a ^1)))
;;; (!1 a (a ?- (!2 b (d ^1 ^2) (c ^2 ^1))))

;;;===============================================================5
;;; Standardization of patterns (very weak for now)
;;; Usual patterns such as ?x, ?-, ??y, ??-, ???x or ???- are
;;; represented as symbols. Other choices may be taken such as
;;; making ? a macro-character.

(define (term-variable? e)
  (and (symbol? e)
       (> (string-length (symbol->string! e)) 1)
       (char=? (string-ref (symbol->string! e) 0) #\?) ) )

(define (segment-variable? e)
  (and (symbol? e)
       (> (string-length (symbol->string! e)) 2)
       (char=? (string-ref (symbol->string! e) 0) #\?)
       (char=? (string-ref (symbol->string! e) 1) #\?) ) )

(define (lispish-segment-variable? e)
  (and (symbol? e)
       (> (string-length (symbol->string! e)) 3)
       (char=? (string-ref (symbol->string! e) 0) #\?)
       (char=? (string-ref (symbol->string! e) 1) #\?)
       (char=? (string-ref (symbol->string! e) 2) #\?) ) )

(define (tree-variable? e)
  (and (symbol? e)
       (> (string-length (symbol->string! e)) 1)
       (char=? (string-ref (symbol->string! e) 0) #\!) ) )

(define (hole-variable? e)
  (and (symbol? e)
       (> (string-length (symbol->string! e)) 1)
       (char=? (string-ref (symbol->string! e) 0) #\^) ) )

(define (term-variable-true-name e)
  (let ((s (symbol->string! e)))
    (string->symbol (substring s 1 (string-length s))) ) )

(define (segment-variable-true-name e)
  (let ((s (symbol->string! e)))
    (string->symbol (substring s 2 (string-length s))) ) )

(define (tree-variable-true-name e)
  (let ((s (symbol->string! e)))
    (string->symbol (substring s 1 (string-length s))) ) )

(define (hole-variable-true-name e)
  (let ((s (symbol->string! e)))
    (string->symbol (substring s 1 (string-length s))) ) )

(define (lispish-segment-variable-true-name e)
  (let ((s (symbol->string! e)))
    (string->symbol (substring s 3 (string-length s))) ) )

;;;===============================================================6
;;; The normalization of the pattern extended syntax.

(define (normalize-pattern e)
  ((standardize-pattern e) 
   r-macro-pattern
   (lambda (pattern rr) pattern) ) )

;(define (standardize-pattern e)
;  (match-case e
;    ( (check macro-pattern?) (standardize-macro-pattern e) )
;    ( (quote ?-)  (standardize-sexp) )
;    ( (check term-variable?) (standardize-term-variable e) )
;    ( (check atom?) (standardize-quote e) )
;    ( (any) (standardize-patterns e) ) ) )

(define (standardize-pattern e)
  (cond ((macro-pattern? e) => (lambda (p) (standardize-macro-pattern e p)))
        ((eq? e '?-) (standardize-sexp))
        ((term-variable? e) (standardize-term-variable e))
        ((hole-variable? e) (standardize-hole-variable e))
        ((vector? e) (standardize-vector e))
        ((struct? e) (standardize-struct e))
        ((atom? e) (standardize-quote e))
        (else (standardize-patterns e)) ) )

;(define (standardize-patterns e*)
;  (match-case (car e*)
;    ( (quote ??-) (standardize-any (cdr e*)) )
;    ( (check segment-variable?) 
;      (standardize-segment-variable (car e*) (cdr e*)) )
;    ( (any)
;      (standardize-cons (car e*) (cdr e*)) ) ) )

(define (standardize-patterns e*)
  (if (pair? e*)
      (cond ((macro-pattern? e*) => (lambda (p) (standardize-macro-pattern e* p)))
	    ((eq? (car e*) '??-) (standardize-any (cdr e*)))
	    ((eq? (car e*) '???-) (standardize-lispish-any (cdr e*)))
	    ((lispish-segment-variable? (car e*))
	     (standardize-lispish-segment-variable (car e*) (cdr e*)) )
	    ((segment-variable? (car e*))
	     (standardize-segment-variable (car e*) (cdr e*)) )
	    ((tree-variable? (car e*))
	     (standardize-tree-variable (car e*) (cadr e*) (caddr e*)) )
	    (else (standardize-cons (car e*) (cdr e*))) )
      (standardize-quote e*) ) )

(define (standardize-repetition e e*)
  (lambda (r c) 
    ((standardize-pattern e)
     r
     (lambda (f rr)
       ((standardize-patterns e*)
        rr
        (lambda (f* rrr)
          (let ((label (jim-gensym "g")))
	     (c `(times ,label
			(cons ,f (hole ,label ,(jim-gensym "HOLE-")))
			 ,f* )
                  rrr ) ) ) ) ) ) ) )

(define (standardize-sexp)
  (lambda (r c)
    (c `(any) r) ) )

(define (standardize-cons f f*)
  (if (and (pair? f*) (eq? (car f*) '...))
      (standardize-repetition f (cdr f*))
      (if (*prefer-xcons* 'value)
	  (standardize-real-xcons f f*)
	  (standardize-real-cons f f*) ) ) )

(define (make-toggle)
  (let ((value #f))
    (lambda (msg)
      (case msg
	((value) value)
	((on) (set! value #t))
	((off) (set! value #f))))))

(define *prefer-xcons* (make-toggle))

(define (standardize-real-cons f f*)
  (lambda (r c)
    ((standardize-pattern f)
     r
     (lambda (pattern1 rr)
;;;       ((standardize-patterns f*)
       ((standardize-pattern f*)
        rr
        (lambda (pattern2 rrr)
          (c `(cons ,pattern1 ,pattern2) rrr) ) ) ) ) ) )
 
(define (standardize-real-xcons f f*)
  (lambda (r c)
    ((standardize-patterns f*)
     r
     (lambda (pattern1 rr)
       ((standardize-pattern f)
        rr
        (lambda (pattern2 rrr)
          (c `(xcons ,pattern2 ,pattern1) rrr) ) ) ) ) ) )

(define (standardize-term-variable e)
  (lambda (r c)
     (let ((name (term-variable-true-name e)))
	(c `(var ,name) r) ) ) )

;(define (standardize-term-variable e)
;  (lambda (r c)
;    (let ((name (term-variable-true-name e)))
;      (if (eq? (lookup r name) unbound-pattern)
;          (c `(ref ,name (any)) 
;             (extend-alist r name 'term) )
;          (c `(ref ,name) r) ) ) ) )

(define (standardize-hole-variable e)
  (lambda (r c)
    (let ((name (hole-variable-true-name e)))
          (c `(hole ,name ,(jim-gensym "HOLE-")) r) ) ) )

(define (standardize-quote e)
  (lambda (r c)
    (c `(quote ,e) r) ) )

(define (standardize-segment-variable e f*)
  (lambda (r c)
    (let ((name (segment-variable-true-name e)))
      (if (eq? (lookup r name) unbound-pattern)
          ((standardize-patterns f*)
           (extend-alist r name 'segment)
           (lambda (pattern rr)
             (let ((label (jim-gensym "g")))
               (c `(ssetq-append 
                    ,name 
                    (tree ,label
			  (cons (any) (hole ,label ,(jim-gensym "HOLE-")))
			  (end-ssetq ,name) )
		    ,pattern )
                  rr ) ) ) )
          ((standardize-patterns f*)
           r
           (lambda (pattern rr)
             (c `(eval-append ,name ,pattern) rr) ) ) ) ) ) )

(define (standardize-tree-variable e f1 f2)
  (lambda (r c)
    (let ((name (tree-variable-true-name e)))
      ((standardize-pattern f1)
       (extend-alist r name 'tree)
       (lambda (hole-pattern rr)
	 ((standardize-pattern f2)
	  rr
	  (lambda (patterns rrr)
	     (if (> (oc-count name patterns) 1)
		 (c `(tree ,name ,patterns ,hole-pattern) rrr)
		 (c `(times ,name ,patterns ,hole-pattern) rrr))) ) ) ) ) ) )

(define (oc-count name pattern)
   (cond
      ((null? pattern) 0)
      ((eq? (car pattern) 'hole)
       (if (eq? (cadr pattern) name)
	   1
	   0))
      ((memq (car pattern) '(or and t-or tagged-or cons not))
       (apply + (map (lambda (pat) (oc-count name pat)) (cdr pattern))))
      (else 0)))

(define (standardize-lispish-segment-variable e f*)
  (if (null? f*)
      (lambda (r c)
        (let ((name (lispish-segment-variable-true-name e)))
          (if (eq? (lookup r name) unbound-pattern)
              (c `(var ,name (any))
                 (extend-alist r name 'segment) )
              (c `(var ,name) r) ) ) )
      (standardize-segment-variable e f*) ) )

(define (standardize-any f*)
  (lambda (r c)
    ((standardize-patterns f*)
     r
     (lambda (pattern rr)
       (let ((label (jim-gensym "g")))
	 (if (*prefer-xcons* 'value)
	     (c `(times ,label
			(xcons (any) (hole ,label ,(jim-gensym "HOLE-")))
			,pattern )
		rr ) 
	     (c `(times ,label
			(cons (any) (hole ,label ,(jim-gensym "HOLE-")))
			,pattern )
		rr ) ) ) ) ) ) )

(define (standardize-lispish-any f*)
  (if (null? f*)
      (lambda (r c) (c `(any) r))
      (standardize-any f*) ) )

(define (standardize-macro-pattern e p)
  (apply p (cdr e)) )

;;;--------------------------------------------------------------------*/
;;;   Macro-patterns                                                   */
;;;--------------------------------------------------------------------*/

;;; The environment binding name to macro-pattern
(define r-macro-pattern-init '())

(define r-macro-pattern r-macro-pattern-init)

(define (extend-alist fn pt im)
  (cons (cons pt im) fn) )

(define (lookup r n)
   (if (assq n r)
       (cdr (assq n r))
       #f))

(define (extend-r-macro-env name fun)
   (set! r-macro-pattern
	 (extend-alist r-macro-pattern
		       name
		       fun)))

(define-macro (defmacro-pattern name variables body)
  `(begin
;*     (set! r-macro-pattern  */
;*          (extend-alist r-macro-pattern  */
;* 		       ',name  */
;* 		       (lambda ,variables ,body) ) )  */
      (extend-r-macro-env ',name (lambda ,variables ,body))
    ',name ) )

(define (macro-pattern? e)
  (and (pair? e)
       (lookup r-macro-pattern (car e)) ) )

(defmacro-pattern atom l
   (if (null? l)
       (match-wrong "Illegal `atom' form" l)
       (let ((e (car l))
	     (e* (cdr l)))
	  (lambda (r c)
	     (if (pair? e*)
		 (match-wrong "Too many patterns provided for atom" e*)
		 ((standardize-pattern e)
		  r
		  (lambda (pattern rr)
		     (c `(and (not (cons (any) (any)))
			      ,pattern)
			rr))))))))

(defmacro-pattern or l
   (if (null? l)
       (match-wrong "Illegal `or' form" l)
       (let ((e (car l))
	     (e* (cdr l)))
	  (lambda (r c)
	     (if (pair? e*)
		 ((standardize-pattern e)
		  r
		  (lambda (pattern1 rr)
		     ((standardize-pattern `(or . ,e*))
		      r
		      (lambda (pattern2 rrr)
			 (if (and (coherent-environment? rr rrr)
				  (coherent-environment? rrr rr))
			     (c `(or ,pattern1 ,pattern2) rrr)
			     (match-wrong "Incompatible alternative" l))))))
		 ((standardize-pattern e) r c))))))

;;; (defmacro-pattern tagged-or (e l . e*)  */
;;;    (lambda (r c)  */
;;; 	     (if (pair? e*)  */
;;; 		 ((standardize-pattern e)  */
;;; 		  r  */
;;; 		  (lambda (pattern1 rr)  */
;;; 		     ((standardize-pattern `(tagged-or . ,e*))  */
;;; 		      r  */
;;; 		      (lambda (pattern2 rrr)  */
;;; 			 (if (and (coherent-environment? rr rrr)  */
;;; 				  (coherent-environment? rrr rr))  */
;;; 			     (c `(tagged-or ,pattern1 ,l ,pattern2) rrr)  */
;;; 			     (match-wrong "Incompatible alternative"))))))  */
;;; 		 ((standardize-pattern e) r c) ) ) )  */

(defmacro-pattern t-or l
   (if (null? l)
       (match-wrong "Illegal `t-or' form" l)
       (let ((e (car l))
	     (e* (cdr l)))
	  (lambda (r c)
	     (if (pair? e*)
		 ((standardize-pattern e)
		  r
		  (lambda (pattern1 rr)
		     ((standardize-pattern `(t-or . ,e*))
		      r
		      (lambda (pattern2 rrr)
			 (if (and (coherent-environment? rr rrr)
				  (coherent-environment? rrr rr))
			     (c `(t-or ,pattern1 ,pattern2) rrr)
			     (match-wrong "Incompatible alternative" l))))))
		 ((standardize-pattern e) r c) ) ) ) ) )

(defmacro-pattern and l
   (if (null? l)
       (match-wrong "Illegal `and' form" l)
       (let ((e (car l))
	     (e* (cdr l)))
	  (lambda (r c)
	     (if (pair? e*)
		 ((standardize-pattern e)
		  r
		  (lambda (pattern1 rr)
		     ((standardize-pattern `(and . ,e*))
		      rr
		      (lambda (pattern2 rrr)
			 (c `(and ,pattern1 ,pattern2) rrr) ) ) ) )
		 ((standardize-pattern e) r c))))))

(defmacro-pattern not l
   (if (or (null? l) (pair? (cdr l)))
       (match-wrong "Illegal `not'" l)
       (let ((e (car l)))
	  (lambda (r c)
	     ((standardize-pattern e)  
	      r
	      (lambda (pattern rr)
		 (c `(not ,pattern) r) ) ) ) ) ) )

(defmacro-pattern ? l
   (if (or (null? l) (pair? (cdr l)))
       (match-wrong "Illegal `?' form" l)
       (lambda (r c)
	  (c `(check ,(car l)) r))))

(defmacro-pattern kwote l
   (if (or (null? l) (pair? (cdr l)))
       (match-wrong "Illegal `kwote' form" l)
       (lambda (r c)
	  (c `(quote ,(car l)) r))))

;;; check coherency between arms of alternative patterns:
;;; For instance (match-lambda (or ?x ?y) t) is not coherent
;;; while (match-lambda (or (?x ?y) (?y ?x)) t) is coherent.
(define (coherent-environment? r rr)
  (labels ((look (n r)
		 (and (pair? r)
		      (or (eq? (caar r) n)
			  (look n (cdr r)) ) ) ))
    (if (pair? r)
        (and (look (caar r) rr)
             (coherent-environment? (cdr r) rr) )
        #t ) ) )

;;;===============================================================8
;;; report an error (implementation dependent)

(define unbound-pattern '**Bad-Luck096561123523452**)

(define (match-wrong msg arg)
  (error 'Pattern-Matching msg arg))

(define (r9-init n)
  unbound-pattern)

(define n normalize-pattern)


;;;--------------------------------------------------------------------*/
;;;   Extension to vectors (J.M. Geffroy)                              */
;;;--------------------------------------------------------------------*/
(define (standardize-vector e)
   ; e is known to be a vector
   (let ((tmp (normalize-pattern (vector->list e))))
      (labels ((vectorify
		(p)
		(cond
		   ((eq? (car p) 'cons)
		    `(vector-cons ,(cadr p)
				  ,(if (equal? (caddr p) '(any))
				       '(vector-any)
				       (vectorify (caddr p)))))
		   ((equal? p '(quote ()))
		    '(vector-end))
		   ((memq (car p) '(and or not))
		    (list (car p)
			  (vectorify (cadr p))
			  (vectorify (caddr p))))
		   ((memq (car p) '(times tree))
		    (list 'vector-times (cadr p)
			  (vectorify (caddr p))
			  (vectorify (cadddr p))))
		   (else p))))
	 (lambda (r c)
	    (c `(vector-begin ,(pattern-length (vector->list e))
			      ,(vectorify tmp)) r) ) ) ) )

(define (pattern-length p)
   (cond
      ((atom? p) 0)
      ((null? p) 0)
      ((eq? (car p) 'not) 1)
      ((tree-variable? (car p)) 0)
      ((memq (car p) '(??- ???-)) 0)
      ((memq (car p) '(or and t-or tagged-or)) (pattern-length (cadr p)))
      (else (+ 1 (pattern-length (cdr p))))))
      
;*---------------------------------------------------------------------*/
;*    XXX-match-define-structure! updates a global environment         */
;*    mapping structure names to their lists of fields:                */
;*    ((2D-point x y) (3D-point x y z))                                */
;*                                                                     */
;*    Modified by Will M. Farr 28 Sept 2006 to support pattern         */
;*    matching on define-record-type structures.  This requires        */
;*    changing the *Match-Structures* environment to store the         */
;*    predicate in addition to the structure name (because the         */
;*    define-record-type from allows the user to specify the predicate */
;*    name): ((2D-point 2D-point? x y) (3D-point 3D-point? x y z)).    */
;*---------------------------------------------------------------------*/
(define *Match-Structures* '())

(define (match-define-structure! exp)
   (match-case exp
      ((define-struct ?name . ?fields)
       (set! *Match-Structures*
             (cons `(,name ,(symbol-append name '?) ,@fields)
		   *Match-Structures*)))
      (else (error "Incorrect declaration: " exp 'Aborted))))

(define (match-define-record-type! exp)
  (match-case exp
    ((define-record-type ?name ?constr ?pred . ?fields)
     (let ((really-fields (map car fields)))
       (set! *Match-Structures*
	     (cons `(,name ,pred ,@fields)
		   *Match-Structures*))))
    (else (error "Incorrect declaration: " exp 'Aborted))))

(defmacro-pattern struct-pat (name pred . e*)
   (lambda (r c)
      (c `(struct-pat ,name ,pred ,@(map normalize-pattern e*)) r)))

(define (standardize-struct e)
   (lambda (r c)
      (define (look-for-structure provided-fields)
         (let loop1 ((S *Match-Structures*))
            (let loop2 ((p-f provided-fields))
               (if (null? S)
                   (error "No such structure: " provided-fields '())
                   (if (null? p-f)
                       (car S)
                       (if (memq (car p-f) (cdar S))
                           (loop2 (cdr p-f))
                           (loop1 (cdr S))))))))
      (let* (;; On recupere le nom et la liste des champs
             ;; dans le bon ordre
	     (f (struct->list e))
             (structure (if (pair? (car f))
                            (look-for-structure (map car (cdr f)))
                            (if (assoc (car f) *Match-Structures*)
                                (assoc (car f) *Match-Structures*)
                                (error 'match-case
				       "No such structure "
				       (car f)))))
             (name (car structure))
	     (pred (cadr structure))
             (fields (cddr structure))
             (provided-fields (if (pair? (car f))
                                  f
                                  (cdr f)))
             ;; Les champs fournis
             ;; Attention: il
             ;; faudrait filtrer la valeur qu'on fournit
             ;; a l'aide du filtre specifie lors de la
             ;; declaration de la structure...
             (pattern
              `(struct-pat
                ,name
		,pred
                ,@(if (pair? (car f))
                      (map (lambda (field)
                              (if (assoc field provided-fields)
                                  (cadr (assoc field provided-fields))
                                  '?-))
                           fields)
                      (cdr f)))))
         ((standardize-pattern pattern) r c))))
