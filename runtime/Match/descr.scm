;;;--------------------------------------------------------------------*/
;;;   geffroy/Match3.0/descriptions.scm ...                            */
;;;   Author      :  Jean-Marie Geffroy                                */
;;;                  Ecole Polytechnique & INRIA Rocquencourt          */
;;;   E-mail      :  geffroy@inria.fr                                  */
;;;   Last change :  Tue Jun  8 13:58:06 1993  (geffroy)               */
;;;                                                                    */
;;;   Les fonctions de manipulation des descriptions et                */
;;;   filtres                                                          */
;;;--------------------------------------------------------------------*/

(module __match_descriptions

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

      (export  (pattern-variables f)
	    (pattern-car p)
	    (pattern-cdr p)
	    (pattern-plus p1 p2)
	    (pattern-minus p1 p2)
	    (vector-plus v i d)
	    (vector-minus v i d)
	    (compatible? p d)
	    (more-precise? d p)
	    (extend-vector v lg fill)
	    (inline isAny? c) 
	    (inline isCheck? c)
	    (inline isSuccess? c)
	    (inline isTop? c)
	    (inline isBottom? c)
	    (inline isQuote? c)
	    (inline isVar? c) 
	    (inline isNot? c) 
	    (inline isAnd? c)
	    (inline isOr? c)
	    (inline isT-Or? c)
	    (inline isTagged-Or? c) 
	    (inline isCons? c)
	    (inline isACons? c) 
	    (inline isXCons? c)
	    (inline isTimes? c)
	    (inline containsHole? c)
	    (inline isHole? c)
	    (inline isVector? c)
	    (inline isVector-begin? c)
	    (inline isVector-end? c)
	    (inline isTree? c) ) )
	    
;;;--------------------------------------------------------------------*/
;;;   Renvoie la liste des variables apparaissant dans un filtre       */
;;;--------------------------------------------------------------------*/
(define (pattern-variables f)
   (cond 
      ((eq? (car f) 'or)
       (pattern-variables (cadr f)))
      ((eq? (car f) 't-or)
       (pattern-variables (cadr f)))
      ((eq? (car f) 'and)
       (union (pattern-variables (cadr f)) (pattern-variables (caddr f))))
      ((memq (car f) '(cons vector-cons))
       (union (pattern-variables (cadr f)) (pattern-variables (caddr f))))
      ((memq (car f) '(tree times))
       (union (pattern-variables (caddr f)) (pattern-variables (cadddr f))))
      ((eq? (car f) 'var)
       (cdr f))
      ((eq? (car f) 'vector-begin)
       (pattern-variables (caddr f)) )
      ((eq? (car f) 'vector-end)
       '())
      ((eq? (car f) 'struct-pat)
       ;; MS FIX 11 april 2007.
       ;; Used to be
       ;;        (let loop ((p* (cdddr f)))
       ;; which is erroneous because f looks like:
       ;;        (struct-pat int-point int-point? (var x) (var y))
       ;; for a structure defined as:
       ;;        (define-struct int-point x y)
       (let loop ((p* (cdddr f)))
          (if (null? p*)
              '()
              (union (pattern-variables (car p*))
                     (loop (cdr p*))))))
      (else
       '())))

(define (union l1 l2)
   (if (null? l1) l2
       (if (member (car l1) l2)
	   (union (cdr l1) l2)
	   (cons (car l1) (union (cdr l1) l2)))))
   
(define (extend fn pt im)
   (lambda (x) (if (eq? x pt) im (fn x))))


;;;--------------------------------------------------------------------*/
;;;   Fonctions de mise a jour des descriptions                        */
;;;--------------------------------------------------------------------*/

;;;--------------------------------------------------------------------*/
;;;   Grammaire des descriptions:                                      */
;;;   descr ::= posD | negD | conjNegD                                 */
;;;   posD  ::= (quote e) | (var v) | (cons Descr Descr)               */
;;;             | (and (var v) Descr)
;;;   negD  ::= (not (quote e)) | (not (var v)) | (not (cons (any)(any)*/
;;;   conjNegD ::= (and negD conjNegD) | negD                          */
;;;--------------------------------------------------------------------*/

;;;--------------------------------------------------------------------*/
;;;   Intersection de deux descriptions                                */
;;;--------------------------------------------------------------------*/
(define (pattern-plus old new)
   (if (or (isTree? new) (isTimes? new))
       ; Perte d'information
       old
       (if (isAny? old)
	   new
	   (if (isAny? new)
	       old
	       (if (isNegation? old)
		   (norm-class new)
		   (norm-class (if (isVar? new)
				   (list 'and new old)
				   (list 'and old new))))))))

(define (pattern-minus p1 p2)
  (if (or (not (or (isNegation? p1) 
		   (isAny? p1)     ;;; p1 is already a non empty affirmative
		   (isBottom? p1)))
          (isTimes? p2) (isTree? p2)) ;;; we don't know how to handle p2
      ; Perte d'information
      p1
      (if (isAny? p1)
	  (list 'not p2)
	  (norm-class (list 'and p1 (list 'not p2))))))

;;; Une classe est une negation si et seulement si c'est un (not ...),
;;; ou un (and f1 f2), dont l'une des deux branches en est une. A noter que
;;; dans ce dernier cas, les deux branches sont forcement des negations, 
;;; puisqu'une affirmative annule une negation.
(define (isNegation? c)
  (or  (and (eq? (car c) 'and) 
            (isNegation? (cadr c)))
       (eq? (car c) 'not)))

(define (pattern-car c)
  (if (eq? (car c) 'cons) 
      (cadr c)
      '(any)))

(define (pattern-cdr c)
  (if (eq? (car c) 'cons) 
      (caddr c)
      '(any)))


;;;--------------------------------------------------------------------*/
;;;   Normalisation des classes                                        */
;;;   Une classe peut etre:                                            */
;;;     (any)                                                          */
;;;     (quote ...)                                                    */
;;;     (not ...)                                                      */
;;;     (and (not ...) (and (not ...) ...)) (conjonction de negations) */
;;;     (and f1 (and f2 ...)) (conjonction d'affirmatives)             */
;;;     (cons f1 f2)                                                   */
;;;     (tree/times n f1 f2)                                           */
;;;   Il faut noter qu'il n'y a pas de or, puisqu'une description      */
;;;   represente une information SURE.                                 */
;;;   Les regles utilisees sont:                                       */
;;;     (not (not c)) = c                                              */
;;;     (and (and f1 f2) f3) = (and f1 (and (f2 f3)))                  */
;;;     (and (cons f1 f2) (cons f3 f4)) = (cons (and f1 f3)(and f2 f4))*/
;;;                                                                    */
;;; Elles permettent notamment de s'assurer que toute classe           */ 
;;; representant une liste a la forme (cons ...)                       */
;;;--------------------------------------------------------------------*/

(define (norm-class c)
  (norm c 'foo))

(define (norm c anc)
  
  (cond
   ((equal? anc c) anc)
   ((eq? (car c) 'not)  (norm-not  (cadr c)           ))
   ((eq? (car c) 'and)  (norm-and  (cadr c) (caddr c) ))
   ((eq? (car c) 'cons) (norm-cons (cadr c) (caddr c) ))
   (else c)))

(define (norm-not c)
  (norm (rewrite-not c) (list 'not c)))

(define (rewrite-not c)
  (if (eq? (car c) 'not)
      ; (not (not c)) <==> c
      (cadr c) 
      (list 'not (norm-class c))))

;;; A priori, c1 et c2 sont normalisees
(define (norm-and c1 c2)
  (norm (rewrite-and c1 c2) (list 'and c1 c2)))

(define (rewrite-and c1 c2)
  (cond
   ((equal? c1 c2) c1)
   ((eq? (car c1) 'and) 
    (list 'and
          (cadr c1) 
          (list 'and 
                (caddr c1)
                c2)))
   ((and (eq? (car c1) 'cons) (eq? (car c2) 'cons)) 
     ;;; and inutile: si c1 est un cons, c2 aussi ...
    (list 'cons 
          (list 'and (cadr c1) (cadr c2)) 
          (list 'and (caddr c1) (caddr c2))))
   (else (list 'and c1 c2))))

(define (norm-cons c1 c2)
  (list 'cons (norm-class c1) (norm-class c2)))


;;;--------------------------------------------------------------------*/
;;;   Comparaison entre descriptions et filtres                        */
;;;--------------------------------------------------------------------*/
;;;   Le t-or presente une particularite par rapport au or normal,     */
;;;   c'est qu'il n'est plus precis que rien, pour respecter la        */
;;;   sequentialite des branches (cf match-case).                      */
;;;--------------------------------------------------------------------*/

;;;-- Une description est-elle plus precise qu'un filtre ? ------------*/
(define (more-precise? descr f)
   (cond
      ((isAny? descr) #f)
      
      ((eqv? (car f) 'any)
       #t)
      
      ((eqv? (car f) 'success)
       #f)
      
      ((eqv? (car f) 'quote)
       (and (isQuote? descr)
	    (equal? (cadr descr) (cadr f))))
      
      ((eqv? (car f) 'and)
       (and (more-precise? descr (cadr f))
	    (more-precise? descr (caddr f))))
      
      ((eqv? (car f) 'or)
       (or (more-precise? descr (cadr f))
	   (more-precise? descr (caddr f))))
      
      ((eqv? (car f) 't-or)
       #f)
      
      ((memq (car f) '(cons acons xcons))
       (and (isCons? descr)
	    (more-precise? (cadr descr) (cadr f))
	    (more-precise? (caddr descr) (caddr f))))

      ((eqv? (car f) 'vector-begin)
       #f) ;;;(and (isVector? descr))
	    

      (else #f)))

;;;--------------------------------------------------------------------*/
;;;   Test de compatibilite entre filtre et description.               */
;;;   Le filtre tree est similaire a (any)                             */
;;;   A noter que si le filtre est un or, ce n'est pas                 */
;;;   la peine de s'embeter. Autant attendre le filtrage des           */
;;;   alternatives, cela permet de ne comparer que des choses "sures"  */
;;;   A noter egalement que j'alpha-convertis le filtre,               */
;;;   ce qui me permet de n'utiliser qu'un seul environnement.         */
;;;   L'ordre est clairement GAUCHE-DROITE.                            */
;;;--------------------------------------------------------------------*/

;;;--------------------------------------------------------------------*/
;;;   Cette fonction est CONSERVATIVE: elle fait parfois des erreurs,  */
;;;   mais toujours dans le sens d'une perte d'information.            */
;;;--------------------------------------------------------------------*/

(define (compatible? descr pattern)
   (let ((res
;;;-- Les filtres composes --------------------------------------------*/
   (if (isAnd? pattern)
       (and (compatible? descr (cadr pattern))
	    (compatible? descr (caddr pattern)))
;;;-- Les filtres "de base" -------------------------------------------*/
       (compare descr
		(alpha-convert pattern)
		(lambda (x) 'unbound)
		(lambda (x) #t)
		(lambda (x) #f)))) )
      res))

;;;--------------------------------------------------------------------*/
;;;   A ce stade, pat ne peut pas etre Or, t-Or, success, and          */
;;;--------------------------------------------------------------------*/
;;;--------------------------------------------------------------------*/
;;;   Grammaire des descriptions:                                      */
;;;   descr ::= posD | negD | conjNegD                                 */
;;;   posD  ::= (quote e) | (var v) | (cons Descr Descr)               */
;;;             | (and (var v) Descr)
;;;   negD  ::= (not (quote e)) | (not (var v)) | (not (cons (any)(any)*/
;;;   conjNegD ::= (and negD conjNegD) | negD                          */
;;;--------------------------------------------------------------------*/
;;;   Le cas ou l'un ou l'autre est une variable est traite ici        */
;;;--------------------------------------------------------------------*/

(define (compare descr pat env k z)
   
   (cond
      ((or (isAny? descr)
	   (isAny? pat)
	   (isOr? pat) (isT-Or? pat) (isTagged-Or? pat)
	   (isSuccess? pat)
	   (isCheck? pat)
	   (isTimes? pat)
	   (isTree? pat))
       (k env))

      ((isAnd? pat)
       (compare descr (cadr pat) env
		(lambda (env) (compare descr (caddr pat) env k z))
		z))

      ((isCons? pat)
       (if (may-be-a-cons descr)
	   (compare (pattern-car descr) (cadr pat) env
		    (lambda (env)
		       (compare (pattern-cdr descr)
				(caddr pat) env k z) )
		    z)
	   (z env)))

;;;-- Si le filtre est une valeur, il suffit de la filtrer par --------*/
;;;-- la description... non ? -----------------------------------------*/
      ((isQuote? pat)
       (match descr (cadr pat) env k z))

      ((and (isVar? descr) (isVar? pat))
       (if (eq? (env (cadr descr)) 'unbound)
	   (if (eq? (env (cadr pat)) 'unbound)
	       (let ((s (list 'var (jim-gensym "VAR-")))) ;;; johnm
		  (k (extend (extend env (cadr descr) s)
			     (cadr pat)
			     s)))
	       (k (extend env (cadr descr) (env (cadr pat)))))
       	   (if (eq? (env (cadr pat)) 'unbound)
	       (k (extend env (cadr pat) (env (cadr descr))))
	       (compare (env (cadr descr)) (env (cadr pat)) env k z))))      

      ((isVar? pat)
       (if (eq? (env (cadr pat)) 'unbound)
	   (k (extend env (cadr pat) descr))
	   (compare descr (env (cadr pat)) env k z)) )

      ((isVar? descr)
       (if (eq? (env (cadr descr)) 'unbound)
	   (k (extend env (cadr descr) pat))
	   (compare descr (env (cadr descr)) env k z)) )

      ((isNot? pat) (if (more-precise? (cadr pat) descr)
			(z env)
			(k env) ) )

      ((isVector-begin? pat)
       (if (isAny? descr)
	   #t
	   (if (isVector? descr)
	       ; la, je matche la descr. avec le pattern,
	       ; pcq + facile (la descr. est 1 vecteur)
	       (match pat descr env k z)
	       #f)))

      (else (k env) ) ) )

(define (may-be-a-cons descr)
   (if (equal? descr '(not (cons (any) (any))))
       #f
       (if (isAnd? descr)
	   (and (may-be-a-cons (cadr descr))
		(may-be-a-cons (caddr descr)))
	   #t) ) )


;;;--------------------------------------------------------------------*/
;;;   Filtrage d'une expression par une DESCRIPION                     */
;;;--------------------------------------------------------------------*/
(define (match d e env k z)
   (case (car d)
      ((any)   (k env))
      
      ((quote) (if (or (eq? e (cadr d)) (and (string? e) (string=? e (cadr d))))
		   (k env) (z env)))

      ((and)   (match (cadr d) e env
		      (lambda (env)
			 (match (caddr d) e env k z))
		      z) )

      ((cons)  (if (pair? e)
		   (match (cadr d) (car e) env
			  (lambda (env)
			     (match (caddr d) (cdr e) env k z))
			  z)
		   (z env) ) )

;;;-- Une regle particuliere pcq on peut avoir des (not (var x)) ------*/
;;;-- avec x non lie. -------------------------------------------------*/
      ((not)
       (if (isVar? (cadr d))
	   (let ((s (jim-gensym "VAR-")))
	      (k (extend env (cadadr d) `(not (quote ,s))))) ;;; johnm
	   (match (cadr d) e env z k)))

;;;-- Le cas ou la descr. est un vector et le filtre un vector-begin --*/
;;;-- Attention: d est ici le filtre ----------------------------------*/
      ((vector-begin)
       ((match (caddr d) (caddr e) env k z) 0))

      ((vector-cons)
       (lambda (i)
	  (if (>=fx i (vector-length e))
	      (k env)
	      (compare (cadr d) (vector-ref e i)
		       env
		       (lambda (env)
			  ((match (caddr d) e env k z)
			   (+fx i 1)))
		       z ))))
      
      ((vector-end)
       (lambda (i) (k env)))

      ((var) (if (eq? (env (cadr d)) 'unbound)
		 (k (extend env (cadr d) e))
		 (if (eq? (env (cadr d)) e)
		     (k env)
		     (z env)))) ) )


;;;--------------------------------------------------------------------*/
;;;   Alpha-conversion d'un filtre                                     */
;;;--------------------------------------------------------------------*/
(define (alpha-convert f)
   (let loop ( (f f)
	       (env (lambda (x) 'unbound))
	       (k (lambda (f e) f)) )
      (cond
	 ((not (pair? f))
	  (k f env))
	 ((equal? (car f) 'quote)
	  (k f env))
	 ((eq? (car f) 'var)
	  (if (eq? (env (cadr f)) 'unbound)
	      (let ((s (jim-gensym "ALPHA-")))
		 (k (list 'var s) (extend env (cadr f) s)))
	      (k (list 'var (env (cadr f))) env)))
	 (else (loop (car f) env
		     (lambda (fcar e)
			(loop (cdr f) e
			      (lambda (fcdr e)
				 (k (cons fcar fcdr) e)))))))))

;;;--------------------------------------------------------------------*/
;;;   Les inlines                                                      */
;;;--------------------------------------------------------------------*/
(define-inline (isAny? c) (if (eq? (car c) 'any)
			      #t
			      (eq? (car c) 'check)))

(define-inline (isCheck? c) (eq? (car c) 'check))

(define-inline (isSuccess? c) (eq? (car c) 'success))

(define-inline (isTop? c) (eq? (car c) 'top))

(define-inline (isBottom? c) (eq? (car c) 'bottom))

(define-inline (isQuote? c) (eq? (car c) 'quote))

(define-inline (isVar? c) (eq? (car c) 'var))

(define-inline (isNot? c) (eq? (car c) 'not))

(define-inline (isAnd? c) (eq? (car c) 'and))

(define-inline (isOr? c) (eq? (car c) 'or))

(define-inline (isT-Or? c) (eq? (car c) 't-or))

(define-inline (isTagged-Or? c) (eq? (car c) 'tagged-or))

(define-inline (isCons? c) (eq? (car c) 'cons))

(define-inline (isACons? c) (eq? (car c) 'acons))

(define-inline (isXCons? c) (eq? (car c) 'xcons))

(define-inline (isTimes? c) (eq? (car c) 'times))

(define-inline (containsHole? c) (eq? (car c) 'hole))

(define-inline (isHole? c) (eq? (car c) 'hole))

(define-inline (isTree? c) (eq? (car c) 'tree))

(define-inline (isVector? c) (eq? (car c) 'vector))

(define-inline (isVector-begin? c) (eq? (car c) 'vector-begin))

(define-inline (isVector-end? c) (eq? (car c) 'vector-end))

;;;--------------------------------------------------------------------*/
;;;   (Vector-Plus v i d) updates the ith element of the description   */
;;;   with d.                                                          */
;;;   The caddr of the descr can be too short. We must be able         */
;;;   to extend it.                                                    */
;;;--------------------------------------------------------------------*/

(define (vector-plus v i d)
   (if (>=fx i (vector-length (caddr v)))
       (set-car! (cddr v)
		 (extend-vector (caddr v) i '(any)))
       #t)
   (let ((res `(vector ,(vector-length (caddr v))
		       ,(list->vector (vector->list (caddr v))))))
      (vector-set! (caddr res) i
		   (pattern-plus (vector-ref (caddr v) i) d))
      res))

(define (vector-minus v i d)
   (if (>=fx i (vector-length (caddr v)))
       (set-car! (cddr v)
		 (extend-vector (caddr v) i '(any)))
       #t)
   (let ((res `(vector ,(length (caddr v))
		       ,(list->vector (vector->list (caddr v))))))
      (vector-set! (caddr res) i
		   (pattern-minus (vector-ref (caddr v) i) d))
      res))

;;;--------------------------------------------------------------------*/
;;;   Extend-vector allocates a new longer vector and fills            */
;;;   its first elements with the ones of v                            */
;;;--------------------------------------------------------------------*/
(define (extend-vector v lg fill)
   (let ((res
   (let ((new-vector (make-vector lg fill)))
      (let loop ((i 0))
	 (if (=fx i (vector-length v))
	     new-vector
	     (begin
		(vector-set! new-vector i (vector-ref v i))
		(loop (+fx i 1))))))))
      res))
   
;;;--------------------------------------------------------------------*/
;;;   End of file...                                                   */
;;;--------------------------------------------------------------------*/
