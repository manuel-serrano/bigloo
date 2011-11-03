;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/filtre.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 12 16:13:39 1992                          */
;*    Last change :  Thu Nov  3 14:18:53 2011 (serrano)                */
;*                                                                     */
;*    Des tests de globalisation d'apres Christian.                    */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module filtre
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-filtre)))

;*---------------------------------------------------------------------*/
;*    L'interprete                                                     */
;*---------------------------------------------------------------------*/
(define (wrong m1 m2)
   (error "filtre" m1 m2)
   #f)

(define (match f e)
  ((m-g f) e r-init a-init m-init 
		     (lambda (e r z) #t)
		     (lambda () #f)))

(define (m-g f)
  (case (car f)
    ((*sexp) (m-sexp-g))
    ((*quote) (m-quote-g (cadr f)))
    ((*or) (m-or-g (cadr f) (caddr f)))
    ((*and) (m-and-g (cadr f) (caddr f)))
    ((*not) (m-not-g (cadr f)))
    ((*setq) (m-setq-g (cadr f) (caddr f)))
    ((*eval) (m-eval-g (cadr f)))
    ((*cons) (m-cons-g (cadr f) (caddr f)))
    ((*ssetq-append) (m-ssetq-append-g (cadr f) (caddr f) (cadddr f)))
    ((*eval-append) (m-eval-append-g (cadr f) (caddr f)))
    ((*end-ssetq) (m-end-ssetq-g (cadr f)))
    ((*times) (m-times-g (cadr f) (caddr f) (cadddr f)))
    ((*end-times) (m-end-times-g (cadr f)))
    (else (wrong "Unrecognized pattern" f)) ) )

(define (m-sexp-g)
  (lambda (e r a m k z) (k e r z)) )

(define (m-quote-g ee)
  (lambda (e r a m k z) 
    (if (eq? e ee)
        (k e r z)
        (z) ) ) )

(define (m-or-g f1 f2)
  (lambda (e r a m k z)
    ((m-g f1)
     e r a m k (lambda () 
                 ((m-g f2) 
                  e r a m k z ) ) ) ) )

(define (m-and-g f1 f2)
  (lambda (e r a m k z)
    ((m-g f1)
     e r a m (lambda (ee rr zz)
               ((m-g f2)
                e rr a m k zz ) )
     z ) ) )

(define (m-not-g f)
  (lambda (e r a m k z)
    ((m-g f)
     e r a m (lambda (ee rr zz) (z)) 
             (lambda () (k e r z)) ) ) )

(define (m-setq-g n f)
  (lambda (e r a m k z)
    ((m-g f)
     e r a m (lambda (ee rr zz)
               (if (eq? (rr n) unbound-pattern)
                   (k e (extend rr n e) zz)
                   (wrong "Cannot rebind pattern" n) ) )
     z ) ) )

(define (m-eval-g n)
  (lambda (e r a m k z)
    (if (eq? (r n) unbound-pattern)
        (wrong "Unbound pattern" n)
        (if (eq? (r n) e)
            (k e r z)
            (z) ) ) ) )

(define (m-cons-g f1 f2)
  (lambda (e r a m k z)
    (if (pair? e)
        ((m-g f1)
         (car e) r a-init m-init 
         (lambda (ee rr zz)
           ((m-g f2)
            (cdr e) rr a m k zz ) )
         z )
        (z) ) ) )

(define (m-ssetq-append-g n f1 f2)
  (lambda (e r a m k z)
    ((m-g f1)
     e r (extend a-init n 
                 (lambda (ee rr zz)
                    (if (eq? (rr n) unbound-pattern)
                        ((m-g f2)
                         ee (extend rr n (cut e ee)) a m k zz ) 
                        (wrong "cannot rebind" n) ) ) )
     m-init (lambda (ee rr zz)
              (wrong "Ssetq not ended" f1) )
     z ) ) )

(define (m-eval-append-g n f)
  (lambda (e r a m k z)
    (if (eq? (r n) unbound-pattern)
        (wrong "Unbound segment" n)
        (check e (r n) (lambda (ee)
                         ((m-g f)
                          ee r a m k z ) )
                       z ) ) ) )

(define (m-end-ssetq-g n)
  (lambda (e r a m k z)
    ((a n) e r z) ) )

;;; corrected version thanks to ML
(define (m-times-g n f1 f2)
  (lambda (e r a m k z)
     (labels ((ltry (e r z)
		   ((m-g f2)
		    e r a m k
		    (lambda () 
                       ((m-g f1)
                        e r a-init 
			(extend m-init n ltry)
			(lambda (ee rr zz)
			   (wrong "Times not ended" f1) )
			z ) ) ) ))
	(ltry e r z))))

(define (m-end-times-g n)
  (lambda (e r a m k z)
    ((m n) e r z) ) )

(define (a-init n)
  (lambda (e r z)
    (wrong "No current ssetq for" n) ) )

(define (m-init n)
  (lambda (e r z)
    (wrong "No current repetition named" n) ) )

(define (r-init n)
  unbound-pattern )

(define unbound-pattern '**unbound-pattern**)

(define (check e ee fn z)
  (if (and (pair? e)
           (pair? ee)
           (eq? (car e) (car ee)) )
      (check (cdr e) (cdr ee) fn z)
      (if (null? ee) (fn e) (z)) ) )

(define (extend fn pt im)
  (lambda (x) (if (eq? pt x) im (fn x))) )

(define (cut e ee)
  (if (eq? e ee) '()
      (cons (car e) (cut (cdr e) ee)) ) )

;*---------------------------------------------------------------------*/
;*    Le jeux d'essai                                                  */
;*---------------------------------------------------------------------*/
;;; Le symbol a
(define f0 '(*quote a))

;;; La liste (a)
(define f1 '(*cons (*quote a) (*quote ())))

;;; Une liste de 0 ou + a.
(define f2 '(*times x 
		    (*cons (*quote a) (*end-times x))
		    (*quote ())))

;;; Une liste (a ... b)
(define f3 '(*times x
		    (*cons (*quote a) (*end-times x))
		    (*cons (*quote b) (*quote ()))))

;;; Une liste  de deux elements egaux
(define f4 '(*cons (*setq y (*sexp))
		  (*cons (*eval y) (*quote ()))))

;*---------------------------------------------------------------------*/
;*    test-filtre ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-filtre)
   (test-module "filtre" "filtre.scm")
   (test "filtre" (list (match f0 'a)   
			(match f0 'b)  
			(match f1 '(a)) 
			(match f1 '(b))
			(match f2 '(a a a a))
			(match f2 '(a b a a))
			(match f3 '(a a a b))
			(match f3 '(a a a a))
			(match f4 '(e e))
			(match f4 '(e f)))
	 '(#t #f #t #f #t #f #t #f #t #f)))
