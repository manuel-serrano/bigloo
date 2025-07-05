;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Type matching (section 3.3 of the specification).

(module type_match
   (library srfi1)
   (import (env_env "Env/env.scm")
           (type_type "Type/type.scm")
           (misc_parse "Misc/parse.scm")
           (misc_list "Misc/list.scm"))

   (export (<rt=::bool env::env t1::pair t2::pair)
           (<vt=::bool env::env t1 t2)
           (<res=::bool env l1 l2)
           (<st=::bool env t1 t2)
           (<ct=::bool env::env t1::pair t2::pair)))

;; subtyping relation return only a boolean, which is good if it is true, but in
;; case of failure we might want a reason to explain it (kind of a constructive
;; proof that not (t1 < t2), could thus be solved by writing a function not-<
;; that returns #f in case t1 < t2 and a reason for why it is not the case
;; otherwise)
;;
;; we could probably speed up the subtyping comparaisons by inserting eq?
;; shortcuts at the write place, the official validation tool does it for
;; defined types; we probably need benchmarks for that

;; section 3.3.3
(define (<ht=::bool env::env t1 t2)
   (cond ((equal? t1 t2) #t)
         ((eq? t1 'bot) #t)
         ((idx? t1) (<ht= env (type-get env t1) t2))
         ((idx? t2) (<ht= env t1 (type-get env t2)))
         ((eq? 'none t1) (<ht= env t2 'any))
         ((eq? 'nofunc t1) (<ht= env t2 'func))
         ((eq? 'noextern t1) (<ht= env t2 'extern))
         ((eq? 'noexn t1) (<ht= env t2 'exn))
         ((eq? t2 'any) (<ht= env t1 'eq))
         ((eq? t2 'eq)
          (or (eq? t1 'i31)
              (eq? t1 'struct)
              (eq? t1 'array)
              (and (deftype? t1) (<ht= env (deftype-head t1) 'eq))))
         ((and (deftype? t1) (symbol? t2))
          (eq? (deftype-head t1) t2))
         ((and (deftype? t1) (deftype? t2))
          (<dt= env t1 t2))
         (#t #f)))

;; actually does redundant checks, could be expanded to avoid them
;; section 3.3.4
(define (<rt=::bool env::env t1::pair t2::pair)
   (and (or (nullable? t2) (not (nullable? t1)))
        (<ht= env (reftype->heaptype t1) (reftype->heaptype t2))))

;; section 3.3.5
(define (<vt=::bool env::env t1 t2)
   (or (eq? t1 t2)
       ;(and (numtype? t1) (numtype? t2) (eq? t1 t2))
       ;(and (vectype? t1) (vectype? t2) (eq? t1 t2))
       (and (reftype? t1) (reftype? t2) (<rt= env t1 t2))
       (eq? t1 'bot)
       (eq? t2 'top)))

;; section 3.3.6
(define (<res=::bool env l1 l2)
   (every' (lambda (t1 t2) (<vt= env t1 t2)) l1 l2))

;; section 3.3.8
(define (<funct=::bool env::env t1::pair t2::pair)
   (let ((t11 (car t1))
         (t12 (cadr t1))
         (t21 (car t2))
         (t22 (cadr t2)))
      (and
       (<res= env t21 t11)
       (<res= env t12 t22))))

;; section 3.3.10
(define (<st=::bool env t1 t2)
  (or ; (and (packedtype? t1) (eq? t1 t2))
      (eq? t1 t2)
      (and (valtype? t1) (valtype? t2) (<vt= env t1 t2))))

(define (<fldt=::bool env::env t1::pair t2::pair)
   (match-case (cons t1 t2)
      (((#f ?st1) . (#f ?st2))
       (<st= env st1 st2))
      (((#t ?st1) . (#t ?st2))
       (and (<st= env st1 st2) (<st= env st2 st1)))
      (else #f)))

;; section 3.3.9
(define (<ct=::bool env::env t1::pair t2::pair)
   (match-case (cons t1 t2)
      (((array ?fldt1) . (array ?fldt2))
       (<fldt= env fldt1 fldt2))
      (((func . ?funct1) . (func . ?funct2))
       (<funct= env funct1 funct2))
      (((struct . ?fldts1) . (struct . ?fldts2))
       (let ((n1 (length fldts1))
             (n2 (length fldts2)))
          (and (<= n2 n1)
               (every (lambda (fldt1 fldt2) (<fldt= env fldt1 fldt2))
                      fldts1 fldts2))))
      (else #f)))

;; section 3.3.11
(define (get-sub-heaptype st)
   (match-case (cdr st)
      ((or (final ?ht ?-)
           ((and (not final) ?ht) ?-))
       ht)
      (else #f)))

(define (<dt=::bool env::env t1 t2)
   (if (eq-clos-dt? env t1 t2)
       #t
       (let ((ht (get-sub-heaptype (unroll-dt t1))))
          (and ht (<ht= env ht t2)))))
