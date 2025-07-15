;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Type manipulation.

(module type_type
   (import (misc_list "Misc/list.scm")
           (misc_parse "Misc/parse.scm")
           (env_env "Env/env.scm"))

   (include "Misc/read-table.sch")

   (export (numtype?::bool t)
           (vectype?::bool t)
           (packedtype?::bool t)
           (absheaptype?::bool t)
           (reftype-abv?::bool t)
           *reftypes-abbreviations*
           (reftype?::bool t)
           (valtype?::bool t)
           (addrtype?::bool t)
           (defaultable? t)
           (unpack t)
           (unpack-ft t)
           (rt-diff::pair t1::pair t2::pair)
           (ht-upperbound::symbol env::env t)
           (rt-upperbound::pair env::env t)
           (type-size::llong t::symbol)
           (deftype?::bool t)
           (rectype?::bool t)
           (deftype-head::symbol t::pair)
           (nullable?::bool rt::pair)
           (reftype->heaptype rt::pair)
           (eq-clos-st?::bool env::env t1 t2)
           (eq-clos-dt?::bool env::env t1::pair t2::pair)
           (unroll-st x::bint sts st)
           (unroll-dt::pair t::pair)
           (expand t)))

(read-table *numtypes* "Type/numtypes.sch")
(define (numtype?::bool t)
   (hashtable-contains? *numtypes* t))
(read-table *vectypes* "Type/vectypes.sch")
(define (vectype?::bool t)
   (hashtable-contains? *vectypes* t))
(read-table *packedtypes* "Type/packedtypes.sch")
(define (packedtype?::bool t)
   (hashtable-contains? *packedtypes* t))
(read-table *absheaptype* "Type/absheaptypes.sch")
(define (absheaptype?::bool t)
   (hashtable-contains? *absheaptype* t))
(read-table *reftypes-abbreviations* "Type/type-abbreviations.sch")
(define (reftype-abv?::bool t)
   (hashtable-contains? *reftypes-abbreviations* t))
(define (reftype?::bool t)
   (or (reftype-abv? t) (and (pair? t) (eq? 'ref (car t)))))
(define (valtype?::bool t)
   (or (reftype? t) (numtype? t) (vectype? t)))
(define (addrtype?::bool t)
   (or (eq? t 'i32) (eq? t 'i64)))

;; section 3.2.20
(define (defaultable? t)
   (or (vectype? t)
       (numtype? t)
       (and (reftype? t) (nullable? t))))

;; section 2.3.8
(define (unpack t)
   (if (packedtype? t) 'i32 t))

(define (unpack-ft t)
   (unpack (cadr t)))

;; section 3.1.1 (convention)
(define (rt-diff::pair t1::pair t2::pair)
   (if (nullable? t2)
       `(ref ,(reftype->heaptype t1))
       t1))

(define (ht-upperbound::symbol env::env t)
   (match-case t
      ((or any none eq i31 struct array) 'any)
      ((or func nofunc) 'func)
      ((or extern noextern) 'extern)
      ((or exn noexn) 'exn)
      ((? idx?) (ht-upperbound env (type-get env t)))
      ((? deftype?) (ht-upperbound env (deftype-head t)))))

(define (rt-upperbound::pair env::env t)
   `(ref null ,(ht-upperbound env (reftype->heaptype t))))

(define (type-size::llong t::symbol)
   (match-case t
      (i8 #l8)
      (i16 #l16)
      (i32 #l32)
      (i64 #l64)))

;; deftypes type x = (rec subtypes*).i are represented as epairs (deftype
;; subtypes* i)) with cer equal to x ; x = (rec i) are represented as (rec i)
;; with cer equal to x
;;
;; this works pretty well because when comparing epairs for equality, cers are
;; not taken into account
(define (deftype?::bool t)
   (and (pair? t) (eq? 'deftype (car t))))

(define (rectype?::bool t)
   (and (pair? t) (eq? 'rec (car t))))

;; avoid full expansion when it is not needed
(define (deftype-head::symbol t::pair)
   (match-case (cdr (list-ref (cadr t) (caddr t))) ; remove the "sub" keyword
      ((or (final ?- (?hd . ?-))
           (final (?hd . ?-))
           (?- (?hd . ?-))
           ((?hd . ?-)))
       hd)))

(define (nullable?::bool rt::pair)
   (eq? (cadr rt) 'null))

(define (reftype->heaptype rt::pair)
   (if (nullable? rt)
       (caddr rt)
       (cadr rt)))

;; we could do the equality check respecting all the structure but we can use
;; our representations slopiness to do things shorter
(define (eq-clos-st?::bool env::env t1 t2)
   (cond
      ((idx? t1) (eq-clos-st? env (type-get env t1) t2))
      ((idx? t2) (eq-clos-st? env t1 (type-get env t2)))
      ((symbol? t1) (eq? t1 t2))
      ; we suppose defined types are already closed, i.e. we have to put closed
      ; types in the context, otherwise, we may have to close the whole context
      ; each time we want to close a type
      ((and (deftype? t1) (deftype? t2))
       (equal? (cdr t1) (cdr t2)))
      ((and (rectype? t1) (rectype? t2))
       (=fx (cadr t1) (cadr t2)))
      ((and (pair? t1) (pair? t2))
       (every-same-length (lambda (t1 t2) (eq-clos-st? env t1 t2)) t1 t2))
      ((and (null? t1) (null? t2)) #t)
      ((and (boolean? t1) (boolean? t2)) (eq? t1 t2))
      (else #f)))

(define (eq-clos-dt?::bool env::env t1::pair t2::pair)
   (and (=fx (caddr t1) (caddr t2))
        (every-same-length (lambda (st1 st2) (eq-clos-st? env st1 st2))
                (cadr t1) (cadr t2))))

;; section 3.1.3

;; we use the same slopiness as in eq-clos-st?
(define (unroll-st x::bint sts st)
   (cond ((rectype? st)
          (econs 'deftype (list sts (cadr st)) (+fx (cadr st) x)))
         ((deftype? st) st)
         ((pair? st) (map (lambda (st) (unroll-st x sts st)) st))
         (else st)))

;; expects well formed arguments
(define (unroll-dt::pair t::pair)
   (unroll-st (-fx (cer t) (caddr t))
              (cadr t) (list-ref (cadr t) (caddr t))))

(define (expand t)
   (match-case (cdr (unroll-dt t)) ; remove the sub keyword
      ((or (final ?- ?ct)
           (final ?ct)
           (?- ?ct)
           (?ct))
       ct)))
