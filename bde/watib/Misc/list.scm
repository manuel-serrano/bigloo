;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Some complements to srfi-1.

(module misc_list
   (export (every' f . lists)
           (map-seq f . lists)
           (length>=?::bool l::pair-nil i::bint)))

;; like every but returns #f if the lists are not of the same length
(define (every' f . lists)
   (if (any null? lists)
       (every null? lists)
       (and (apply f (map car lists)) (apply every' f (map cdr lists)))))

;; evaluation order is not unspecified in the standard, we enforce one here
(define (map-seq f . lists)
   (if (any null? lists)
       '()
       (cons (apply f (map car lists)) (apply map-seq f (map cdr lists)))))

(define (length>=?::bool l::pair-nil i::bint)
   (if (null? l)
       (>=fx 0 i)
       (or (>=fx 1 i) (length>=? (cdr l) (-fx i 1)))))
