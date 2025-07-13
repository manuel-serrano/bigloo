;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Some complements to srfi-1.

(module misc_list
   (export (every-same-length f . lists)
           (length>=?::bool l::pair-nil i::bint)
           (econcat l)))

;; like every but returns #f if the lists are not of the same length
(define (every-same-length f . lists)
   (if (any null? lists)
       (every null? lists)
       (and (apply f (map car lists))
            (apply every-same-length f (map cdr lists)))))

(define (length>=?::bool l::pair-nil i::bint)
   (if (null? l)
       (>=fx 0 i)
       (or (>=fx 1 i) (length>=? (cdr l) (-fx i 1)))))

(define (econcat l)
   (if (null? l)
       '()
       (if (null? (car l))
           (econcat (cdr l))
           (econs (car (car l))
                  (econcat (cons (cdr (car l)) (cdr l)))
                  (cer (car l))))))
