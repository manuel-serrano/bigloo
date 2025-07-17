;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Parsing functions.

(module misc_parse
   (export (ident?::bool x)
           (idx?::bool x)
           (wnumber->number x)))

(define (ident?::bool x)
   (and (symbol? x) (equal? #\$ (string-ref (symbol->string x) 0))))

(define (idx?::bool x)
   (or (ident? x)
       (number? x)))

(define (wnumber->number n)
   (cond ((number? n) n)
         ((eq? n 'inf) +inf.0)
         ((eq? n '-inf) -inf.0)
         ((eq? n 'nan) +nan.0)
         ((symbol? n)
          (let ((s (symbol->string n)))
             (if (and (>= (string-length s) 2) (substring-at? s "0x" 0))
                 (let ((m (string->number (substring s 2) 16)))
                   (if m m (raise `(expected-number ,n))))
                 (raise `(expected-number ,n)))))
         (else (raise `(expected-number ,n)))))
