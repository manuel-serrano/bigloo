;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Parsing functions.

(module misc_parse
   (export (ident?::bool x)
           (idx?::bool x)))

(define (ident?::bool x)
   (and (symbol? x) (equal? #\$ (string-ref (symbol->string x) 0))))

(define (idx?::bool x)
   (or (ident? x)
       (number? x)))
