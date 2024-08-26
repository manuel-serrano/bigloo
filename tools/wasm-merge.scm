(module util
  (main main))

(define *generate-exe* #f)
(define (parse-args args)
  (filter 
    (lambda (arg)
      (cond
        ((string=? arg "-exe")
          (set! *generate-exe* #t)
          #f)
        (else #t)))
    args))

(define (main args)
  (let ((files (parse-args args)))
    (let ((module '(module)))
      (call-with-input-file (cadr files)
        (lambda (p)
          (append! module (port->sexp-list p))))

      (for-each 
        (lambda (f) 
          (append! module (read-module f))) 
        (cddr files))

      (collect-exports (cdr module))
      (remove-scheme-imports! module)
      (remove-duplicate-imports! (cdr module))
      (remove-duplicate-tags! (cdr module))
      (remove-duplicate-recs! (cdr module))
      (remove-duplicate-types! (cdr module))

      (when *generate-exe*
        (remove-exports! (cdr module)))

      (compute-initial-order (cdr module))
      (sort-module! module)
      (wasm-pp module))))

(define (ignore-wasm-comments code)
  (pregexp-replace* "\\(;(.|\n)*?;\\)" code ""))

(define (read-module f)
  (call-with-input-file f 
    (lambda (p)
      (let ((m (read (open-input-string (ignore-wasm-comments (read-string p))))))
        (if (symbol? (cadr m))
          (cddr m)
          (cdr m))))))

(define (hash-key-of x)
  (let ((p (open-output-string)))
    (write x p)
    (get-output-string p)))

(define *seen-exports* (make-hashtable))
(define (collect-exports l)
  (for-each
    (lambda (d)
      (match-case d
        ((export ?n ???-) (hashtable-put! *seen-exports* n #t))
        ((global ?- (export ?n) ???-) (hashtable-put! *seen-exports* n #t))
        ((func ?- (export ?n) ???-) (hashtable-put! *seen-exports* n #t))))
    l))

(define *seen-imports* (make-hashtable))
(define (remove-duplicate-imports! l)
  (filter!
    (lambda (d)
      (match-case d
        ((import ?m ?n ???-)
          (let ((key (string-append m ":" n)))
            (if (hashtable-contains? *seen-imports* key)
                #f
              (begin
                (hashtable-put! *seen-imports* key #t)
                #t))))
        (else #t)))
    l))

(define *seen-tags* (make-hashtable))
(define (remove-duplicate-tags! l)
  (filter!
    (lambda (d)
      (match-case d
        ((tag ?n ???-)
          (if (hashtable-contains? *seen-tags* n)
              #f
            (begin
              (hashtable-put! *seen-tags* n #t)
              #t)))
        (else #t)))
    l))

(define *seen-types* (make-hashtable))
(define (remove-duplicate-types! l)
  (filter!
    (lambda (d)
      (match-case d
        ((type ?n ???-)
          (if (hashtable-contains? *seen-types* n)
              #f
            (begin
              (hashtable-put! *seen-types* n #t)
              #t)))
        (else #t)))
    l))

(define *seen-recs* (make-hashtable))
(define (remove-duplicate-recs! l)
  (filter!
    (lambda (d)
      (match-case d
        ((rec . ?c)
          (let* ((key (hash-key-of c)))
            (if (hashtable-contains? *seen-recs* key)
                #f
              (begin
                (hashtable-put! *seen-recs* key #t)
                #t))))
        (else #t)))
    l))

(define (remove-scheme-imports! mod)
  (filter!
    (lambda (d)
      (match-case d
        ((import ?m ?n ???-)
          (if (string-prefix? "__js" m) ;; Keep JS imports
            #t
            (not (hashtable-contains? *seen-exports* n))))
        (else #t)))
    (cdr mod)))

(define *export-whitelist* (make-hashtable))
(define (keep-export? n) (hashtable-contains? *export-whitelist* n))

(define (init-export-whitelist)
  (hashtable-put! *export-whitelist* "memory" #t))

(define (remove-trivial-exports! l)
  (filter!
    (lambda (d)
      (match-case d
        ((export ?n ???-) (keep-export? n))
        (else #t)))
    l))

(define (remove-exports! l)
  (init-export-whitelist)

  (remove-trivial-exports! l)
  (for-each
    (lambda (d)
      (match-case d
        ((global ?id (export ?n) . ?r)
          (unless (keep-export? n)
            (set-cdr! d (cons id r))))
        ((func ?id (export ?n) . ?r)
          (unless (keep-export? n)
            (set-cdr! d (cons id r))))))
    l))

(define *initial-order* (make-hashtable))
(define (compute-initial-order l)
  (let ((index 0))
    (for-each 
      (lambda (d)
        (let ((key (hash-key-of d)))
          (unless (hashtable-contains? *initial-order* key)
            (hashtable-put! *initial-order* key index)))
        (set! index (+fx index 1)))
      l)))

(define (sort-module! mod)
  (let ((orders '((import 0)
                  (memory 1)
                  (type 2)
                  (rec 2)
                  (tag 3)
                  (export 4)
                  (global 5)
                  (data 6)
                  (func 7))))

    (define (order-of x)
      (let ((r (assq (car x) orders)))
        (if r
          (cadr r)
          1000)))

    (set-cdr! mod 
      (sort (cdr mod) 
        (lambda (x y)
          (let ((xorder (order-of x))
                (yorder (order-of y)))
            (if (=fx xorder yorder)
              (<fx 
                (hashtable-get *initial-order* (hash-key-of x))
                (hashtable-get *initial-order* (hash-key-of y)))
              (<fx xorder yorder))))))))

(define (wasm-pp l)
  (define (ppindent depth)
    (unless (=fx depth 0)
        (display "  ")
        (ppindent (-fx depth 1))))

  (letrec ((aux (lambda (l depth)
    (define (pp-args l)
      (for-each (lambda (n)
        (newline)
        (aux n (+fx depth 1))) l))

    (define (dump-string s)
      (define (visible? c)
        (and 
          (char>=? c #\x20) 
          (char<? c #\x7F) ;; exclude the DEL character (illegal in WASM text format)
          (not (char=? c #\")) 
          (not (char=? c #\\))))

      (display "\"")
      (let iter ((i 0))
        (when (<fx i (string-length s))
          (let ((c (string-ref s i))
                (hex "0123456789abcdef"))
            (cond 
              ((visible? c) (display c))
              ((char=? c #\") (display "\\\""))
              ((char=? c #\\) (display "\\\\"))
              (else (display* "\\" 
                (string-ref hex (bit-rsh (char->integer (char-and c #\xF0)) 4))
                (string-ref hex (char->integer (char-and c #\x0F)))
                ))))
          (iter (+fx i 1))))
      (display "\""))

    (define (pp-arg a)
      (cond
        ((elong? a) (display a))
        ((llong? a) (display a))
        ((bignum? a) (display a))
        ((string? a) (dump-string a))
        (else (write a))))

    (define (pp-0 l)
      (display "(")
      (write (car l))
      (pp-args (cdr l))
      (display ")"))

    (define (pp-1 l)
      (display "(")
      (write (car l))
      (display " ")
      (pp-arg (cadr l))
      (pp-args (cddr l))
      (display ")"))

    (define (pp-2 l)
      (display "(")
      (write (car l))
      (display " ")
      (pp-arg (cadr l))
      (display " ")
      (pp-arg (car (cddr l)))
      (pp-args (cdr (cddr l)))
      (display ")"))

    (define (pp-oneline l)
      (display "(")
      (write (car l))
      (map (lambda (a) (display " ") (pp-arg a)) (cdr l))
      (display ")"))

    (ppindent depth)
    (cond 
      ((pair? l)
        (case (car l)
          ('import (pp-2 l))
          ('func (pp-1 l))
          ('type (pp-1 l))
          ('sub (pp-1 l))
          ('global (pp-1 l))
          ('memory (pp-oneline l))
          ('data (pp-1 l))
          ('elem (pp-oneline l))
          ('export (pp-oneline l))
          ('param (pp-oneline l))
          ('result (pp-oneline l))
          ('local (pp-oneline l))
          ('field (pp-oneline l))
          ('mut (pp-oneline l))
          ('i32.const (pp-oneline l))
          ('i64.const (pp-oneline l))
          ('f32.const (pp-oneline l))
          ('f64.const (pp-oneline l))
          ('ref (pp-oneline l))
          ('ref.null (pp-oneline l))
          ('local.get (pp-oneline l))
          ('global.get (pp-oneline l))
          ('local.set (pp-1 l))
          ('global.set (pp-1 l))
          ('br (pp-oneline l))
          ('unreachable (pp-oneline l))
          ('block (if (symbol? (cadr l)) (pp-1 l) (pp-0 l)))
          ('loop (if (symbol? (cadr l)) (pp-1 l) (pp-0 l)))
          ('call (pp-1 l))
          ('return_call (pp-1 l))
          ('call_ref (pp-1 l))
          ('return_call_ref (pp-1 l))
          ('struct.new (pp-1 l))
          ('struct.get (pp-2 l))
          ('array.get (pp-1 l))
          ('array.new (pp-1 l))
          ('array.new_elem (pp-2 l))
          (else (pp-0 l))))
      ((not l) 'nothing)
      (else (pp-arg l))))))
    (aux l 0))
  (newline))
