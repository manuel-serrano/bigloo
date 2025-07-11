;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Validation of wasm in text format and translation to an internal
;; representation.

(module val_validate
   (library srfi1)
   (cond-expand
      ((and multijob (library pthread)) (library pthread)))
   (include "Misc/read-table.sch")
   (import (env_env "Env/env.scm")
           (type_type "Type/type.scm")
           (type_match "Type/match.scm")
           (misc_list "Misc/list.scm")
           (misc_parse "Misc/parse.scm")
           (ast_node "Ast/node.scm"))

   (export (class &watlib-validate-error::&error)
       (valid-file f::pair-nil nthreads::long keep-going::obj silent::bool)))

;;; we force everywhere the number of type indices after sub final? to be one;
;;; even though forms with more than one type are syntactically correct, they
;;; are never valid

(define keep-going #f)
(define error-encountered? #f)
(define silent #f)
(define nthreads 1)

(define-macro (with-default-value env def dec . body)
   `(with-handler
       (lambda (e)
          (if (isa? e &error)
              (raise e))
          (if keep-going
              (begin
                 (set! (-> ,env error-list) (cons (append ,dec (list e))
                                                  (-> ,env error-list)))
                 ,def)
              (raise (append ,dec (list e)))))
       ,@body))

;; section 3.4.13
;; only use on desuggared instructions
(read-table *const-instrs* "Val/constant-instructions.sch")
(define-generic (non-constant-instr? i::instruction)
   (if (or (hashtable-contains? *const-instrs* (-> i opcode))
           (eq? (-> i opcode) 'error))
       #f
       i))

(define-method (non-constant-instr? i::one-arg)
   (if (eq? 'global.get (-> i opcode))
       (with-access::globalidxp (-> i x) (mut?) (if mut? i #f))
       (call-next-method)))

(define (non-constant-expr? e::pair-nil)
   (find (lambda (i) (non-constant-instr? i)) e))

(define-macro (replace-exception e e' . body)
   `(with-handler
       (lambda (exn)
          (if (and (pair? exn) (eq? (car exn) ,e))
              (raise (cons ,e' (cdr exn)))
              (raise exn)))
       ,@body))

(define-macro (map-env f env . l)
   `(map (lambda (x) (,f ,env x)) ,@l))

(define (stack-take::pair-nil st::pair-nil i::long)
   (cond ((=fx i 0) '())
         ((null? st) (raise 'empty-stack))
         ((eq? (car st) 'poly) (make-list i 'bot))
         (else (cons (car st) (stack-take (cdr st) (-fx i 1))))))

(define (stack-drop::pair-nil st::pair-nil i::long)
   (cond ((=fx i 0) '())
         ((null? st) (raise 'empty-stack))
         ((eq? (car st) 'poly) '(poly))
         (else (stack-drop (cdr st) (-fx i 1)))))

(define (stack-drop-reftype st::pair-nil)
   (when (null? st)
      (raise 'empty-stack-reftype))
   (cond ((reftype? (car st)) (values (reftype->heaptype (car st)) (cdr st)))
         ((eq? 'poly (car st)) (values 'bot st))
         (else (raise `(expected-reftype-stack ,st)))))

(define (valid-func-ref?::bool env::env x::long)
   (let ((h (-> env refs)))
      (or (hashtable-contains? h x)
          (hashtable-contains? h (vector-ref (-> env func-names) x)))))

;;; validation functions take types in the text-format and return the internal
;;; representation if there wasn't any problem

;; section 3.2.3
(define (valid-ht env::env t)
   (if (or (absheaptype? t) (rectype? t))
       t
       ;; ensures that type that get validated are closed
       (replace-exception 'expected-typeidx 'expected-heaptype
          (type-get env t))))

;; section 3.2.4
(define (valid-rt::pair env::env t)
   (replace-exception 'expected-heaptype 'expected-reftype
      (match-case t
         ((ref ?ht) `(ref ,(valid-ht env ht)))
         ((ref null ?ht) `(ref null ,(valid-ht env ht)))
         ((? reftype-abv?) (hashtable-get *reftypes-abbreviations* t))
         (else (raise `(expected-reftype ,t))))))

;; sections 3.2.1, 3.2.2, and 3.2.5
(define (valid-vt env::env t)
   (replace-exception 'expected-reftype 'expected-valtype
      (if (or (vectype? t) (numtype? t))
          t
          (valid-rt env t))))

;; section 3.2.9 and 6.4.6
(define (valid-names/param/result/get-tl env::env l::pair-nil)
   (define (valid-vt-at pos::epair t)
      (with-default-value env 'error `(at ,(cer pos))
         (valid-vt env t)))

   (match-case l
      (((param (and (? ident?) ?id) ?vt) . ?tl)
       (multiple-value-bind (n p r tl) (valid-names/param/result/get-tl env tl)
          (values (cons id n) (cons (valid-vt-at (car l) vt) p) r tl)))
      (((param . ?vts) . ?tl)
       (multiple-value-bind (n p r tl) (valid-names/param/result/get-tl env tl)
          (values (append (make-list (length vts) #f))
                  (append (map-env valid-vt-at (car l) vts) p) r tl)))
      (((result . ?-) . ?-)
       (define (get-results/tl l)
          (match-case l
             (((result . ?vts) . ?tl)
              (multiple-value-bind (r tl) (get-results/tl tl)
                 (values (append (map-env valid-vt-at (car l) vts) r) tl)))
             (else (values '() l))))
       (multiple-value-bind (r tl) (get-results/tl l)
          (values '() '() r tl)))
      (else (values '() '() '() l))))

(define (valid-param/result env::env l::pair-nil)
   (multiple-value-bind (n p r tl) (valid-names/param/result/get-tl env l)
      (unless (null? tl)
         (raise `(expected-functiontype ,tl)))
      (values p r)))

(define (valid-tu/get-tl env::env l::pair-nil)
   (multiple-value-bind (n p r tl) (valid-names/param/result/get-tl env l)
      (values n (list p r) tl)))

(define (valid-blocktype/get-tl env::env l::pair-nil)
   (multiple-value-bind (args f tl) (valid-tu/get-tl env l)
      (unless (every not args)
         (raise `(named-param-blocktype ,args)))
      (values f tl)))

;; section 3.2.11 and 6.4.7
(define (valid-fldt env::env t)
   (define (valid-st t)
      (replace-exception 'expected-valtype 'expected-storagetype
         (if (packedtype? t)
             t
             (valid-vt env t))))
   (with-default-value env '(#f error) '()
      (match-case t
         ((mut ?st) `(#t ,(valid-st st)))
         (?st (replace-exception 'expected-storagetype 'expected-fieldtype
                 `(#f ,(valid-st st)))))))

;; section 6.4.7
(define (valid-fields/names env::env l)
   (match-case l
      (() (values '() '()))
      (((field (and (? ident?) ?name) ?fldt) . ?tl)
       (multiple-value-bind (fields names) (valid-fields/names env tl)
          (values (cons (valid-fldt env fldt) fields) (cons name names))))
      (((field . ?fldts) . ?tl)
       (multiple-value-bind (fields names) (valid-fields/names env tl)
          (values (append (map-env valid-fldt env fldts) fields)
                  (append (map (lambda (-) (gensym "$unnamedfield")) fldts)
                          names))))
      (else (raise `(expected-fields ,l)))))

;; section 3.2.10
(define (valid-ct env::env t x::long)
   (match-case t
      ((func . ?p/r)
       (multiple-value-bind (p r) (valid-param/result env p/r)
          `(func ,p ,r)))
      ((array ?fldt) `(array ,(valid-fldt env fldt)))
      ((struct . ?fldts)
       (multiple-value-bind (fields names) (valid-fields/names env fldts)
          (vector-set! (-> env field-names) x names)
          `(struct ,@fields)))
      (else (raise `(expected-comptype ,t)))))

;; expects input to be of the form (sub final? id? t)*, where t has been
;; validated: a first lifting is done while checking modules.

;; section 3.2.12
(define (valid-rect env::env l x::long)
   (define (valid-st t x)
      (match-case t
         ((sub final . ?rst)
          `(sub final ,@(cdr (valid-st `(sub ,@rst) x))))
         ((sub ?y ?ct)
          (if (<= x (type-get-index env y))
              (raise `(forward-subtype ,x ,y)))
          (match-case (cdr (unroll-dt (type-get env y)))
             ((final . ?-) (raise `(supertype-final ,x ,y)))
             ((or (?- ?ct') (?ct'))
              (unless (<ct= env ct ct')
                 (raise `(non-matching-supertype ,x ,y ,ct ,ct'))))
             (else (raise 'internal-error)))
          `(sub ,(type-get-index env y) ,ct))
         ((sub ?-) t)))

   (define (valid-rec-list l x)
      (if (null? l) ; (rec) is valid
          '()
          (cons (valid-st (car l) x) (valid-rec-list (cdr l) (+fx x 1)))))

   `(rec ,(valid-rec-list l x)))

;; section 3.2.14
(define (valid-lim l::pair-nil k::llong)
   (when (null? l)
      (raise `((expected limit) ,l)))
   (let ((n (wnumber->number (car l))))
      (unless (<= n k)
         (raise `(invalid-bound ,n ,k)))
      (if (null? (cdr l))
          (cons n '())
          (let ((m (wnumber->number (cadr l))))
             (unless (<= m k)
                (raise `(invalid-bound ,m ,k)))
             (cons n m)))))

;; section 3.2.16 and 6.4.13
(define (valid-mt::memory env::env t)
   (with-default-value env '(error (0)) '()
      (match-case t
         (((and ?at (? addrtype?)) . ?l)
          (instantiate::memory
           (at at)
           (lim (valid-lim l (bit-lshllong #l1 (-llong (type-size at) #l16))))))
         ; section 6.4.10
         (else (valid-mt env `(i32 ,@t))))))

;; section 3.2.18 and 6.4.14
(define (valid-gt::pair env::env t)
   (with-default-value env '(#f error) '()
         (match-case t
            ((mut ?vt) (list #t (valid-vt env vt)))
            (else (replace-exception 'expected-valtype 'expected-globaltype
                     (list #f (valid-vt env t)))))))

;; section 6.4.9
(define (clean-mod-rectype! env::env l x::long)
   ; the `(rec ...) in the environment assures rolling
   (let ((sts (map-in-order
                 (match-lambda
                    ((type (and (? ident?) ?id) ?st)
                     (add-type! env id `(rec ,(-fx (-> env ntype) x)
                                         ,(-> env ntype))) st)
                    ((type ?st)
                     (add-type! env #f `(rec ,(-fx (-> env ntype) x)
                                         ,(-> env ntype))) st)
                    ((type ?x ?-)
                     (raise `((expected ident) ,x)))
                    (?x (raise `(expected-typedef ,x)))) l)))
      (define (valid-st st x)
         (with-default-value env '(sub final (error)) `(at-subtype ,st)
               (match-case st
                  ((sub final ?ct) `(sub final ,(valid-ct env ct x)))
                  ((sub final (and ?y (? idx?)) ?ct)
                   `(sub final ,(type-get-index env y) ,(valid-ct env ct x)))
                  ((sub (and ?y (? idx?)) ?ct)
                   `(sub ,(type-get-index env y) ,(valid-ct env ct x)))
                  ((sub ?ct)
                   `(sub ,(valid-ct env ct x)))
                  (else
                   (replace-exception 'expected-comptype 'expected-subtype
                      `(sub final ,(valid-ct env st x)))))))

      (let ((rolled-sts (map valid-st sts (iota (length sts) x 1))))
         (for-each (lambda (i t)
                     (set-type! env (+fx x i)
                                `(deftype ,(+fx x i) ,rolled-sts ,i)))
                   (iota (length sts)) rolled-sts)
         rolled-sts)))

;; section 3.4

(define (wnumber->number n)
   (cond ((number? n) n)
         ((eq? n 'inf) +inf.0)
         ((eq? n '-inf) -inf.0)
         ((eq? n 'nan) +nan.0)
         ((symbol? n)
          (let ((s (symbol->string n)))
             (if (and (>= (string-length s) 2) (substring-at? s "0x" 0))
                 (string->number (substring s 2) 16)
                 (raise `(expected-number ,n)))))
         (#t (raise `(expected-number ,n)))))

(define (i32::i32p env::env n)
   (let ((n (wnumber->number n)))
      (instantiate::i32p
       (num
        (cond ((not (integer? n)) (raise `(expected-int ,n)))
              ((and (<= n 2147483647) (>= n -2147483648)) n)
              ((and (> n 2147483647) (<= n 4294967295)) (- n (* 2 2147483648)))
              (#t (raise `(out-bounds-i32 ,n))))))))

(define (i64::i64p env::env n)
   (let ((n (wnumber->number n)))
      (instantiate::i64p
       (num
        (cond ((not (integer? n)) (raise `(expected-int ,n)))
              ((and (<= n 9223372036854775807) (>= n -9223372036854775808)) n)
              ((and (> n 9223372036854775807) (<= n 18446744073709551615))
               (- n (* 2 9223372036854775808)))
              (#t (raise `(out-bounds-i64 ,n))))))))

(define (f32::f32p env::env n)
   (instantiate::f32p (num (wnumber->number n))))

(define (f64::f64p env::env n)
   (instantiate::f64p (num (wnumber->number n))))

(define (u32::idxp env::env n)
   (let ((n (wnumber->number n)))
      (instantiate::idxp
       (idx
        (cond ((not (integer? n)) (raise `(expected-int ,n)))
              ((and (<= n 4294967295) (>= n 0)) n)
              (#t (raise `(out-bounds-u32 ,n))))))))

(define (ht::typep env::env t)
   (instantiate::typep (type (valid-ht env t))))

(define (rt::typep env::env t)
   (instantiate::typep (type (valid-rt env t))))

(define (get-struct-fldts x::typeidxp)
   (match-case (expand (-> x type))
      ((struct . ?fldts) fldts)
      (?t
       (raise `((expected struct) ,x ,t)))))

(define (get-array-ft x::typeidxp)
   (match-case (expand (-> x type))
      ((array ?ft) ft)
      (?t (raise `((expected array) ,x ,t)))))

(define (label-get-last-rest l::labelidxp)
   (let ((t'* (-> l type)))
      (when (null? t'*)
         (raise 'expected-non-empty-result))
      (multiple-value-bind (t* tl) (split-at t'* (- (length t'*) 1))
         (values t* (car tl)))))

(read-table *instruction-types* "Val/instruction-types.sch")

; the following function implements subsumption (section 3.4.12) in an
; syntax-directed way
(define (check-stack::pair-nil env::env st::pair-nil ts::pair-nil)
   (define (aux::pair-nil st::pair-nil ts::pair-nil)
      (cond ((null? st)
             (unless (null? ts) (raise `(empty-stack ,ts)))
             '())
            ((null? ts) st)
            ((equal? st '(poly)) '(poly))
            ((<vt= env (car st) (car ts))
             (aux (cdr st) (cdr ts)))
            (#t (raise `(non-matching-stack ,(car st) ,(car ts))))))
   (aux st (reverse ts)))

(define (check-block::sequence env::env body::pair-nil t::pair-nil
                               bt::pair #!optional (l #f))
   (let ((loc-init (-> env local-types)))
      (push-label! env l t)
      (multiple-value-bind (i st) (valid-instrs env body (car bt))
         (let ((st-rst (check-stack env st (cadr bt))))
            (unless (or (null? st-rst) (eq? 'poly (car st-rst)))
               (raise `(value-left-stack ,st-rst))))
         (pop-label! env)
         (set! (-> env local-types) loc-init)
         (instantiate::sequence
          (intype (car bt))
          (outtype (cadr bt))
          (parent (-> env parent))
          (opcode 'nop)
          (body i)))))

(define (valid-blockinstr env::env i::pair st::pair-nil)
   (match-case i
      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.183
      ((block (and (? ident?) ?l) . ?body)
       (multiple-value-bind (bt tl) (valid-blocktype/get-tl env body)
          (values bt
                  (duplicate::block (check-block env tl (cadr bt) bt l)
                                    (opcode 'block))
                  '())))
      ((block . ?rst)
       (valid-blockinstr env `(block ,(gensym "$unnamed-label") ,@rst) st))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.184
      ((loop (and (? ident?) ?l) . ?body)
       (multiple-value-bind (bt tl) (valid-blocktype/get-tl env body)
          (values bt
                  (duplicate::loop (check-block env tl (car bt) bt l)
                                   (opcode 'loop))
                  '())))
      ((loop . ?rst)
       (valid-blockinstr env `(loop ,(gensym "$unnamed-label") ,@rst) st))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.185
      ((if (and (? ident?) ?l) . ?body)
       (define (get-tl/then/else l::pair-nil)
          (match-case l
             (((then . ?then) ((kwote else) . ?else)) (values '() then else))
             (((then . ?then)) (values '() then '()))
             ((?hd . ?tl)
              (multiple-value-bind (tl then else) (get-tl/then/else tl)
                 (values (cons hd tl) then else)))
             (else (raise `(expected-then/else ,l)))))

       (multiple-value-bind (bt tl) (valid-blocktype/get-tl env body)
          (multiple-value-bind (tl then else) (get-tl/then/else tl)
             (let ((i::if-then (instantiate::if-then
                                (intype `(,@(car bt) i32))
                                (outtype (cadr bt))
                                (parent (-> env parent))
                                (opcode 'if)
                                (then (check-block env then (cadr bt) bt l))))
                   (else::sequence (check-block env else (cadr bt) bt l)))
                (values `((,@(car bt) i32) ,(cadr bt))
                        (if (null? (-> else body))
                            i
                            (duplicate::if-else i (else else)))
                        tl)))))
      ((if . ?rst)
       (valid-blockinstr env `(if ,(gensym "$unnamed-label") ,@rst) st))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.186
      ((try_table (and (? ident?) ?l) . ?body)
       (define (valid-catch/get-body l::pair-nil)
          (match-case l
             ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.187
             (((catch ?tag ?lab) . ?tl)
              (let* ((x::tagidxp (tagidx env tag))
                     (l::labelidxp (labelidx env lab))
                     (t* (cadr (expand (-> x type))))
                     (lt (-> l type)))
                 (unless (<res= env t* lt)
                    (raise `(non-matching-catch ,x ,l ,t* ,lt)))
                 (multiple-value-bind (c tl) (valid-catch/get-body tl)
                    (values (cons (instantiate::catch (label l) (tag x)) c)
                            tl))))

             ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.188
             (((catch_ref ?tag ?lab) . ?tl)
              (let* ((x::tagidxp (tagidx env tag))
                     (l::labelidxp (labelidx env lab))
                     (t* (cadr (expand (-> x type))))
                     (lt (-> l type)))
                 (unless (<res= env (append t* '((ref exn))) lt)
                    (raise `(non-matching-catch-ref ,x ,l ,t* ,lt)))
                 (multiple-value-bind (c tl) (valid-catch/get-body tl)
                    (values (cons (instantiate::catch_ref (label l) (tag x)) c)
                            tl))))

             ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.189
             (((catch_all ?lab) . ?tl)
              (let* ((l::labelidxp (labelidx env lab))
                     (lt (-> l type)))
                 (unless (null? lt)
                    (raise `(non-empty-label-catch-all ,l ,lt)))
                 (multiple-value-bind (c tl) (valid-catch/get-body tl)
                    (values (cons (instantiate::catch_all (label l)) c) tl))))

             ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.190
             (((catch_all_ref ?lab) . ?tl)
              (let* ((l::labelidxp (labelidx env lab))
                     (lt (-> l type)))
                 (unless (<res= env '((ref exn)) lt)
                    (raise `(non-matching-catch-all-ref ,l ,lt)))
                 (multiple-value-bind (c tl) (valid-catch/get-body tl)
                    (values (cons (instantiate::catch_all_ref (label l)) c)
                            tl))))

             (?tl (values '() tl))))

       (multiple-value-bind (bt tl) (valid-blocktype/get-tl env body)
          (multiple-value-bind (c tl) (valid-catch/get-body tl)
             (values bt
                     (duplicate::try_table (check-block env tl (cadr bt) bt l)
                      (parent (-> env parent))
                      (opcode 'try_table)
                      (catches c))
                     '()))))
      ((try_table . ?rst)
       (valid-blockinstr env `(try_table ,(gensym "$unnamed-label") ,@rst) st))

      (else (raise `(unknown-opcode ,(car i))))))

;; returns the type of the given instruction, the desuggared instruction and
;; the tail in case it is in s-expression format
;;
;; for instance with (i32.add (i32.const 0) (i32.const 0)), it will return:
;; (values (i32.add) ((i32 i32) (i32)) ((i32.const 0) (i32.const 0)))
(define (typeof-instr/instr/tl env::env i::pair st::pair-nil)
   (if (hashtable-contains? *instruction-types* (car i))
       (let* ((v (hashtable-get *instruction-types* (car i)))
              (exp-args (car v))
              (t (cadr v))
              (k (length exp-args)))
          (when (< (length (cdr i)) k)
             (raise `(not-enough arguments ,i ,exp-args)))
          (multiple-value-bind (giv-args tl) (split-at (cdr i) k)
             (let* ((args (map (lambda (f x) (f env x)) exp-args giv-args))
                    (t (if (procedure? t) (apply t env args) t))
                    (i (instantiate::instruction
                        (intype (car t))
                        (outtype (cadr t))
                        (parent (-> env parent))
                        (opcode (car i)))))
                (match-case args
                   (()
                    (values t i tl))
                   ((?x)
                    (values t (duplicate::one-arg i (x x)) tl))
                   ((?x ?y)
                    (values t (duplicate::two-args i (x x) (y y)) tl))
                   ((?x ?y ?z)
                    (values t (duplicate::three-args i (x x) (y y) (z z))
                            tl))))))
       (valid-blockinstr env i st)))

(define (adhoc-instr?::bool s)
   (or (eq? s 'br_table) (eq? s 'br_on_null) (eq? s 'ref.as_non_null)
       (eq? s 'ref.is_null)))

(define (adhoc-instr env::env i::pair st::pair-nil)
   (match-case i
      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.193

      ; to avoid having to compute a greatest lower bound, we check each label's
      ; type against the stack, which will serve as a lower bound in absence of
      ; failure and if all the label's arity are the same
      ((br_table ?lab . ?rst)
       (let* ((l::labelidxp (labelidx env lab))
              (n (length (-> l type))))

          (define (get-label/tl l::pair-nil)
             (match-case l
                (((and (? idx?) ?lab) . ?tl)
                    (multiple-value-bind (ls tl) (get-label/tl tl)
                       (values (cons (labelidx env lab) ls) tl)))
                (else (values '() l))))

          (multiple-value-bind (ls tl) (get-label/tl (cdr i))
             (multiple-value-bind (i st) (valid-instrs env tl st)
                (let* ((st (check-stack env st '(i32)))
                       (lower-bound (stack-take st n)))
                   (define (valid-label l::labelidxp)
                      (let ((lt (-> l type)))
                         (unless (<res= env lower-bound lt)
                            (raise `(non-matching ,lower-bound ,lt)))))

                   (for-each valid-label ls)
                   (values (append i `(,(instantiate::br_table
                                         (intype '())
                                         (outtype '(poly))
                                         (parent (-> env parent))
                                         (opcode 'br_table)
                                         (labels ls))))
                           '(poly)))))))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.194
      ((br_on_null ?lab . ?tl)
       (let* ((l::labelidxp (labelidx env lab))
              (t* (-> l type)))
          (multiple-value-bind (i st) (valid-instrs env tl st)
             (multiple-value-bind (ht st) (stack-drop-reftype st)
               (values (append i `(,(instantiate::one-arg
                                     (intype `(,@(reverse t*) (ref null ,ht)))
                                     (outtype `(,@(reverse t*) (ref ,ht)))
                                     (parent (-> env parent))
                                     (opcode 'br_on_null)
                                     (x l))))
                       (append (cons `(ref ,ht) (reverse t*))
                               (check-stack env st t*)))))))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.99
      ((ref.is_null . ?tl)
       (multiple-value-bind (i st) (valid-instrs env tl st)
          (multiple-value-bind (ht st) (stack-drop-reftype st)
             (values (append i `(,(instantiate::instruction
                                   (intype `((ref null ,ht)))
                                   (outtype '(i32))
                                   (parent (-> env parent))
                                   (opcode 'ref.is_null)))) (cons 'i32 st)))))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.100
      ((ref.as_non_null . ?tl)
       (multiple-value-bind (i st) (valid-instrs env tl st)
          (multiple-value-bind (ht st) (stack-drop-reftype st)
             (values (append i `(,(instantiate::instruction
                                   (intype `((ref null ,ht)))
                                   (outtype `((ref ,ht)))
                                   (parent (-> env parent))
                                   (opcode 'ref.as_non_null))))
                     (cons `(ref ,ht) st)))))))

;; returns the desuggared instructions and the new stack state
(define (valid-instrs env::env l::pair-nil st::pair-nil)
   (define (valid-instr i::pair st::pair-nil)
      (with-default-value env (values (list (instantiate::instruction
                                             (opcode 'error)
                                             (intype '())
                                             (outtype '(poly))
                                             (parent (-> env parent))))
                                      '(poly)) `(at-instruction ,i)
            (if (adhoc-instr? (car i))
                (adhoc-instr env i st)
                (multiple-value-bind (t i tl) (typeof-instr/instr/tl env i st)
                   (multiple-value-bind (tl st) (valid-instrs env tl st)
                      (for-each (lambda (x) (local-init! env x)) (cddr t))
                      (let ((st (check-stack env st (car t))))
                             ; t : ... -> (poly) ?
                         (if (and (not (null? (cadr t))) (eq? 'poly (caadr t)))
                             ; try to avoid this append (cps by hand ?)
                             (values (append tl (list i)) '(poly))
                             (values (append tl (list i))
                                     (append (reverse (cadr t)) st)))))))))
   (cond
    ((null? l) (values '() st))
    ((pair? (car l))
     (multiple-value-bind (is st) (valid-instr (car l) st)
        (multiple-value-bind (tl st) (valid-instrs env (cdr l) st)
           (values (append is tl) st))))
    ((symbol? (car l))
     (multiple-value-bind (is st) (valid-instr (list (car l)) st)
        (multiple-value-bind (tl st) (valid-instrs env (cdr l) st)
           (values (append is tl) st))))
    (else (raise `(at-pos ,(cer l) expected-instruction ,(car l))))))

(define (valid-expr env::env l::pair-nil)
   (valid-instrs env l '()))

;; section 3.5.3
(define (valid-loct::local-var env::env t)
   (let ((vt (valid-vt env t)))
      (instantiate::local-var (init? (defaultable? vt)) (type vt))))

;; section 6.6.4
(define (valid-names/local/get-tl env::env l::pair-nil)
   (match-case l
      (((local (and (? ident?) ?id) ?vt) . ?tl)
       (multiple-value-bind (n l tl) (valid-names/local/get-tl env tl)
          (values (cons id n) (cons (valid-loct env vt) l) tl)))
      (((local . ?vts) . ?tl)
       (multiple-value-bind (n l tl) (valid-names/local/get-tl env tl)
          (values (append (make-list (length vts) #f) n)
                  (append (map-env valid-loct env vts) l) tl)))
      (else (values '() '() l))))

;; section 6.6.3 and 3.5.12
(define (valid-importdesc env::env d imp::import)
   (match-case d
      ((func (and (? ident?) ?id) . ?rst)
       (func-add-name! env id)
       (valid-importdesc env `(func ,@rst) imp))
      ((memory (and (? ident?) ?id) . ?rst)
       (mem-add-name! env id)
       (valid-importdesc env `(memory ,@rst) imp))
      ((global (and (? ident?) ?id) . ?rst)
       (global-add-name! env id)
       (valid-importdesc env `(global ,@rst) imp))
      ((tag (and (? ident?) ?id) . ?rst)
       (tag-add-name! env id)
       (valid-importdesc env `(tag ,@rst) imp))

      ((func . ?ft)
       (multiple-value-bind (p r) (valid-param/result env ft)
          (vector-set! *funcs* (-> env nfunc) #f)
          (let ((t `(deftype -1 ((sub final (func ,p ,r))) ,0)))
             (func-add! env t)
             (duplicate::import-func imp (deftype t)))))
      ((global ?gt)
       (vector-set! *globals* (-> env nglobal) #f)
       (let ((gt (valid-gt env gt)))
          (global-add! env gt)
          (duplicate::import-global imp (globaltype gt))))
      ((memory . ?mt)
       (let ((mt::memory (valid-mt env mt)))
          (mem-add! env mt)
          (duplicate::import-mem imp (memtype mt))))
      ((tag . ?tt)
       (multiple-value-bind (p r) (valid-param/result env tt)
          (let ((t `(deftype -1 ((sub final (func ,p ,r))) ,0)))
             (tag-add! env t)
             (duplicate::import-tag imp (tagtype t)))))
      (else (raise `(expected-importdesc ,d)))))

; section 6.6.9
(define (valid-exportdesc env::env d)
   (instantiate::export
    (name (car d))
    (idx
     (match-case (cdr d)
        ((func ?id) (funcidx env id))
        ((memory ?id) (memidx env id))
        ((global ?id) (globalidx env id))
        ((tag ?id) (tagidx env id))
        (else (raise `(expected-exportdesc ,d)))))))

(define (map-pos f::procedure l)
   (if (null? l)
       '()
       (cons (cons (f (car l)) (cer l)) (map-pos f (cdr l)))))

(define (type-pass-mf env::env m)
   (with-handler
      (match-lambda
         ((in-module ?- ?e) (raise `(in-module ,m ,e)))
         (?e
           (if (isa? e &error)
               (raise e))
          (raise `(in-module ,m ,e))))
      (match-case m
         ; first abreviation of 6.4.9
         ((type . ?-) (type-pass-mf env `(rec ,m)))
         ((rec . ?l)
          (let ((x (-> env ntype)))
             (valid-rect env (clean-mod-rectype! env l x) x)
             #f))
         (else m))))

(define (decorate p::pair l::pair)
   (cons l p))

(define *funcs* (make-vector 1000000))
(define *globals* (make-vector 1000000))
(define *data* (make-vector 1000000))
(define *exports* '())
(define *imports* '())
(define *declared-funcrefs* '())

(define (env-pass-mf env::env m)
   (with-default-value env #f `(at-pos ,(cdr m))
      (match-case (car m)
         (#f #f)
         ; section 6.6.4 (abbreviations)
         ((func (export (and (? string?) ?exp)) . ?rst)
          (env-pass-mf env (decorate (cdr m)
                                     `(export ,exp (func ,(-> env nfunc)))))
          (env-pass-mf env (decorate (cdr m) `(func ,@rst))))
         ((func (import (and (? string?) ?nm1) (and (? string?) ?nm2)) . ?rst)
          (env-pass-mf env (decorate (cdr m) `(import ,nm1 ,nm2 (func ,@rst)))))
         ; section 6.6.4
         ((func (and (? ident?) ?id) . ?rst)
          (func-add-name! env id)
          (env-pass-mf env (decorate (cdr m) `(func ,@rst))))
         ((func . ?rst)
          (multiple-value-bind (args f tl) (valid-tu/get-tl env rst)
             (vector-set! *funcs* (-> env nfunc)
                          (instantiate::func
                           (type f)
                           (formals args)
                           (body tl)
                           (locals '())
                           (pos (cdr m))))
             (func-add! env `(deftype -1 ((sub final (func ,@f))) 0))))

         ((data (and (? ident?) ?id) (memory ?memidx) (offset . ?expr) . ?-)
          (raise 'todo))
          ; section 6.6.12
         ((data (and (? ident?) ?id) . ?rst)
          (hashtable-put! (-> env data-table) id (-> env ndata))
          (env-pass-mf env (decorate (cdr m) `(data ,@rst))))
         ((data . ?rst)
          (for-each (lambda (s) (unless (string? s)
                                   (raise `(expected-string ,s)))) rst)
          ; section 3.5.9 - passive data segments are always valid, we currently
          ; only support those
          (vector-set! *data* (-> env ndata)
                       (instantiate::data (data (apply string-append rst))))
          (set! (-> env ndata) (+ 1 (-> env ndata))))

          ; section 6.6.7
         ((global (and (? ident?) ?id) . ?rst)
          (global-add-name! env id)
          (env-pass-mf env (decorate (cdr m) `(global ,@rst))))
         ((global (export (and (? string?) ?exp)) . ?rst)
          (env-pass-mf env (decorate (cdr m)
                                     `(export ,exp (global ,(-> env nglobal)))))
          (env-pass-mf env (decorate (cdr m) `(global ,@rst))))
         ((global (import (and (? string?) ?nm1) (and (? string?) ?nm2)) . ?rst)
          (env-pass-mf env (decorate (cdr m)
                                     `(import ,nm1 ,nm2 (global ,@rst)))))
         ((global ?gt . ?e)
          (define (add-func-refs! l)
             (cond ((pair? l)
                    (if (eq? 'ref.func (car l))
                        (hashtable-put! (-> env refs) (cadr l) #t)
                        (for-each add-func-refs! l)))))

          (let ((t (valid-gt env gt)))
             (vector-set! *globals* (-> env nglobal)
                          (instantiate::global (type t) (body e) (pos (cdr m))))
             (global-add! env t)
             (add-func-refs! e)))

         ; section 6.6.3
         ((import (and (? string?) ?mod) (and (? string?) ?name) ?d)
          (set! *imports*
                (cons
                 (valid-importdesc env d (instantiate::import
                                          (mod mod) (name name)))
                 *imports*)))

         ; we don't have all the indices yet, so we can't validate an
         ; exportdesc's
         ((export (and ?nm (? string?)) ?d)
          (set! *exports* (cons (cons nm d) *exports*)))

         ; section 6.6.8 and 3.5.7
         ((tag (and (? ident?) ?id) . ?rst)
          (tag-add-name! env id)
          (env-pass-mf env (decorate (cdr m) `(tag ,@rst))))
         ((tag . ?tu)
          (multiple-value-bind (p r) (valid-param/result env tu)
             (unless (null? r)
                (tag-add! env '(deftype -1 ((sub final (error))) ,0))
                (raise `(non-empty-tag-result ,r)))
             (tag-add! env `(deftype -1 ((sub final (func ,p ,r))) ,0))))

         ; section 6.6.6
         ((memory (and (? ident?) ?id) . ?rst)
          (mem-add-name! env id)
          (env-pass-mf env (decorate (cdr m) `(memory ,@rst))))
         ((memory (export (and (? string?) ?exp)) . ?rst)
          (env-pass-mf env (decorate (cdr m)
                                     `(export ,exp (memory ,(-> env nglobal)))))
          (env-pass-mf env (decorate (cdr m) `(memory ,@rst))))
         ((memory (import (and (? string?) ?nm1) (and (? string?) ?nm2)) . ?rst)
          (env-pass-mf env (decorate (cdr m)
                                     `(import ,nm1 ,nm2 (memory ,@rst)))))
         ((memory . ?mt)
          (mem-add! env (valid-mt env mt)))

         ; section 6.6.11
         ((elem declare func . ?funcs)
          (set! *declared-funcrefs* (append funcs *declared-funcrefs*))
          (for-each (lambda (x) (hashtable-put! (-> env refs) x #t)) funcs))

         (else
          (raise 'expected-modulefield)))))

(define (valid-global env::env g::global x::long)
   (let ((old-nglobal (-> env nglobal)))
      ; global can only refer to the previous ones
     (set! (-> env nglobal) x)
     (multiple-value-bind (e t') (valid-expr env (-> g body))
        (when (and (length>=? t' 2) (not (eq? 'poly (cadr t'))))
           (raise `(too-much-value-stack ,t')))
        (when (or (null? t') (eq? 'poly (car t')))
           (raise `(missing-value-stack ,(-> g type))))
        (unless (<vt= env (car t') (cadr (-> g type)))
           (raise `(non-matching-globaltype ,(car t') ,(-> g type))))
        (when (non-constant-expr? e)
           (raise `(non-constant-global ,(non-constant-expr? e))))
        (set! (-> env nglobal) old-nglobal)
        (set! (-> g body) e))))

(define (valid-function env::env f::func x::long)
   (multiple-value-bind (n lts body)
      (valid-names/local/get-tl env (-> f body))
      (set! (-> env local-names) (append (-> f formals) n))
      (set! (-> env parent) f)
      (set! (-> env local-types)
       (list->vector (append (map (lambda (vt) (instantiate::local-var
                                                (init? #t) (type vt)))
                                  (car (-> f type))) lts)))
      (set! (-> f locals) (map (lambda (l::local-var) (-> l type)) lts))
      (set! (-> env return) (cadr (-> f type)))
      (set! (-> f body)
       (replace-exception 'empty-stack 'no-return-value-stack
          (check-block env body (cadr (-> f type))
                       `(() ,(cadr (-> f type))))))))

(define (valid-functions env::env a::long b::long)
   (let ((x (-> env nfunc)))
      (do ((i 0 (+fx i 1)))
          ((>=fx (+fx (*fx a i) b) x))
         (let ((f (vector-ref *funcs* (+fx (*fx a i) b))))
            (when f
               (with-handler
                  (lambda (e)
                     (format-exn env
                                 `(at-pos ,(with-access::func f (pos) pos) ,e)))
                  (valid-function env f (+fx (*fx a i) b)))
               (unless (null? (-> env error-list))
                  ;(format-exn env `(at-pos ,(with-access::func f (pos) pos) ""))
                  (for-each (lambda (e) (format-exn env e))
                            (-> env error-list))
                  (set! (-> env error-list) '())))))))

(define (valid-globals env::env)
   (let ((x (-> env nglobal)))
      (do ((i 0 (+fx i 1)))
          ((>=fx i x))
         (let ((g (vector-ref *globals* i)))
           (when g
               (with-handler
                  (lambda (e)
                    (format-exn env `(at-pos
                                      ,(with-access::global g (pos) pos) ,e)))
                  (valid-global env g i))
               (unless (null? (-> env error-list))
                  ;(format-exn env `(at-pos ,(with-access::global g (pos) pos)
                  ;                  ""))
                  (for-each (lambda (e) (format-exn env e))
                            (-> env error-list))
                  (set! (-> env error-list) '())))))))

(define (valid-exports env::env)
   (map! (lambda (d) (valid-exportdesc env d)) *exports*))

(define (sprintf s . args)
   (call-with-output-string
      (lambda (p) (apply fprintf p s args))))

(define (sdisplay o)
   (call-with-output-string
      (lambda (p) (display o p))))

(define (error->string env::env e)
   (if (isa? e &error)
       (raise e))
   (define (type->string::bstring t)
      (cond ((deftype? t) (sdisplay (type-get-name env (cadr t))))
            ((number? t) (sdisplay (type-get-name env t)))
            ((reftype? t)
             (string-append "ref " (if (nullable? t) "null " "")
                            (type->string (reftype->heaptype t))))
            (else (sdisplay t))))

   (match-case e
      ((undeclared-funcref ?x)
       (sprintf "undeclared function reference: ~a" (idx-get-name x env)))

      ((got-packed ?x ?y ?t)
       (sprintf "used struct.get on type ~a on field ~a while it has a packed type (~a)"
                (idx-get-name x env) (fieldidx-get-name env x y) t))

      ((got-packed ?x ?t)
       (sprintf "used array.get on type ~a while its elements have a packed type (~a)"
                (idx-get-name x env) t))

      ((non-matching-stack ?t1 ?t2)
       (sprintf "non matching types on stack: expected ~a got ~a"
                (type->string t2) (type->string t1)))

      ((no-return-value-stack ?t)
       (sprintf "function expected ~a on stack but got nothing"
                (type->string (car t))))

      (((unknown ?x) ?s)
       (sprintf "unknown ~a: ~a" x s))

      (((expected ?x) ?s)
       (sprintf "expected ~a, got ~a" x s))

      ((empty-stack ?t)
       (sprintf "expected ~a on stack but got nothing" (type->string (car t))))

      ((value-left-stack ?t)
       (sprintf "expected empty stack, got a value of type ~a on top"
                (type->string (car t))))

      ((supertype-final ?t1 ?t2)
       (sprintf "~a can't be a supertype of ~a because the first is marked as final"
                (type->string t2) (type->string t1)))

      (else (sdisplay e))))

(define (format-exn env::env e)
   (if (isa? e &error)
       (raise e))
   (set! error-encountered? #t)
   (define (rep msg obj)
     (with-handler error-notify
        (when (epair? obj)
           (error/location "watib" "" msg (cadr (cer obj)) (caddr (cer obj))))))
   (define (rep/pos msg pos)
     (with-handler error-notify
        (error/location "watib" "" msg (cadr pos) (caddr pos))))

   (match-case e
      ((in-module ?m ?e)
       (if silent
           (fprint (current-error-port) (error->string env e))
           (rep/pos (error->string env e) (cer m))))
      ((at-pos ?i (at-instruction . ?-))
       (format-exn env (caddr e)))
      ((at-pos ?p ?e)
       (if silent
           (fprint (current-error-port) (error->string env e))
           (rep/pos (error->string env e) p)))
      ((at-instruction ?i (at-instruction . ?-))
       (format-exn env (caddr e)))
      ((at-instruction ?i ?e)
       (if silent
           (fprint (current-error-port) (error->string env e))
           (rep (error->string env e) i)))

      (else
       (when (number? keep-going)
          (set! keep-going (-fx keep-going 1))
          (if (=fx 0 keep-going)
              (set! keep-going #f)))
       (display "***ERROR: " (current-error-port))
       (display e (current-error-port))
       (newline (current-error-port))))
   (unless keep-going
      (raise
     (instantiate::&watlib-validate-error
        (proc "watlib-validate")
        (msg "validation error")
        (obj e)))))

(cond-expand
   ((and multijob (library pthread))
;;;
(define (multijob env::env)
   (define (dupenv)
      (duplicate::env env (label-types (make-vector 10000))))

   (let ((ts (cons
              (instantiate::pthread
               (body
                (lambda ()
                  (valid-globals (dupenv))
                  (valid-functions (dupenv) nthreads 0))))
              (cons
               (instantiate::pthread
                (body
                 (lambda ()
                   (valid-exports (dupenv))
                   (map! (lambda (x) (func-get-index env x))
                         *declared-funcrefs*)
                   (valid-functions (dupenv) nthreads 1))))
               (list-tabulate
                (-fx nthreads 2)
                (lambda (i)
                  (instantiate::pthread
                   (body
                    (lambda ()
                      (valid-functions (dupenv) nthreads (+fx 2 i)))))))))))
     (map thread-start-joinable! ts)
     (map thread-join! ts)))))

(define (singlejob env::env)
   ; todo add position for funcref checking
   (map! (lambda (x) (func-get-index env x))
      *declared-funcrefs*)
   (valid-globals env)
   (valid-exports env)
   (valid-functions env 1 0))

(define (valid-file f::pair-nil nt::long kg::obj s::bool)
   (set! nthreads nt)
   (set! keep-going kg)
   (set! silent s)

   (let ((env::env (instantiate::env)))
      (define (mf-pass/handle-error f)
         (lambda (m)
            (let ((clean-m (with-handler (lambda (e) (format-exn env e))
                                         (f env m))))
               (unless (null? (-> env error-list))
                 (format-exn env `(in-module ,m ""))
                 (for-each (lambda (e) (format-exn env e)) (-> env error-list))
                 (set! (-> env error-list) '()))
               clean-m)))

      (match-case f
         ((or (module (? ident?) . ?mfs) (module . ?mfs))
          (let* ((type-mfs (map-pos (mf-pass/handle-error type-pass-mf) mfs)))
             (for-each (mf-pass/handle-error env-pass-mf) type-mfs)
         (cond-expand
        ((and multijob (library pthread))
         (if (= nthreads 1)
             (singlejob env)
             (multijob env)))
        (else
         (singlejob env))))))
      (unless error-encountered?
     (instantiate::prog
        (exports *exports*)
        (env env)
        (data *data*)
        (funcs *funcs*)
        (funcrefs *declared-funcrefs*)
        (imports *imports*)
        (globals *globals*)))))
