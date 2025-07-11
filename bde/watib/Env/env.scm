(module env_env
   (from (ast_node "Ast/node.scm"))
   (import (type_type "Type/type.scm"))
   (import (misc_parse "Misc/parse.scm"))
   (export (class local-var::object
              type
              init?::bool)

           (type-get-index::bint env::env x)
           (type-get env::env x)
           (type-get-name env::env x::bint)
           (add-type! env::env nm t)
           (set-type! env::env id t)
           (typeidx::typeidxp env::env x)

           (func-get-index::bint env::env x)
           (func-get-type env::env x::bint)
           (func-get-name env::env x::bint)
           (func-add! env::env t)
           (func-add-name! env::env id::symbol)
           (funcidx::funcidxp env::env x)

           (global-get-index::bint env::env x)
           (global-get-type env::env x::bint)
           (global-add! env::env t)
           (global-add-name! env::env id::symbol)
           (globalidx::globalidxp env::env x)

           (tag-get-index::bint env::env x)
           (tag-get-type env::env x::bint)
           (tag-add! env::env t)
           (tag-add-name! env::env id::symbol)
           (tagidx::tagidxp env::env x)

           (mem-get-index::bint env::env x)
           (mem-get-type env::env x::bint)
           (mem-add! env::env t)
           (mem-add-name! env::env id::symbol)
           (memidx::memidxp env::env x)

           (data-get-index::bint env::env x)
           (dataidx::dataidxp env::env x)

           (field-get-index::bint env::env t::bint f)
           (fieldidx-get-name env::env x::typeidxp y::fieldidxp)
           (fieldidx::fieldidxp env::env x)

           (local-get-index env::env l)
           (local-init! env::env l::bint)
           (local-init?::bool env::env l)
           (local-get-type env::env l)
           (localidx::localidxp env::env x)

           (label-get-index::bint env::env x)
           (label-get-type env::env x::bint)
           (push-label! env::env nm t::pair-nil)
           (pop-label! env::env)
           (labelidx::labelidxp env::env x)

           (generic idx-get-name x::idxp env::env)))

(define (get-index::bint table range::bint x ex-oor ex-unkwn ex-exp)
   (cond ((number? x)
          (if (<fx x range)
              x
              (raise (list ex-oor range x))))
         ((ident? x)
          (if (hashtable-contains? table x)
              (hashtable-get table x)
              (raise (list ex-unkwn x))))
         (else (raise (list ex-exp x)))))

(define (index::long lst::pair-nil x i::long e)
      (cond ((null? lst) (raise (list e x)))
            ((equal? (car lst) x) i)
            (else (index (cdr lst) x (+fx 1 i) e))))

(define (type-get-index::bint env::env x)
   (get-index (-> env type-table) (-> env ntype) x '(idx-out-of-range type)
              '(unknown type) '(expected-idx type)))

(define (type-get env::env x)
   (vector-ref (-> env types) (type-get-index env x)))

(define (type-get-name env::env x::bint)
   (let ((nm (vector-ref (-> env type-names) x)))
      (if nm nm x)))

(define-generic (idx-get-name x::idxp env::env)
   "")

(define-method (idx-get-name x::typeidxp env::env)
   (type-get-name env (-> x idx)))

(define-method (idx-get-name x::funcidxp env::env)
   (func-get-name env (-> x idx)))

(define (add-type! env::env nm t)
   (let ((x (-> env ntype)))
      (set! (-> env ntype) (+fx 1 x))
      (when nm
         (hashtable-put! (-> env type-table) nm x))
      (vector-set! (-> env type-names) x nm)
      (vector-set! (-> env types) x t)))

(define (set-type! env::env id t)
   (vector-set! (-> env types) (type-get-index env id) t))

(define-macro (table-boilerplate x)
   `(begin
       (define (,(symbol-append x '-get-index::bint) env::env x)
          (get-index (-> env ,(symbol-append x '-table))
                     (-> env ,(symbol-append 'n x)) x
                     '(idx-out-of-range ,x) '(unknown ,x) '(expected-idx ,x)))

       (define (,(symbol-append x '-get-type) env::env x::bint)
          (vector-ref (-> env ,(symbol-append x '-types)) x))

       (define (,(symbol-append x '-add!) env::env t)
          (let ((x (-> env ,(symbol-append 'n x))))
             (vector-set! (-> env ,(symbol-append x '-types)) x t)
             (set! (-> env ,(symbol-append 'n x)) (+fx 1 x))))

       (define (,(symbol-append x '-add-name!) env::env id::symbol)
          (when (hashtable-contains? (-> env ,(symbol-append x '-table)) id)
             (raise `(name-already-used ,id)))
          (hashtable-put! (-> env ,(symbol-append x '-table)) id
                          (-> env ,(symbol-append 'n x)))
          (vector-set! (-> env ,(symbol-append x '-names))
                       (-> env ,(symbol-append 'n x)) id))

       (define (,(symbol-append x '-get-name) env::env x::bint)
          (let ((nm (vector-ref (-> env ,(symbol-append x '-names)) x)))
             (if nm nm x)))))

(table-boilerplate func)
(table-boilerplate global)
(table-boilerplate tag)
(table-boilerplate mem)

(define (data-get-index::bint env::env x)
   (get-index (-> env data-table) (-> env ndata) x '(idx-out-range data)
              '(unknown data) '(expected-idx data)))

(define (field-get-index::bint env::env t::bint f)
   (let ((v (vector-ref (-> env field-names) t)))
      (cond
       ((number? f)
        (if (<fx f (length v))
         t
         (raise `((idx-out-of-range field) ,t (length v) ,f))))
    ((ident? f)
     (index v f 0 'unknown-field))
    (#t (raise `(expected-fieldidx ,t))))))

(define (fieldidx-get-name env::env x::typeidxp y::fieldidxp)
   (list-ref (vector-ref (-> env field-names) (-> x idx)) (-> y idx)))

(define (local-get-index env::env l)
   (cond
    ((number? l)
     (if (< l (vector-length (-> env local-types)))
         l
         (raise `(localidx-out-of-range ,l))))
    ((ident? l)
     (index (-> env local-names) l 0 'unknown-local))
    (#t `(expected-local ,l))))

(define (local-init! env::env l::bint)
   (with-access::local-var (vector-ref (-> env local-types) l) ((init? init?))
      (set! init? #t)))

(define (local-init?::bool env::env l)
   (with-access::local-var
      (vector-ref (-> env local-types) (local-get-index env l))
      ((init? init?))
      init?))

(define (local-get-type env::env l)
   (with-access::local-var
      (vector-ref (-> env local-types) (local-get-index env l))
      ((type type))
      type))

(define (label-get-index::bint env::env x)
  (cond
   ((number? x)
    (if (<fx x (-> env nlabel))
        x
        (raise `(labelidx-out-of-range ,x))))
   ((ident? x)
    (index (-> env label-names) x 0 'unknown-label))
   (else `((expected-idx label) ,x))))

(define (label-get-type env::env x::bint)
   (vector-ref (-> env label-types) (- (-> env nlabel) x 1)))

(define (push-label! env::env nm t::pair-nil)
   (let ((x (-> env nlabel)))
      (set! (-> env nlabel) (+fx x 1))
      (set! (-> env label-names) (cons nm (-> env label-names)))
      (vector-set! (-> env label-types) x t)))

(define (pop-label! env::env)
   (set! (-> env nlabel) (-fx (-> env nlabel) 1))
   (set! (-> env label-names) (cdr (-> env label-names))))

;;;;;;;; REPLACE LAST TYPE WITH A TYPEIDX
(define (get-struct-fldts env::env x::bint)
   (match-case (expand (type-get env x))
      ((struct . ?fldts) fldts)
      (?t (raise `(expected-struct ,x ,t)))))

;; to work, needs to be called after typeidx
(define (fieldidx::fieldidxp env::env x)
   (unless (vector-ref (-> env field-names) (-> env last-type))
      (raise `(expected-struct ,(-> env last-type)
               ,(expand (type-get env (-> env last-type))))))
   (let* ((idx (field-get-index env (-> env last-type) x))
          (t (list-ref (get-struct-fldts env (-> env last-type)) idx)))
      (instantiate::fieldidxp (idx idx) (mut? (car t)) (type (cadr t)))))

(define (typeidx::typeidxp env::env x)
   (let ((idx (type-get-index env x)))
      (set! (-> env last-type) idx)
      (instantiate::typeidxp (idx idx) (type (type-get env idx)))))

(define (localidx::localidxp env::env x)
   (let ((idx (local-get-index env x)))
      (with-access::local-var (vector-ref (-> env local-types) idx) (type init?)
         (instantiate::localidxp (idx idx) (init? init?) (type type)))))

(define (labelidx::labelidxp env::env x)
   (let ((idx (label-get-index env x)))
      (instantiate::labelidxp (idx idx) (type (label-get-type env idx)))))

(define (funcidx::funcidxp env::env x)
   (let ((idx (func-get-index env x)))
      (instantiate::funcidxp (idx idx) (type (func-get-type env idx)))))

(define (memidx::memidxp env::env x)
   (instantiate::memidxp (idx (mem-get-index env x))))

(define (tagidx::tagidxp env::env x)
   (let ((idx (tag-get-index env x)))
      (instantiate::tagidxp (idx idx) (type (tag-get-type env idx)))))

(define (globalidx::globalidxp env::env x)
   (let* ((idx (global-get-index env x))
          (t (global-get-type env idx)))
      (instantiate::globalidxp (idx idx) (mut? (car t)) (type (cadr t)))))

(define (dataidx::dataidxp env::env x)
   (instantiate::dataidxp (idx (data-get-index env x))))
