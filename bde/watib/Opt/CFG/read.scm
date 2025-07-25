;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Read CFGs for testing purposes.
;;
;; A CFG is a prelude followed by a type delcaration for the CFG and a
;; sequence of nodes. Such a graph corresponds to a Wasm function
;;
;; A node is of the form:
;; (node <name> (param <vt>*)
;;   (body <instr>*)
;;   (end <jump>))
;;
;; And a jump is of the form:
;; | (goto <name>)
;; | (if <name> <name>)
;; | (terminal <instr>)
;; | (on-cast <name> <name> <rt1> <rt2>)
;; | (switch <name>+) --- the last is the default label
;;
;; The prelude is of the form (prelude <module-decl>*), where <module-decl>
;; refers to the usual Wasm module declaration. It can be used to declare types
;; for instance. The type declaration of the CFG is of the form:
;; (cfg (param <name> <vt>)*
;;      (result <vt>*)*
;;      (local <name> <vt>)*
;;      (entry <name>)).
;; Parameters, results and local variables can be specified like in a Wasm
;; function.
;;
;; Warning: we it does handle yet all the necessary checks for reducibility and
;; type compatibility

(module cfg_read
   (library srfi1)

   (import (ast_node      "Ast/node.scm")
           (env_env       "Env/env.scm")
           (val_validate  "Val/validate.scm")
           (type_match    "Type/match.scm")
           (cfg_order     "Opt/CFG/order.scm")
           (cfg_walk      "Opt/CFG/walk.scm")
           (cfg_dominance "Opt/CFG/dominance.scm"))

   (from (cfg_node "Opt/CFG/node.scm"))

   (export (read-cfg/prog ip::input-port)))

(define (read-jump::jump j nodes env::env src-outtype::pair-nil)
   (match-case j
      ((goto ?n)
       (instantiate::unconditional (dst (hashtable-get nodes n))))

      ((if ?t ?f)
       (instantiate::conditional (dst-true (hashtable-get nodes t))
                                 (dst-false (hashtable-get nodes f))))

      ((terminal ?i)
       (multiple-value-bind (i st)
          (valid-instrs env (econs i '() (cer j)) src-outtype)
          (unless (and (null? (cdr i)) (equal? st '(poly)))
             (error/location "watib" "invalid return" j
                             (cadr (cer j)) (caddr (cer j))))
          (instantiate::terminal (i (car i)))))

      ((on-cast ?cast ?cast-fail ?rt1 ?rt2)
       (let ((rt-src (valid-rt env rt1))
             (rt-dst (valid-rt env rt2)))
          (unless (<rt= env rt-dst rt-src)
             (error/location "watib" "invalid br_on_cast, first type should be a subtype of the second"
                             (cons rt-dst rt-src) (cadr (cer j))
                             (caddr (cer j))))
          (unless (and (not (null? src-outtype))
                       (<rt= env (last src-outtype) rt-src))
             (error/location "watib" "invalid br_on_cast, expected a value on stack of type"
                             rt-src (cadr (cer j)) (caddr (cer j))))
          (instantiate::on-cast
           (dst-cast (hashtable-get nodes cast))
           (dst-cast-fail (hashtable-get nodes cast-fail))
           (rt-src rt-src)
           (rt-dst rt-dst))))

      ((switch . ?labels)
       (instantiate::switch
        (dsts (map (lambda (l) (hashtable-get nodes l)) labels))))

      (else (error "watib" "expected jump got" j))))

(define (read-node::cfg-node n nodes env::env)
   (match-case n
      ((node ?- (param . ?vts)
         (body . ?instrs)
         (end ?j))
       (let ((intype (map (lambda (t) (valid-vt env t)) vts)))
          (multiple-value-bind (i outtype) (valid-instrs env instrs intype)
             (let ((end (read-jump j nodes env outtype)))
                (instantiate::cfg-node
                 (intype intype)
                 (outtype (reverse (remove-top-outtype end outtype)))
                 (body i)
                 (end end))))))
      (else (error "watib" "expected node got" n))))

(define (read-cfg/prog ip::input-port)
   (let* ((p::prog (match-case (read ip #t)
                      ((prelude . ?m) (valid-file `(module ,@m) 1 #f #f))
                      (?x (error "watib" "expected prelude got" x))))
          (env::env (-> p env))
          (entry-name #f)
          (f #f))
      (match-case (read ip #t)
         ((cfg . ?l)
          (multiple-value-bind (formals tu tl) (valid-tu/get-tl env l)
             (multiple-value-bind (lnames lts entry)
                (valid-names/local/get-tl env tl)
                (match-case entry
                   (((entry ?name))
                    (set! (-> env local-names) (append formals lnames))
                    (set! (-> env local-types)
                          (list->vector
                           (map (lambda (t) (instantiate::local-var
                                             (type t)
                                             (init? #t)))
                                (append (car tu)
                                        (map (lambda (x::local-var)
                                               (-> x type)) lts)))))
                    (set! (-> env return) (cadr tu))
                    (set! entry-name name)
                    (let ((t (econs 'deftype
                                    (list `((sub final (func ,@tu))) 0) -1)))
                      (func-add! env t))
                    (set! f (instantiate::func
                             (body #f)
                             (formals formals)
                             (type tu)
                             (locals (map (lambda (x::local-var)
                                               (-> x type)) lts))
                             (pos '(cfg)))))
                   (?x (error "watib" "expected entry got" x))))))
         (?x (error "watib" "expected cfg declaration got" x)))
      (let* ((nodes (make-hashtable))
             (nodes-list::pair-nil
              (unfold eof-object?
                      (match-lambda
                         ((and (node ?name . ?tl) ?n)
                          (hashtable-put! nodes name (make-dummy-node))
                          n)
                         (?x (error "watib" "expected node got" x)))
                          (lambda (-) (read ip #t)) (read ip #t))))
         (for-each (lambda (name n::cfg-node)
                     (let ((dummy::cfg-node (hashtable-get nodes name)))
                        (set! (-> dummy intype) (-> n intype))
                        (set! (-> dummy outtype) (-> n outtype))
                        (set! (-> dummy end) (-> n end))
                        (set! (-> dummy body) (-> n body))))
                   (map cadr nodes-list)
                   (map (lambda (n) (read-node n nodes env)) nodes-list))

         (let ((entry (hashtable-get nodes entry-name)))
            (multiple-value-bind (-size rpostorder) (reverse-postorder! entry)
               (let ((g::cfg (instantiate::cfg
                              (entry entry)
                              (size (-fx 0 -size))
                              (rpostorder rpostorder)
                              (func f))))
                  (with-access::func f (body)
                     (set! body (cfg->wasm g))
                     (vector-set! (-> p funcs) (-fx (-> env nfunc) 1) f)
                     (set! (-> p exports)
                           (cons (instantiate::export
                                  (name "cfg")
                                  (idx (instantiate::funcidxp
                                        (idx (-fx (-> env nfunc) 1))
                                        (type '(useless)))))
                                 (-> p exports)))
                     (values g p))))))))
