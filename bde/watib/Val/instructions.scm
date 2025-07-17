;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Some parametric validation rules, to avoid code duplication.

(module val_instructions
   (import (env_env "Env/env.scm")
           (ast_node "Ast/node.scm"))

   (export (load-instruction::procedure t::symbol)
           (store-instruction::procedure t::symbol)))

(define (load-instruction::procedure t::symbol)
   (lambda (env::env)
       (unless (>=fx (-> env nmem) 1)
          (raise 'no-declared-memory))
       (with-access::memory (vector-ref (-> env mem-types) 0) (at)
          `((,at) (,t)))))

(define (store-instruction::procedure t::symbol)
   (lambda (env::env)
       (unless (>=fx (-> env nmem) 1)
          (raise 'no-declared-memory))
       (with-access::memory (vector-ref (-> env mem-types) 0) (at)
          `((,at ,t) ()))))
