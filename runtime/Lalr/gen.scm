;; ---------------------------------------------------------------------- ;;
;; FICHIER               : gen.scm                                        ;;
;; DATE DE CREATION      : Mon Jul  3 14:04:24 1995                       ;;
;; DERNIERE MODIFICATION : Tue Jul  4 13:40:53 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Génération du code ...                                                 ;;
;; ---------------------------------------------------------------------- ;;

(module __lalr_gen
   
   (include "Lalr/lalr.sch")
   
   (import __error
	   __lalr_global
	   __lalr_rewrite
	   
	   __type
	   __bigloo
	   __tvector
	   __structure
	   __tvector
	   __param
	   __bexit
	   __bignum
	   __object
	   __thread
	   __rgc
	   __bit
	   
	   __r4_numbers_6_5
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_numbers_6_5_flonum_dtoa
	   __r4_characters_6_6
	   __r4_equivalence_6_2
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_strings_6_7
	   __r4_pairs_and_lists_6_3
	   __r4_input_6_10_2
	   __r4_control_features_6_9
	   __r4_vectors_6_8
	   __r4_ports_6_10_1
	   __r4_output_6_10_3
	   __evenv)
   
   (export (gen-lalr-code)))

(define (gen-lalr-code)
  `(let* (,(gen-action-table)
	  ,(gen-goto-table)
	  ,(gen-push-action)
	  ,(gen-reduction-table))
     (__make-parser __action-table __reduce)))


(define (gen-push-action)
   '(__push (lambda (stack sp new-cat goto-table lval)
	       (let* ((state     (vector-ref stack sp))
		      (new-state (cdr (assq new-cat
					    (vector-ref goto-table state))))
		      (new-sp    (+fx sp 2)))
		  (vector-set! stack new-sp new-state)
		  (vector-set! stack (-fx new-sp 1) lval)
		  new-sp))))

(define (gen-action-table)
  (define (actions)
    (let loop-a ((i 0))
      (if (= i nstates)
	  '()
	  (cons
	   (let loop ((l (vector-ref action-table i)))
	     (if (null? l)
		 '()
		 (let* ((p (car l)) (x (car p)) (y (cdr p)))
		   (cons
		    (cons
		     (if (fixnum? x) (vector-ref *symv* (+fx nvars x)) x)
		     y)
		    (loop (cdr l))))))
	   (loop-a (+fx i 1))))))

  `(__action-table '#(,@(actions))))

(define (gen-goto-table)
  (define (gotos)
    (let loop-g ((i 0))
      (if (= i nstates)
	  '()
	  (cons 
	   (let ((shifts (vector-ref shift-table i)))
	     (if shifts
		 (let loop ((l (shift-shifts shifts)))
		   (if (null? l)
		       '()
		       (let* ((state (car l))
			      (symbol (vector-ref acces-symbol state)))
			 (if (<fx symbol nvars)
			     (cons
			      `(,(vector-ref *symv* symbol) . ,state)
			      (loop (cdr l)))
			     (loop (cdr l))))))
		 '()))
	   (loop-g (+fx i 1))))))
  
  `(__goto-table '#(,@(gotos))))

(define (gen-reduction-table)
  (define (bindings rhs act n) 
    (let loop ((i n) (l rhs))
      (if (null? l)
	  '()
	  (let ((sym (car l)))
	    (cons
	     `(,(if (pair? sym) (cdr sym) sym)
	       (vector-ref-ur __stack (-fx __sp ,(-fx (*fx i 2) 1))))
	     (loop (-fx i 1) (cdr l)))))))
  
  (define (action nt n act)
    (if (eq? nt '*start*)
	(vector-ref *symv* 1)
	`(__push __stack (-fx __sp ,(*fx 2 n)) 
	       ',nt
	       __goto-table
	       (let () ,@act))))

  (define (reductions)
    (let loop-l ((l grammar) (no 1))
      (if (null? l)
	  '()
	  (let* ((def (car l))
		 (nt (car def)))
	    (let loop-p ((prods (cdr def)) (no no))
	      (if (null? prods)
		  (loop-l (cdr l) no)
		  (let* ((rhs (caar prods)) (act (cdar prods)) (n (length rhs)))
		    (cons 
		     (list
		      (list no)
		      `(let (,@(bindings rhs act n))
			  ,(action nt n act)))
		     (loop-p (cdr prods) (+fx no 1))))))))))
  
  
  `(__reduce (lambda (n __stack __sp) 
	       (case n
		 ,@(reductions)))))
