;; ---------------------------------------------------------------------- ;;
;; FICHIER               : util.scm                                       ;;
;; DATE DE CREATION      : Thu Jun 29 15:11:17 1995                       ;;
;; DERNIERE MODIFICATION : Thu Jun 29 15:12:47 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Quelques fonctions utilitaires ....                                    ;;
;; ---------------------------------------------------------------------- ;;

(module __lalr_util

   (import __error
	   __param)

   (use    __type
		
	   __bigloo
	   __tvector
	   __structure
	   __tvector
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
	
   (export (pos-in-list x lst)
	   (sunion lst1 lst2)
	   (sinsert elem lst)
	   (filter p lst)))

(define (pos-in-list x lst)
  (let loop ((lst lst) (i 0))
    (cond ((not (pair? lst))    #f)
	  ((equal? (car lst) x) i)
	  (else                 (loop (cdr lst) (+fx i 1))))))

(define (sunion lst1 lst2)		; union of sorted lists
  (let loop ((L1 lst1)
	     (L2 lst2))
    (cond ((null? L1)    L2)
	  ((null? L2)    L1)
	  (else 
	   (let ((x (car L1)) (y (car L2)))
	     (cond
	      ((>fx x y)
	       (cons y (loop L1 (cdr L2))))
	      ((<fx x y)
	       (cons x (loop (cdr L1) L2)))
	      (else
	       (loop (cdr L1) L2))
	      ))))))

(define (sinsert elem lst)
  (let loop ((l1 lst))
    (if (null? l1) 
	(cons elem l1)
	(let ((x (car l1)))
	  (cond ((<fx elem x)
		 (cons elem l1))
		((>fx elem x)
		 (cons x (loop (cdr l1))))
		(else 
		 l1))))))

(define (filter p lst)
  (let loop ((l lst))
    (if (null? l)
	'()
	(let ((x (car l)) (y (cdr l)))
	(if (p x)
	    (cons x (loop y))
	    (loop y))))))

;------------------------------------------------------------------------------
