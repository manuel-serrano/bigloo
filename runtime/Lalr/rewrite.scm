;; ---------------------------------------------------------------------- ;;
;; FICHIER               : rewrite.scm                                    ;;
;; DATE DE CREATION      : Thu Jun 29 15:23:05 1995                       ;;
;; DERNIERE MODIFICATION : Mon Nov 27 14:54:41 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Fonctions pour la réécriture de la grammaire ...                       ;;
;; ---------------------------------------------------------------------- ;;

(module __lalr_rewrite

   (import __error
	   __lalr_global
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

   (export (rewrite-grammar! g)
	   (clean-plist)
	   *symv*))

;; ---------------------------------------------------------------------- ;;
;; On garde une liste de tous les symboles de la grammaire. À la fin, il  ;;
;; faut enlever les propriétés attachées à ces symboles ...               ;;
;; ---------------------------------------------------------------------- ;;
(define *plist* #f)

(define (clean-plist)
  (let loop ((l *plist*))
    (if (pair? l)
	(let ((sym (car l)))
	  (if (getprop sym 'nt?)
	      (remprop! sym 'nt?))
	  (if (getprop sym 'prec)
	      (remprop! sym 'prec))
	  (remprop! sym 'sym-no)
	  (loop (cdr l)))
	#f)))

 
(define *symv* #f) 
(define (make-sym-table)
  (set! *symv* (make-vector *max-term* #f))
  (let loop ((l *plist*))
    (if (pair? l)
	(let ((sym (car l)))
	  (vector-set! *symv* (getprop sym 'sym-no) sym)
	  (loop (cdr l))))))

;; ---------------------------------------------------------------------- ;;
;; Gestion des listes de propriétés et assignation d'un numéro à chaque   ;;
;; symbole ...                                                            ;;
;; ---------------------------------------------------------------------- ;;
(define *max-term* 0)
(define *max-nt* 1)

(define (set-nt-no! sym)
  (let ((x *max-nt*))
    (putprop! sym 'nt? #t)
    (putprop! sym 'sym-no *max-nt*)
    (set! *max-nt* (+ *max-nt* 1))
    (set! *plist* (cons sym *plist*))
    x))

(define (set-sym-no! sym)
  (let ((x (getprop sym 'sym-no)))
    (if x
	(error 'lalr-grammar "Grammar symbol already defined" sym)
	(let ((y *max-term*))
	  (putprop! sym 'sym-no *max-term*) 
	  (set! *plist* (cons sym *plist*))
	  (set! *max-term* (+ *max-term* 1))
	  y))))

(define (assert-symbol! sym)
  (if (not (getprop sym 'sym-no))
      (error 'lalr-grammar "Undefined symbol" sym)))

(define (symbol->symbol/binding sym)
  (let loop ((l (string->list (symbol->string sym)))
	     (prefix '()))
    (if (null? l)
	sym
	(let ((c (car l))
	      (r (cdr l)))
	  (if (char=? c #\@)
	      (if (null? r)
		  sym
		  (cons (string->symbol (list->string (reverse prefix)))
			(string->symbol (list->string r))))
	    (loop r (cons c prefix)))))))
    

;; ---------------------------------------------------------------------- ;;
;; On réécrit la grammaire ...                                            ;;
;; ---------------------------------------------------------------------- ;;
(define (rewrite-grammar! gram)
  (define the-rules (cdr gram))
  (define terminals (car gram))
  
  (define no-items 0)
  (define no-rules 0)
  (define prec-level 0)

  (set! *plist* '(*eoi* *start*))
  (set! *max-nt* 1)

;; --- On détermine les non-terminaux ... ------------------------------- ;;
  (let loop ((l the-rules))
    (if (pair? l)
	(let ((prods (car l)))
	  (if (pair? prods)
	      (let ((lhs (car prods)))
		(if (symbol? lhs)
		    (if (getprop lhs 'nt?)
			(error 'lalr-grammar
			       "Non-terminal defined twice"
			       lhs)
			(begin
			  (set-nt-no! lhs)
			  (loop (cdr l))))
		    (error 'lalr-grammar
			   "LHS must be a symbol"
			   lhs)))
	      (error 'lalr-grammar
		     "Bad rule specification"
		     prods)))
	(if (not (null? l))
	    (error 'lalr-grammar "Ill-formed grammar" l))))

  (set! *max-term* (+ *max-nt* 1))
  (putprop! '*eoi* 'sym-no *max-nt*)
  (putprop! '*start* 'sym-no 0)
  (putprop! '*start* 'nt? #t)
  
;; --- On numérote les terminaux ... ------------------------------------ ;;
;; Also, assign precedence to terminals which have been grouped in a      ;;
;; (left:/right:/none: ...) precedence grouping.			  ;;
;; ---------------------------------------------------------------------- ;;
  (for-each
    (lambda (t)
      (cond
	((symbol? t)
	 (set-sym-no! t))
	((and (pair? t) (memq (car t) '(left: right: none:)))
	 (set! prec-level (+fx prec-level 1))
	 (for-each
	   (lambda (inner-t)
	     (if (not (symbol? inner-t))
	       (error 'lalr-grammar "Bad terminal" inner-t))
	     (set-sym-no! inner-t)
	     (putprop! inner-t 'prec (cons (car t) prec-level)))
	   (cdr t)))
	(else
	 (error 'lalr-grammar "Bad terminal" t))))
    terminals)
  
;; --- On remplace les `foo@bar' par `(foo . bar)' ---------------------- ;;
;; --- et on numérote ... ----------------------------------------------- ;;
  (let loop ((l the-rules))
    (if (pair? l)
	(let* ((rules (car l))
	       (lhs   (car rules)))
	  (let loop-1 ((rhs-l (cdr rules)))
	    (cond
	     ((null? rhs-l)
	      (loop (cdr l)))
	     ((pair? rhs-l)
	      (let ((rhs/action (car rhs-l)))
		(set! no-rules (+ no-rules 1))
		(if (pair? rhs/action)
		    (let ((action (cdr rhs/action)))
		      (if (not (list? action))
			  (error 'lalr-grammar "Invalid semantic action" rhs/action))
		      (if (null? action)
			  (set-cdr! rhs/action '(#f)))
		      (let loop-2 ((rhs (car rhs/action)))
			(cond 
			 ((null? rhs)
			  (set! no-items 
				(+ no-items (length (car rhs/action)) 1))
			  (loop-1 (cdr rhs-l)))
			 ((pair? rhs)
			  (let ((sym (car rhs)))
			    (if (symbol? sym)
				(let* ((sym/var (symbol->symbol/binding sym))
				       (the-sym (if (symbol? sym/var)
						    sym/var
						    (car sym/var))))
				  (set-car! rhs sym/var)
				  (assert-symbol! the-sym)
				  (loop-2 (cdr rhs)))
				(error 'lalr-grammar
				       "Invalid symbol in right-hand side"
				       rhs))))
			 (else
			  (error 'lalr-grammar
				 "Bad right-hand side"
				 rhs))))))))
	     (else
	      (error 'lalr-grammar "Bad rule specification" l)))))))
  
;; --- On ajoute une production à la grammaire et ... ------------------- ;;
;; --- on initialise quelques variables globales ... -------------------- ;;
  (make-sym-table)
  (let ((start-sym (caar the-rules)))
    (set! grammar
	  (cons `(*start* ((,start-sym *eoi*) ,start-sym))
		the-rules)))
  (set! nrules (+ no-rules 2))
  (set! nitems (+ no-items 3))
  (set! nterms *max-term*) 
  (set! nvars  *max-nt*)
  (set! nsyms  (+ nterms nvars)))
		 

