;; ---------------------------------------------------------------------- ;;
;; FICHIER               : lalr.scm                                       ;;
;; DATE DE CREATION      : Thu Jun 29 15:07:55 1995                       ;;
;; DERNIERE MODIFICATION : Tue Jul  4 13:04:38 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Un générateur d'analyseurs syntaxiques LALR(1) inspiré de Bison.       ;;
;; ---------------------------------------------------------------------- ;;

(module __lalr_expand
   
   (include "Lalr/lalr.sch")

   (import __error
	   __lalr_util
	   __lalr_gen
	   __lalr_global
	   __lalr_rewrite
		
	   __type
	   __bigloo
	   __tvector
	   __structure
	   __tvector
	   __bexit
	   __bignum
	   __param
	   __object
	   __thread
	   __rgc

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
	   __bit
	   __evenv)

   (export (expand-lalr-grammar x e)))

;*---------------------------------------------------------------------*/
;*    *default* ...                                                    */
;*---------------------------------------------------------------------*/
(define *default* (gensym 'default))


;; ---------------------------------------------------------------------- ;;
;; C'est le point d'entrée ...                                            ;;
;; ---------------------------------------------------------------------- ;;
(define (expand-lalr-grammar x e)
   (match-case x
      ((?- . ?rules)
       (match-case rules
	  (((and (? list?) ?tokens) . (and (? list?) ?rules))
	   (check-lalr-rules x tokens rules)))
       (let ((code (unwind-protect
		      (begin
			 (initialize-all)
			 (rewrite-grammar! rules)
			 (pack-grammar)
			 (set-derives)
			 (set-nullable)
			 (generate-states)
			 (lalr)
			 (build-tables)
			 (compact-action-table)
			 (gen-lalr-code))
		      (clean-plist))))
	  (e code e)))
      (else
       (error "lalr-grammar" "Illegal form" x))))

;; --- Encodage compact de la grammaire ... ----------------------------- ;;
;; Also populates `rprec', which is the precedence for every rule, from 
;; the precedence of the last terminal in the production.
(define (pack-grammar)
  (set! rlhs   (make-vector nrules #f))
  (set! rrhs   (make-vector nrules #f))
  (set! ritem  (make-vector (+fx 1 nitems) #f))
  (set! rprec  (make-vector nrules #f))
  
  (let loop ((p grammar) (item-no 0) (rule-no 1))
    (if (not (null? p))
	(let ((nt (getprop (caar p) 'sym-no)))
	  (let loop2 ((prods (cdar p)) (it-no2 item-no) (rl-no2 rule-no))
	    (if (null? prods)
		(loop (cdr p) it-no2 rl-no2)
		(begin
		  (vector-set! rlhs rl-no2 nt)
		  (vector-set! rrhs rl-no2 it-no2)
		  (let loop3 ((rhs (caar prods)) (it-no3 it-no2))
		    (if (null? rhs)
			(begin
			  (vector-set! ritem it-no3 (negfx rl-no2))
			  (loop2 (cdr prods) (+fx it-no3 1) (+fx rl-no2 1)))
			(let* ((sym (car rhs))
			       (asym (if (pair? sym)
				       (car sym)
				       sym))
			       (sym-no (getprop asym 'sym-no)))

			  (if (getprop asym 'prec)
			    (vector-set! rprec rl-no2 (getprop asym 'prec)))
			  (vector-set! ritem it-no3 sym-no)
			  (loop3 (cdr rhs) (+fx it-no3 1))))))))))))

;; ---------------------------------------------------------------------- ;;
;; On verifie la syntaxe des regles
;; ---------------------------------------------------------------------- ;;
(define (check-lalr-rules x tokens rules)
   (define (check-lalr-rule rule)
      (match-case rule
	 (((? symbol?) . (and (? list?) ?subrules))
	  (for-each (lambda (sr)
		       (match-case sr
			  (((and ?sr (? list?)) . ?-)
			   (for-each (lambda (sym)
					(if (not (symbol? sym))
					    (error "lalr-grammar"
						   "Illegal form "
						   rule)))
				     sr))
			  (else
			   (error "lalr-grammar"
				  "Illegal form (Illegal left hand side)"
				  rule))))
		    subrules))
	 (else
	  (error "lalr-grammar" "Illegal form" x))))
   (for-each (lambda (t)
	       (cond
		 ((symbol? t)
		  #t)
		 ((and (pair? t) (memq (car t) '(left: right: none:)))
		  (for-each (lambda (t)
			       (if (not (symbol? t))
				   (bigloo-type-error 'lalr-grammar
						      'symbol
						      t)))
			    (cdr t)))
		 (else
		  (error "lalr-grammar" "Illegal token" t))))
	     tokens)
   (for-each check-lalr-rule rules))

;; ---------------------------------------------------------------------- ;;
;; Les autres fonctions sont tirées du code de Bison ...                  ;;
;; ---------------------------------------------------------------------- ;;


;; Fonction set-derives
;; --------------------
(define (set-derives)
  (define delts (make-vector (+fx nrules 1) 0))
  (define dset  (make-vector nvars -1))

  (let loop ((i 1) (j 0))		; i = 0
    (if (<fx i nrules)
	(let ((lhs (vector-ref rlhs i)))
	  (if (>=fx lhs 0)
	      (begin
		(vector-set! delts j (cons i (vector-ref dset lhs)))
		(vector-set! dset lhs j)
		(loop (+fx i 1) (+fx j 1)))
	      (loop (+fx i 1) j)))))
  
  (set! derives (make-vector nvars 0))
  
  (let loop ((i 0))
    (if (<fx i nvars)
	(let ((q (let loop2 ((j (vector-ref dset i)) (s '()))
		   (if (<fx j 0)
		       s
		       (let ((x (vector-ref delts j)))
			 (loop2 (cdr x) (cons (car x) s)))))))
	  (vector-set! derives i q)
	  (loop (+fx i 1))))))



(define (set-nullable)
  (set! nullable (make-vector nvars #f))
  (let ((squeue (make-vector nvars 0))
	(rcount (make-vector (+fx nrules 1) 0))
	(rsets  (make-vector nvars #f))
	(relts  (make-vector (+fx nitems (+fx nvars 1)) #f)))
    (let loop ((r 0) (s2 0) (p 0))
      (let ((*r (vector-ref ritem r)))
	(if *r
	    (if (<fx *r 0)
		(let ((symbol (vector-ref rlhs (negfx *r))))
		  (if (and (>=fx symbol 0)
			   (not (vector-ref nullable symbol)))
		      (begin
			(vector-set! nullable symbol #t)
			(vector-set! squeue s2 symbol)
			(loop (+fx r 1) (+fx s2 1) p))))
		(let loop2 ((r1 r) (any-tokens #f))
		  (let* ((symbol (vector-ref ritem r1)))
		    (if (>fx symbol 0)
			(loop2 (+fx r1 1) (or any-tokens (>=fx symbol nvars)))
			(if (not any-tokens)
			    (let ((ruleno (negfx symbol)))
			      (let loop3 ((r2 r) (p2 p))
				(let ((symbol (vector-ref ritem r2)))
				  (if (>fx symbol 0)
				      (begin
					(vector-set! rcount ruleno
						     (+fx (vector-ref rcount ruleno) 1))
					(vector-set! relts p2
						     (cons (vector-ref rsets symbol)
							   ruleno))
					(vector-set! rsets symbol p2)
					(loop3 (+fx r2 1) (+fx p2 1)))
				      (loop (+fx r2 1) s2 p2)))))
			    (loop (+fx r1 1) s2 p))))))
	    (let loop ((s1 0) (s3 s2))
	      (if (<fx s1 s3)
		  (let loop2 ((p (vector-ref rsets (vector-ref squeue s1))) (s4 s3))
		    (if p 
			(let* ((x (vector-ref relts p))
			       (ruleno (cdr x))
			       (y (-fx (vector-ref rcount ruleno) 1)))
			  (vector-set! rcount ruleno y)
			  (if (= y 0)
			      (let ((symbol (vector-ref rlhs ruleno)))
				(if (and (>=fx symbol 0)
					 (not (vector-ref nullable symbol)))
				    (begin
				      (vector-set! nullable symbol #t)
				      (vector-set! squeue s4 symbol)
				      (loop2 (car x) (+fx s4 1)))
				    (loop2 (car x) s4)))
			      (loop2 (car x) s4))))
		    (loop (+fx s1 1) s4)))))))))
		  


; Fonction set-firsts qui calcule un tableau de taille
; nvars et qui donne, pour chaque non-terminal X, une liste des
; non-terminaux pouvant apparaitre au debut d'une derivation a
; partir de X.

(define (set-firsts)
  (set! firsts (make-vector nvars '()))
  
  ;; -- initialization
  (let loop ((i 0))
    (if (<fx i nvars)
	(let loop2 ((sp (vector-ref derives i)))
	  (if (null? sp)
	      (loop (+fx i 1))
	      (let ((sym (vector-ref ritem (vector-ref rrhs (car sp)))))
		(if (< -1 sym nvars)
		    (vector-set! firsts i (sinsert sym (vector-ref firsts i))))
		(loop2 (cdr sp)))))))

  ;; -- reflexive and transitive closure
  (let loop ((continue #t))
    (if continue
	(let loop2 ((i 0) (cont #f))
	  (if (>=fx i nvars)
	      (loop cont)
	      (let* ((x (vector-ref firsts i))
		     (y (let loop3 ((l x) (z x))
			  (if (null? l)
			      z
			      (loop3 (cdr l)
				     (sunion (vector-ref firsts (car l)) z))))))
		(if (equal? x y)
		    (loop2 (+fx i 1) cont)
		    (begin
		      (vector-set! firsts i y)
		      (loop2 (+fx i 1) #t))))))))
  
  (let loop ((i 0))
    (if (<fx i nvars)
	(begin
	  (vector-set! firsts i (sinsert i (vector-ref firsts i)))
	  (loop (+fx i 1))))))




; Fonction set-fderives qui calcule un tableau de taille
; nvars et qui donne, pour chaque non-terminal, une liste des regles pouvant
; etre derivees a partir de ce non-terminal. (se sert de firsts)

(define (set-fderives)
  (set! fderives (make-vector nvars #f))

  (set-firsts)

  (let loop ((i 0))
    (if (<fx i nvars)
	(let ((x (let loop2 ((l (vector-ref firsts i)) (fd '()))
		   (if (null? l) 
		       fd
		       (loop2 (cdr l) 
			      (sunion (vector-ref derives (car l)) fd))))))
	  (vector-set! fderives i x)
	  (loop (+fx i 1))))))


; Fonction calculant la fermeture d'un ensemble d'items LR0
; ou core est une liste d'items

(define (closure core)
  ;; Initialization
  (define ruleset (make-vector nrules #f))

  (let loop ((csp core))
    (if (not (null? csp))
	(let ((sym (vector-ref ritem (car csp))))
	  (if (< -1 sym nvars)
	      (let loop2 ((dsp (vector-ref fderives sym)))
		(if (not (null? dsp))
		    (begin
		      (vector-set! ruleset (car dsp) #t)
		      (loop2 (cdr dsp))))))
	  (loop (cdr csp)))))

  (let loop ((ruleno 1) (csp core) (itemsetv '())) ; ruleno = 0
    (if (<fx ruleno nrules)
	(if (vector-ref ruleset ruleno)
	    (let ((itemno (vector-ref rrhs ruleno)))
	      (let loop2 ((c csp) (itemsetv2 itemsetv))
		(if (and (pair? c)
			 (<fx (car c) itemno))
		    (loop2 (cdr c) (cons (car c) itemsetv2))
		    (loop (+fx ruleno 1) c (cons itemno itemsetv2)))))
	    (loop (+fx ruleno 1) csp itemsetv))
	(let loop2 ((c csp) (itemsetv2 itemsetv))
	  (if (pair? c)
	      (loop2 (cdr c) (cons (car c) itemsetv2))
	      (reverse itemsetv2))))))



(define (allocate-item-sets)
  (set! kernel-base (make-vector nsyms 0))
  (set! kernel-end  (make-vector nsyms #f)))


(define (allocate-storage)
  (allocate-item-sets)
  (set! red-set (make-vector (+fx nrules 1) 0)))

;; --


(define (initialize-states)
  (let ((p (new-core)))
    (set-core-number! p 0)
    (set-core-acc-sym! p #f)
    (set-core-nitems! p 1)
    (set-core-items! p '(0))

    (set! first-state (list p))
    (set! last-state first-state)
    (set! nstates 1)))



(define (generate-states)
  (allocate-storage)
  (set-fderives)
  (initialize-states)
  (let loop ((this-state first-state))
    (if (pair? this-state)
	(let* ((x (car this-state))
	       (is (closure (core-items x))))
	  (save-reductions x is)
	  (new-itemsets is)
	  (append-states)
	  (if (>fx nshifts 0)
	      (save-shifts x))
	  (loop (cdr this-state))))))


;; Fonction calculant les symboles sur lesquels il faut "shifter" 
;; et regroupe les items en fonction de ces symboles

(define (new-itemsets itemset)
  ;; - Initialization
  (set! shift-symbol '())
  (let loop ((i 0))
    (if (<fx i nsyms)
	(begin
	  (vector-set! kernel-end i '())
	  (loop (+fx i 1)))))

  (let loop ((isp itemset))
    (if (pair? isp)
	(let* ((i (car isp))
	       (sym (vector-ref ritem i)))
	  (if (>=fx sym 0)
	      (begin
		(set! shift-symbol (sinsert sym shift-symbol))
		(let ((x (vector-ref kernel-end sym)))
		  (if (null? x)
		      (begin
			(vector-set! kernel-base sym (cons (+fx i 1) x))
			(vector-set! kernel-end sym (vector-ref kernel-base sym)))
		      (begin
			(set-cdr! x (list (+fx i 1)))
			(vector-set! kernel-end sym (cdr x)))))))
	  (loop (cdr isp)))))

  (set! nshifts (length shift-symbol)))



(define (get-state sym)
  (let* ((isp  (vector-ref kernel-base sym))
	 (n    (length isp))
	 (key  (let loop ((isp1 isp) (k 0))
		 (if (null? isp1)
		     (modulofx k STATE-TABLE-SIZE)
		     (loop (cdr isp1) (+fx k (car isp1))))))
	 (sp   (vector-ref state-table key)))
    (if (null? sp)
	(let ((x (new-state sym)))
	  (vector-set! state-table key (list x))
	  (core-number x))
	(let loop ((sp1 sp))
	  (if (and (= n (core-nitems (car sp1)))
		   (let loop2 ((i1 isp) (t (core-items (car sp1)))) 
		     (if (and (pair? i1) 
			      (= (car i1)
				 (car t)))
			 (loop2 (cdr i1) (cdr t))
			 (null? i1))))
	      (core-number (car sp1))
	      (if (null? (cdr sp1))
		  (let ((x (new-state sym)))
		    (set-cdr! sp1 (list x))
		    (core-number x))
		  (loop (cdr sp1))))))))


(define (new-state sym)
  (let* ((isp  (vector-ref kernel-base sym))
	 (n    (length isp))
	 (p    (new-core)))
    (set-core-number! p nstates)
    (set-core-acc-sym! p sym)
    (if (= sym nvars) (set! final-state nstates))
    (set-core-nitems! p n)
    (set-core-items! p isp)
    (set-cdr! last-state (list p))
    (set! last-state (cdr last-state))
    (set! nstates (+fx nstates 1))
    p))


;; --

(define (append-states)
  (set! shift-set
	(let loop ((l shift-symbol))
	  (if (null? l)
	      '()
	      (cons (get-state (car l)) (loop (cdr l)))))))

;; --

(define (save-shifts core)
  (let ((p (new-shift)))
	(set-shift-number! p (core-number core))
	(set-shift-nshifts! p nshifts)
	(set-shift-shifts! p shift-set)
	(if last-shift
	(begin
	  (set-cdr! last-shift (list p))
	  (set! last-shift (cdr last-shift)))
	(begin
	  (set! first-shift (list p))
	  (set! last-shift first-shift)))))

(define (save-reductions core itemset)
  (let ((rs (let loop ((l itemset))
	      (if (null? l)
		  '()
		  (let ((item (vector-ref ritem (car l))))
		    (if (<fx item 0)
			(cons (negfx item) (loop (cdr l)))
			(loop (cdr l))))))))
    (if (pair? rs)
	(let ((p (new-red)))
	  (set-red-number! p (core-number core))
	  (set-red-nreds!  p (length rs))
	  (set-red-rules!  p rs)
	  (if last-reduction
	      (begin
		(set-cdr! last-reduction (list p))
		(set! last-reduction (cdr last-reduction)))
	      (begin
		(set! first-reduction (list p))
		(set! last-reduction first-reduction)))))))


;; --

(define (lalr)
  (set! token-set-size (+fx 1 (quotientfx nterms (BITS-PER-WORD))))
  (set-accessing-symbol)
  (set-shift-table)
  (set-reduction-table)
  (set-max-rhs)
  (initialize-LA)
  (set-goto-map)
  (initialize-F)
  (build-relations)
  (digraph includes)
  (compute-lookaheads))

(define (set-accessing-symbol)
  (set! acces-symbol (make-vector nstates #f))
  (let loop ((l first-state))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! acces-symbol (core-number x) (core-acc-sym x))
	  (loop (cdr l))))))

(define (set-shift-table)
  (set! shift-table (make-vector nstates #f))
  (let loop ((l first-shift))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! shift-table (shift-number x) x)
	  (loop (cdr l))))))

(define (set-reduction-table)
  (set! reduction-table (make-vector nstates #f))
  (let loop ((l first-reduction))
    (if (pair? l)
	(let ((x (car l)))
	  (vector-set! reduction-table (red-number x) x)
	  (loop (cdr l))))))

(define (set-max-rhs)
  (let loop ((p 0) (curmax 0) (length 0))
    (let ((x (vector-ref ritem p)))
      (if x
	  (if (>=fx x 0)
	      (loop (+fx p 1) curmax (+fx length 1))
	      (loop (+fx p 1) (max curmax length) 0))
	  (set! maxrhs curmax)))))

(define (initialize-LA)
  (define (last l)
    (if (null? (cdr l))
	(car l)
	(last (cdr l))))

  (set! consistent (make-vector nstates #f))
  (set! lookaheads (make-vector (+fx nstates 1) #f))

  (let loop ((count 0) (i 0))
    (if (<fx i nstates)
	(begin
	  (vector-set! lookaheads i count)
	  (let ((rp (vector-ref reduction-table i))
		(sp (vector-ref shift-table i)))
	    (if (and rp
		     (or (>fx (red-nreds rp) 1)
			 (and sp
			      (not
			       (<fx (vector-ref acces-symbol
						(last (shift-shifts sp)))
				  nvars)))))
		(loop (+fx count (red-nreds rp)) (+fx i 1))
		(begin
		  (vector-set! consistent i #t)
		  (loop count (+fx i 1))))))

	(begin
	  (vector-set! lookaheads nstates count)
	  (let ((c (max count 1)))
	    (set! LA (make-vector c #f))
	    (do ((j 0 (+fx j 1))) ((= j c)) (vector-set! LA j (new-set token-set-size)))
	    (set! LAruleno (make-vector c -1))
	    (set! lookback (make-vector c #f)))
	  (let loop ((i 0) (np 0))
	    (if (<fx i nstates)
		(if (vector-ref consistent i)
		    (loop (+fx i 1) np)
		    (let ((rp (vector-ref reduction-table i)))
		      (if rp
			  (let loop2 ((j (red-rules rp)) (np2 np))
			    (if (null? j)
				(loop (+fx i 1) np2)
				(begin
				  (vector-set! LAruleno np2 (car j))
				  (loop2 (cdr j) (+fx np2 1)))))
			  (loop (+fx i 1) np))))))))))


(define (set-goto-map)
  (set! goto-map (make-vector (+fx nvars 1) 0))
  (let ((temp-map (make-vector (+fx nvars 1) 0)))
    (let loop ((ng 0) (sp first-shift))
      (if (pair? sp)
	  (let loop2 ((i (reverse (shift-shifts (car sp)))) (ng2 ng))
	    (if (pair? i)
		(let ((symbol (vector-ref acces-symbol (car i))))
		  (if (<fx symbol nvars)
		      (begin
			(vector-set! goto-map symbol 
				     (+fx 1 (vector-ref goto-map symbol)))
			(loop2 (cdr i) (+fx ng2 1)))
		      (loop2 (cdr i) ng2)))
		(loop ng2 (cdr sp))))

	  (let loop ((k 0) (i 0))
	    (if (<fx i nvars)
		(begin
		  (vector-set! temp-map i k)
		  (loop (+fx k (vector-ref goto-map i)) (+fx i 1)))

		(begin
		  (do ((i 0 (+fx i 1)))
		      ((>=fx i nvars))
		    (vector-set! goto-map i (vector-ref temp-map i)))

		  (set! ngotos ng)
		  (vector-set! goto-map nvars ngotos)
		  (vector-set! temp-map nvars ngotos)
		  (set! from-state (make-vector ngotos #f))
		  (set! to-state (make-vector ngotos #f))
		  
		  (do ((sp first-shift (cdr sp)))
		      ((null? sp))
		    (let* ((x (car sp))
			   (state1 (shift-number x)))
		      (do ((i (shift-shifts x) (cdr i)))
			  ((null? i))
			(let* ((state2 (car i))
			       (symbol (vector-ref acces-symbol state2)))
			  (if (<fx symbol nvars)
			      (let ((k (vector-ref temp-map symbol)))
				(vector-set! temp-map symbol (+fx k 1))
				(vector-set! from-state k state1)
				(vector-set! to-state k state2))))))))))))))


(define (map-goto state symbol)
  (let loop ((low (vector-ref goto-map symbol))
	     (high (-fx (vector-ref goto-map (+fx symbol 1)) 1)))
    (if (>fx low high)
	(begin
	  (display (list "Error in map-goto" state symbol)) (newline)
	  0)
	(let* ((middle (quotientfx (+fx low high) 2))
	       (s (vector-ref from-state middle)))
	  (cond
	   ((= s state)
	    middle)
	   ((<fx s state)
	    (loop (+fx middle 1) high))
	   (else
	    (loop low (-fx middle 1))))))))


(define (initialize-F)
  (set! F (make-vector ngotos #f))
  (do ((i 0 (+fx i 1))) ((= i ngotos)) (vector-set! F i (new-set token-set-size)))

  (let ((reads (make-vector ngotos #f)))

    (let loop ((i 0) (rowp 0))
      (if (<fx i ngotos)
	  (let* ((rowf (vector-ref F rowp))
		 (stateno (vector-ref to-state i))
		 (sp (vector-ref shift-table stateno)))
	    (if sp
		(let loop2 ((j (shift-shifts sp)) (edges '()))
		  (if (pair? j)
		      (let ((symbol (vector-ref acces-symbol (car j))))
			(if (<fx symbol nvars)
			    (if (vector-ref nullable symbol)
				(loop2 (cdr j) (cons (map-goto stateno symbol) 
						     edges))
				(loop2 (cdr j) edges))
			    (begin
			      (set-bit rowf (-fx symbol nvars))
			      (loop2 (cdr j) edges))))
		      (if (pair? edges)
			  (vector-set! reads i (reverse edges))))))
	      (loop (+fx i 1) (+fx rowp 1)))))
    (digraph reads)))

(define (add-lookback-edge stateno ruleno gotono)
  (let ((k (vector-ref lookaheads (+fx stateno 1))))
    (let loop ((found #f) (i (vector-ref lookaheads stateno)))
      (if (and (not found) (<fx i k))
	  (if (= (vector-ref LAruleno i) ruleno)
	      (loop #t i)
	      (loop found (+fx i 1)))

	  (if (not found)
	      (begin (display "Error in add-lookback-edge : ")
		     (display (list stateno ruleno gotono)) (newline))
	      (vector-set! lookback i
			   (cons gotono (vector-ref lookback i))))))))


(define (transpose r-arg n)
  (let ((new-end (make-vector n #f))
	(new-R  (make-vector n #f)))
    (do ((i 0 (+fx i 1))) 
	((= i n))
      (let ((x (list 'bidon)))
	(vector-set! new-R i x)
	(vector-set! new-end i x)))
    (do ((i 0 (+fx i 1)))
	((= i n))
      (let ((sp (vector-ref r-arg i)))
	(if (pair? sp)
	    (let loop ((sp2 sp))
	      (if (pair? sp2)
		  (let* ((x (car sp2))
			 (y (vector-ref new-end x)))
		    (set-cdr! y (cons i (cdr y)))
		    (vector-set! new-end x (cdr y))
		    (loop (cdr sp2))))))))
    (do ((i 0 (+fx i 1)))
	((= i n))
      (vector-set! new-R i (cdr (vector-ref new-R i))))
    
    new-R))



(define (build-relations)

  (define (get-state stateno symbol)
    (let loop ((j (shift-shifts (vector-ref shift-table stateno)))
	       (stno stateno))
      (if (null? j)
	  stno
	  (let ((st2 (car j)))
	    (if (= (vector-ref acces-symbol st2) symbol)
		st2
		(loop (cdr j) st2))))))

  (set! includes (make-vector ngotos #f))
  (do ((i 0 (+fx i 1)))
      ((= i ngotos))
    (let ((state1 (vector-ref from-state i))
	  (symbol1 (vector-ref acces-symbol (vector-ref to-state i))))
      (let loop ((rulep (vector-ref derives symbol1))
		 (edges '()))
	(if (pair? rulep)
	    (let ((*rulep (car rulep)))
	      (let loop2 ((rp (vector-ref rrhs *rulep))
			  (stateno state1)
			  (states (list state1)))
		(let ((*rp (vector-ref ritem rp)))
		  (if (>fx *rp 0)
		      (let ((st (get-state stateno *rp)))
			(loop2 (+fx rp 1) st (cons st states)))
		      (begin

			(if (not (vector-ref consistent stateno))
			    (add-lookback-edge stateno *rulep i))
			
			(let loop2 ((done #f) 
				    (stp (cdr states))
				    (rp2 (-fx rp 1))
				    (edgp edges))
			  (if (not done)
			      (let ((*rp (vector-ref ritem rp2)))
				(if (< -1 *rp nvars)
				  (loop2 (not (vector-ref nullable *rp))
					 (cdr stp)
					 (-fx rp2 1)
					 (cons (map-goto (car stp) *rp) edgp))
				  (loop2 #t stp rp2 edgp)))

			      (loop (cdr rulep) edgp))))))))
	    (vector-set! includes i edges)))))
  (set! includes (transpose includes ngotos)))
			


(define (compute-lookaheads)
  (let ((n (vector-ref lookaheads nstates)))
    (let loop ((i 0))
      (if (<fx i n)
	  (let loop2 ((sp (vector-ref lookback i)))
	    (if (pair? sp)
		(let ((LA-i (vector-ref LA i))
		      (F-j  (vector-ref F (car sp))))
		  (bit-union LA-i F-j token-set-size)
		  (loop2 (cdr sp)))
		(loop (+fx i 1))))))))



(define (digraph relation)
  (define infinity (+fx ngotos 2))
  (define INDEX (make-vector (+fx ngotos 1) 0))
  (define VERTICES (make-vector (+fx ngotos 1) 0))
  (define top 0)
  (define R relation)

  (define (traverse i)
    (set! top (+fx 1 top))
    (vector-set! VERTICES top i)
    (let ((height top))
      (vector-set! INDEX i height)
      (let ((rp (vector-ref R i)))
	(if (pair? rp)
	    (let loop ((rp2 rp))
	      (if (pair? rp2)
		  (let ((j (car rp2)))
		    (if (= 0 (vector-ref INDEX j))
			(traverse j))
		    (if (>fx (vector-ref INDEX i) 
			   (vector-ref INDEX j))
			(vector-set! INDEX i (vector-ref INDEX j)))
		    (let ((F-i (vector-ref F i))
			  (F-j (vector-ref F j)))
		      (bit-union F-i F-j token-set-size))
		    (loop (cdr rp2))))))
	(if (= (vector-ref INDEX i) height)
	    (let loop ()
	      (let ((j (vector-ref VERTICES top)))
		(set! top (-fx top 1))
		(vector-set! INDEX j infinity)
		(if (not (= i j))
		    (begin
		      (bit-union (vector-ref F i) 
				 (vector-ref F j)
				 token-set-size)
		      (loop)))))))))

  (let loop ((i 0))
    (if (<fx i ngotos)
	(begin
	  (if (and (= 0 (vector-ref INDEX i))
		   (pair? (vector-ref R i)))
	      (traverse i))
	  (loop (+fx i 1))))))


;; --

(define (build-rule n)
  (cons
   (vector-ref *symv* (vector-ref rlhs n))
   (cons
    '-->
    (let loop ((pos (vector-ref rrhs n)))
      (let ((x (vector-ref ritem pos)))
	(if (<fx x 0)
	    '()
	    (cons (vector-ref *symv* x)
		  (loop (+fx pos 1)))))))))

(define (get-precedence Sym Act)
  (if (<=fx Act 0) 
    (vector-ref rprec (negfx Act))			 ; Reduce (from rule)
    (getprop (vector-ref *symv* (+fx Sym nvars)) 'prec))); Shift (from terminal)

(define (build-tables)

  (define (add-action St Sym Act)
     (let* ((x (vector-ref action-table St))
	    (y (assv Sym x)))
	(if y
	  (let ((cur-prec (get-precedence Sym (cdr y)))
		(new-prec (get-precedence Sym Act)))
	    (cond
	      ((=fx Act (cdr y))	  ; ** Same action as in the table.
	       #t)
	      ((and (<=fx (cdr y) 0)	  ; ** Reduce/Reduce conflict.
		    (<=fx Act 0))
	       (warning "lalr-grammar"
			"** Reduce/Reduce conflict: "
			#"\n - reduce by rule " (build-rule (negfx Act))
			#"\n - reduce by rule " (build-rule (negfx (cdr y)))
			#"\non token `" (vector-ref *symv* (+fx nvars Sym))
			"'")
	       (set-cdr! y (max (cdr y) Act)))
	      ((or cur-prec new-prec)	  ; ** Resolve with precedence.
	       (cond
		 ((not new-prec)	  ; ** Old symbol has precedence.
		  #t)
		 ((not cur-prec)	  ; ** New symbol has precedence.
		  (set-cdr! y Act))
		 ((=fx (cdr cur-prec) (cdr new-prec))
		  ; ** Equal precedence, use associativity.
		  (let ((shift (max (cdr y) Act))
			(reduce (min (cdr y) Act)))
		    (case (car cur-prec)
		      ((left:)
		       (set-cdr! y reduce))
		      ((right:)
		       (set-cdr! y shift))
		      ((none:)
		       ;; Dominique Boucher suggested to (set-cdr! y '*error*)
		       ;; 16 Nov 2006
		       (set-cdr! y '*error*)))))
		       ; Remove from action table so we cause an error during
		       ; parse.  NOTE: If we have another reduce later, we
		       ; have no record so we can't point out the
		       ; reduce/reduce conflict.
;* 		       (vector-set! action-table St                    */
;* 				    (let loop ((iter x))               */
;* 				      (cond                            */
;* 					((null? iter)                  */
;* 					 iter)                         */
;* 					((eqv? (car iter) St)          */
;* 					 (loop (cdr iter)))            */
;* 					(else                          */
;* 					 (cons (car iter)              */
;* 					       (loop (cdr iter)))))))  */
		 ((>fx (cdr cur-prec) (cdr new-prec))
		  ; ** New action has higher precedence (lower precedence num).
		  (set-cdr! y Act))
		 (else
		  ; ** Current action has higher precedence.  Do nothing.
		  #t)))
	      (else			  ; ** Shift/Reduce conflict.
	       (warning "lalr-grammar"
			"** Shift/Reduce conflict: "
			#"\n - shift to state "  Act
			#"\n - reduce rule " (build-rule (negfx (cdr y)))
			#"\non token `"  (vector-ref *symv* (+fx nvars Sym))
			"'")
	       (set-cdr! y Act))))
	  (vector-set! action-table St (cons (cons Sym Act) x)))))

  (define (add-action-for-all-terminals state action)
     (do ((i 1 (+ i 1)))
	 ((= i nterms))
	 (add-action state i action)))
   
  (set! action-table (make-vector nstates '()))

  (do ((i 0 (+fx i 1)))  ; i = state
      ((= i nstates))
    (let ((red (vector-ref reduction-table i)))
      (if (and red (>=fx (red-nreds red) 1))
	  (if (and (= (red-nreds red) 1) (vector-ref consistent i))
;*---------------------------------------------------------------------*/
;*    Patch Dominique Boucher, 5 sep 2005.                             */
;*---------------------------------------------------------------------*/
;;	      (add-action i *default* (negfx (car (red-rules red))))
              (add-action-for-all-terminals i (- (car (red-rules red))))
	      (let ((k (vector-ref lookaheads (+fx i 1))))
		(let loop ((j (vector-ref lookaheads i)))
		  (if (<fx j k)
		      (let ((rule (negfx (vector-ref LAruleno j)))
			    (lav  (vector-ref LA j)))
			(let loop2 ((token 0) (x (vector-ref lav 0)) (y 1) (z 0))
			  (if (<fx token nterms)
			      (begin
				(let ((in-la-set? (modulofx x 2)))
				  (if (= in-la-set? 1)
				      (add-action i token rule)))
				(if (= y (BITS-PER-WORD))
				    (loop2 (+fx token 1) 
					   (vector-ref lav (+fx z 1))
					   1
					   (+fx z 1))
				    (loop2 (+fx token 1) (quotientfx x 2) (+fx y 1) z)))))
			(loop (+fx j 1)))))))))

    (let ((shiftp (vector-ref shift-table i)))
      (if shiftp
	  (let loop ((k (shift-shifts shiftp)))
	    (if (pair? k)
		(let* ((state (car k))
		       (symbol (vector-ref acces-symbol state)))
		  (if (>=fx symbol nvars)
		      (add-action i (-fx symbol nvars) state))
		  (loop (cdr k))))))))

  (add-action final-state 0 'accept))

(define (compact-action-table)
  (define (most-common-action acts)
    (let ((accums '()))
      (let loop ((l acts))
	(if (pair? l)
	    (let* ((x (cdar l))
		   (y (assv x accums)))
	      (if (and (number? x) (<fx x 0))
		  (if y
		      (set-cdr! y (+fx 1 (cdr y)))
		      (set! accums (cons `(,x . 1) accums))))
	      (loop (cdr l)))))

      (let loop ((l accums) (max 0) (sym #f))
	(if (null? l)
	    sym
	    (let ((x (car l)))
	      (if (>fx (cdr x) max)
		  (loop (cdr l) (cdr x) (car x))
		  (loop (cdr l) max sym)))))))

  (do ((i 0 (+fx i 1)))
      ((= i nstates))
    (let ((acts (vector-ref action-table i)))
      (if (vector? (vector-ref reduction-table i))
	  (let ((act (most-common-action acts)))
	    (vector-set! action-table i
			 (cons `(,*default* . ,(if act act 'error))
			       (filter (lambda (x) 
					 (not (eq? (cdr x) act)))
				       acts))))
	  (vector-set! action-table i 
		       (cons `(,*default* . *error*) acts))))))


;; --- Fin de `lalr.scm' ------------------------------------------------ ;;


