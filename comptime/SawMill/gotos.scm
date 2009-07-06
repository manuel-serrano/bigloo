(module saw_gotos
   (import type_type ast_var ast_node
	   saw_defs )
   (export (add-gotos l::pair-nil)) )

;;
;; Make the gotos explicits
;;
(define (add-gotos l::pair-nil) ;(list block)
   (let walk ( (l l) )
      (if (pair? l)
	  (let ( (r (cdr l)) )
	     (add-goto (car l) r)
	     (walk r) )
	  #unspecified )))

;;
;; Make one goto explicit
;;
(define (add-goto b::block l::pair-nil) ; ::(list block)
   (let ( (last (last-pair (block-first b))) )
      (let ( (added (make-goto b (car last) l)) )
	 (if (not (null? added))
	     (set-cdr! last added) ))))

;;
;; Create extra code for explicit control flow
;;
(define (make-goto b::block ins l::pair-nil) ; ::(list block)
   (with-access::block b (succs)
      (cond ((null? succs)
	     '() )
	    ((null? (cdr succs))
	     (let ( (s (car succs)) )
		(if (or (null? l) (not (eq? (car l) s)))
		    (cons (_goto s) '())
		    '() )))
	    (else
	     (with-access::rtl_ins ins (fun)
		(cond
		   ((rtl_select? fun)
		    (_switch! fun ins succs)
		    '() )
		   ((rtl_if? fun)
		    (_if fun ins succs l) )
		   (else (error 'add-goto "CARE" ins)) ))))))

;; creation of a goto instruction
(define (_goto::rtl_ins b::block) ;()
   (instantiate::rtl_ins (dest #f)
			 (fun (instantiate::rtl_go (to b)))
			 (args '()) ))

;; modification of a switch
(define (_switch! fun::rtl_select ins::rtl_ins succs) ;(list block)
   (with-access::rtl_select fun (type patterns)
      (rtl_ins-fun-set! ins
			(instantiate::rtl_switch
			   (type type)
			   (patterns patterns)
			   (labels succs) ))))

;; if treatment
(define (_if fun::rtl_if ins::rtl_ins succs nexts) ;(list block)
   (let ( (then (car succs)) (else (cadr succs)) )
      (cond ((null? nexts)
	     (_ifeq! ins else)
	     (cons (_goto then) '()) )
	    ((eq? then (car nexts))
	     (_ifeq! ins else)
	     '() )
	    ((eq? else (car nexts))
	     (_ifne! ins then)
	     '() )
	    (else
	     (_ifeq! ins else)
	     (cons (_goto then) '()) ))))

(define (_ifeq! ins::rtl_ins then::block) ;()
   (rtl_ins-fun-set! ins (instantiate::rtl_ifeq (then then))) )

(define (_ifne! ins::rtl_ins then::block) ;()
   (rtl_ins-fun-set! ins (instantiate::rtl_ifne (then then))) )
