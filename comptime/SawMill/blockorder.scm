(module saw_blockorder
   (import type_type ast_var ast_node
	   saw_defs )
   (export (block-ordering::pair-nil b::block))
   (static (wide-class done::block)
	   (wide-class rdone::block)
	   (wide-class dfs::block n::int) ))

(define *blockordering* 'reverse-dfs)

(define (block-ordering::pair-nil b::block) ;(list block)
   (case *blockordering*
      ((reverse-dfs) (rdfs-ordering b))
      (else (dfs-ordering b)) ))

;;
;; Depth First Search Ordering
;;
(define (dfs-ordering::pair-nil b::block) ;(list block)
   (let ( (r '()) )
      (let dfs ( (b b) )
	 (widen!::done b)
	 (set! r (cons b r))
	 (for-each (lambda (s) (if (not (done? s)) (dfs s)))
		   (block-succs b) ))
      (reverse! r) ))


;;
;; Reverse Depth First Search Ordering
;;
(define (rdfs-ordering::pair-nil b::block) ;(list block)
   (let ( (r '()) )
      (define (visit b::block)
	 (widen!::rdone b)
	 (set! r (cons b r))
	 ;; CARE elements of predecessors are always not done
	 (for-each (lambda (s) (if (not (rdone? s)) (visit s)))
		   (predecessors b) ))
      ;; Grab blocks backward from exit points of the graph
      (for-each (lambda (s) (if (not (rdone? s)) (visit s))) (find-exit b))
      ;; Grab all blocks not visited (i.e. loops)
      (let dfs ( (b b) )
	 (if (not (rdone? b)) (set! r (cons b r)))
	 (widen!::done b)
	 (for-each (lambda (s) (if (not (done? s)) (dfs s))) (block-succs b)) )
      ; put b in front and failures in back
      (let walk ( (l r) (prev '()) (lastfail '()) )
	 (let ( (x (car l)) )
	    (if (eq? x b)
		(begin (if (not (null? prev))
			   (let ( (end (last-pair l)) )
			      (if (or (null? lastfail) (eq? prev lastfail))
				  (begin (set-cdr! end r)
					 (set-cdr! prev '()) )
				  (begin (set-cdr! end (cdr lastfail))
					 (set-cdr! prev r)
					 (set-cdr! lastfail '()) ))))
		       l )
		(let ( (fun (rtl_ins-fun (car (last-pair (block-first x))))) )
		   (if (or (not (rtl_last? fun)) (rtl_return? fun))
		       (walk (cdr l) l lastfail)
		       (walk (cdr l) l l) )))))))

;      (let walk ( (l r) (done '()) )
;	     (let ( (x (car l)) )
;		(if (eq? x b)
;		    (append l (reverse done))
;		    (walk (cdr l) (cons x done)) )))

;; find the exit points of the graph
(define (find-exit b::block) ;()
   (let ( (rets '()) (fails '()) (n 0) )
      (let dfs ( (b b) )
	 (widen!::dfs b (n n))
	 (set! n (+fx n 1))
	 ;; CARE bizzare un code mettait dans fails les blocks sans
	 ;; predecesseurs.....
	 (let ( (fun (rtl_ins-fun (car (last-pair (block-first b))))) )
	    (if (rtl_last? fun)
		(if (rtl_return? fun)
		    (set! rets (cons b rets))
		    (set! fails (cons b fails)) )))
	 (for-each (lambda (s) (if (not (dfs? s)) (dfs s))) (block-succs b)) )
      (append rets fails) ))

;; 
(define (predecessors b::block) ;()
   (sort (filter (lambda (x) (not (rdone? x))) (block-preds b))
	 lengthuncolsucc<=? ))

(define (lengthuncolsucc<=? b1 b2) ;()
   (let ( (n1 (lengthuncolsucc b1)) (n2 (lengthuncolsucc b2)) )
      (cond ((< n1 n2) #t)
	    ((> n1 n2) #f)
	    (else (> (dfs-n b1) (dfs-n b2))) )))

(define (lengthuncolsucc b::block) ;()
   (length (filter (lambda (x) (not (rdone? x))) (block-succs b))) )

;;
;; Reverse Path Search Ordering
;;
(define (rpath-ordering::pair-nil b::block) ;(list block)
   (let ( (r '()) )
      (define (visit b::block)
	 (widen!::rdone b)
	 (set! r (cons b r))
	 (let ( (l (predecessors b)) )
	    (if (pair? l)
		(begin 
		   (if (rdone? (car l)) (error 'rpath "impossible" '()))
		   (visit (car l)) ))))
      ;; Grab blocks backward from exit points of the graph
      (let walk ( (l (find-end b)) )
	 (if (pair? l)
	     (begin (visit (car l))
		    (walk (find-end b)) )))
      ; put b in front and failures in back
      (let walk ( (l r) (prev '()) (lastfail '()) )
	 (let ( (x (car l)) )
	    (if (eq? x b)
		(begin (if (not (null? prev))
			   (let ( (end (last-pair l)) )
			      (if (or (null? lastfail) (eq? prev lastfail))
				  (begin (set-cdr! end r)
					 (set-cdr! prev '()) )
				  (begin (set-cdr! end (cdr lastfail))
					 (set-cdr! prev r)
					 (set-cdr! lastfail '()) ))))
		       l )
		(let ( (fun (rtl_ins-fun (car (last-pair (block-first x))))) )
		   (if (or (not (rtl_last? fun)) (rtl_return? fun))
		       (walk (cdr l) l lastfail)
		       (walk (cdr l) l l) )))))))

;      (let walk ( (l r) (done '()) )
;	     (let ( (x (car l)) )
;		(if (eq? x b)
;		    (append l (reverse done))
;		    (walk (cdr l) (cons x done)) )))

;; find the exit points of the graph
(define (find-end b::block) ;()
   (let ( (rets '()) (fails '()) (n 0) )
      (let dfs ( (b b) )
	 (widen!::dfs b (n n))
	 (set! n (+fx n 1))
	 ;; CARE bizzare un code mettait dans fails les blocks sans
	 ;; predecesseurs.....
	 (let ( (fun (rtl_ins-fun (car (last-pair (block-first b))))) )
	    (if (rtl_last? fun)
		(if (rtl_return? fun)
		    (set! rets (cons b rets))
		    (set! fails (cons b fails)) )))
	 (for-each (lambda (s) (if (not (dfs? s)) (dfs s))) (block-succs b)) )
      (append rets fails) ))

