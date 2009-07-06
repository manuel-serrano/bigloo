(module ev
   (main top) )

(define (top l)
   (if (< (length l) 2)
       (toplevel)
       (compile (call-with-input-file (cadr l) read)) ))

(define (compile l)
   l )

(define (toplevel)
   (define (loop)
      (let ( (pg (pre (read))) )
	 (print "= " (ev pg '() '()
			 (lambda (v events2 k) v)
			 (lambda (s events2 k) 'stop) )) )
      (loop) )
   (loop) )

(define (pre e)
   (match-case e
      (() ''())
      ((? symbol?) e)
      ((? integer?) `',e)
      ((lambda ?args . ?body)
       `(lambda ,args ,(preb body)))
      ((quote ?-) e)
      ((stop) e)
      ((if ?test ?then ?else)
       `(if ,(pre test) ,(pre then) ,(pre else)))
      ((begin . ?es)
       (preb es))
      ((let ((?i1 ?v1)) . ?body)
       (pre `((lambda (,i1) ,@body) ,v1)))
      ((let ((?i1 ?v1) (?i2 ?v2)) . ?body)
       (pre `((lambda (,i1) ((lambda (,i2) ,@body) ,v2)) ,v1)))
      ((let ?bdgs . ?body)
       (pre `((lambda ,(map car bdgs) ,@body) ,@(map cadr bdgs))))
      ((call/cc ?body)
       `(call/cc ,(pre body)))
      ((call-with-current-continuation ?body)
       `(call-with-current-continuation ,(pre body)))
      (else
       (map pre e))))

(define (preb l)
   (cond
      ((null? l) '())
      ((null? (cdr l))
       (pre (car l) ))
      (else
       `(begin ,(pre (car l)) ,(preb (cdr l))) )))

(define (prel vars body)
   (if (null? vars)
       body
      `(let ,(caar vars) ,(pre (cadar vars))
	    ,(prel (cdr vars) body) )))

(define (ev e env events pk nk)
   (match-case e
      ((? symbol?)
       (let ((cell (assq e env)))
	  (if (pair? cell)
	      (pk (cdr cell) events nk)
	      (error 'eval "unbound variable" e))))
      ((quote ?a)
       (pk a events nk))
      ((if ?test ?then ?else)
       (ev test env events
	   (lambda (v events2 nk2)
	      (if v
		  (ev then env events2 pk nk2)
		  (ev else env events2 pk nk2) ))
	   nk ))
      ((begin ?e1 ?e2)
       (ev e1 env events
	   (lambda (v events2 nk2)
	      (ev e2 env events2 pk nk2) )
	   nk ))
      ((lambda ?args ?body)
       (pk `(closure ,args ,body ,env) events nk))
      ((print ?a)
       (ev a env events
	   (lambda (v events2 nk2)
	      (begin (print v)
		     (pk v events2 nk2) ))
	   nk ))
      ((call/cc ?fun)
       (ev `(,fun (lambda (v) (funcall ,pk v))) env events
	   pk
	   nk ))
      ((call-with-current-continuation ?fun)
       (ev `(,fun (lambda (v) (funcall2 ,pk ,nk v))) env events
	   pk
	   nk ))
      ((funcall ?pk ?val)
       (ev val env events
	   (lambda (v events' nk')
	      (pk v events' nk'))
	   nk ))
      ((funcall2 ?pk ?nk0 ?val)
       (ev val env events
	   (lambda (v events' nk')
	      (pk v events' nk0))
	   nk ))
      ((emit ?a)
       (ev a env events
	   (lambda (v events' nk')
	      (ev ''_ env events'
		  (lambda (v' events'' nk'')
		     (pk v' (cons (cons v v') events'') nk'') )
		  nk' ))
	   nk ))
      ((emit ?a ?val)
       (ev a env events
	   (lambda (v events' nk')
	      (ev val env events'
		  (lambda (v' events'' nk'')
		     (pk v' (cons (cons v v') events'') nk'') )
		  nk' ))
	   nk ))
      ((wait ?a)
       (ev a env events
	   (lambda (v events2 nk2)
	      (let ( (ok (assq v events2)) )
		 (if (pair? ok)
		     (pk (cdr ok) events2 nk2)
		     (nk2 (cons 'wait v) events2 pk) )))
	   nk ))
      ((stop)
       (nk (cons 'stop '()) events pk))
      ((new ?a)
       (nk (cons 'new (cons a env)) events pk))
      ((par . ?as)
       (pk (schedule (map (lambda (e) (cons (cons 'init (cons e env))
					    nevererror ))
			  as )
		     '() )
	   events
	   nk ))
      ((?e1 ?e2)
       (ev e1 env events
	   (lambda (v events' nk')
	      (ev e2 env events'
		  (lambda (v' events'' nk'')
		     (match-case v
			((closure (?a) ?body ?env)
			 (ev body (cons (cons a v') env) events'' pk nk'')) 
			(else
			 (error 'eval "Illegal function" v))))
		  nk' ))
	   nk ))
      (else
       (error 'eval "bad form" e))))
       
(define (nevererror v events nk)
   (error 'schedule "never used partial cont" v) )

(define (enderror v events nk)
   (error 'schedule "cannot restart a dead process" v) )

(define *microtime* 0)
(define *time* 0)

(define (schedule procs events)
   (print "begin time " *microtime* " " (map caar procs) " " events)
   (set! *microtime* (+ 1 *microtime*))
   (walk procs '() events
	 (lambda (procs events)
	    (if (finish? procs)
		(map cdar procs)
		(advance procs events) ))) )

(define (walk l done events sk)
   (cond
      ((null? l)
       (sk (reverse done) events) )
      (else
       (let ( (p (car l)) (r (cdr l)) )
	  (let ( (sname (caar p)) (sargs (cdar p)) (cpk (cdr p)) )
	     (cond
		((eq? sname 'init)
		 (ev (car sargs) (cdr sargs) events
		     (lambda (v events2 nk2)
			(nk2 (cons 'end v) events2 enderror) )
		     (trap-event r done sk) ))
		((eq? sname 'stop)
		 (walk r (cons p done) events sk) )
		((eq? sname 'end)
		 (walk r (cons p done) events sk) )
		((eq? sname 'wait)
		 (let ((cell (assq sargs events)))
		    (if (pair? cell)
			(cpk (cdr cell) events (trap-event r done sk))
			(walk r (cons p done) events sk) )))
		((eq? sname 'restart)
		 (cpk #unspecified events (trap-event r done sk)) )
		(else 'schedule "bad state" (car p)) ))))))

(define (trap-event r done sk)
   (lambda (state events2 pk)
      (cond
	 ((eq? (car state) 'new)
	  (let ( (p (cons (cons 'init (cdr state)) nevererror )) )
	     (pk p events2 (trap-event (cons p r) done sk)) ))
	 (else
	  (walk r (cons (cons state pk) done) events2 sk) ))))
		   

(define (finish? l)
   (cond
      ((null? l)
       #t)
      ((eq? (caaar l) 'end)
	(finish? (cdr l)) )
      (else #f) ))

(define (advance l events)
   (if (anywaitarrive l events)
       (schedule l events)
       (if (deadlock l)
	   (error 'eval "deadlock " (map caar l))
	   (begin (print "TIME " *time*)
		  (set! *time* (+ 1 *time*))
		  (set! *microtime* 0)
		  (schedule (restart l) '()) ))))

(define (deadlock l)
   (cond
      ((null? l) #t)
      ((eq? (caaar l) 'wait)
       (deadlock (cdr l)) )
      ((eq? (caaar l) 'end)
       (deadlock (cdr l)) )
      (else #f) ))

(define (anywaitarrive l events)
   (cond
      ((null? l) #f)
      ((eq? (caaar l) 'wait)
       (if (pair? (assq (cdaar l) events))
	   #t
	   (anywaitarrive (cdr l) events) ))
      (else (anywaitarrive (cdr l) events)) ))

(define (restart l)
   (cond
      ((null? l) '())
      ((eq? (caaar l) 'stop)
       (cons (cons (cons 'restart (cdaar l)) (cdar l))
	     (restart (cdr l)) ))
      (else (cons (car l) (restart (cdr l)))) ))

	  

