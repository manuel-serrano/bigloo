;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime1.8/Init/args.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 12 11:11:53 1996                          */
;*    Last change :  Tue Apr  9 15:13:56 1996 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The argument parsing facility                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    args-case ...                                                    */
;*    -------------------------------------------------------------    */
;*    Here is an example of `args-case' usage (extracted from          */
;*    Examples/Args/foo.scm).					       */
;*                                                                     */
;*    (args-case (prologue (print "toto")) (epilogue (print "bye bye"))*/
;*       argv                                                          */
;*       (("-o" ?name (synopsis "The name of the src file"))           */
;*        (print "The source file is " name))                          */
;*       (("-o?name" (synopsis "An alternate src file"))               */
;*        (print "The source file is again " name))                    */
;*       (("-dummy" (synopsis "-dummy" "A dummy option"))              */
;*        (print "One dummy option"))                                  */
;*       (("-dummy2" ?dummy1 ?dummy2                                   */
;* 		  (synopsis "-dummy2 <arg1> <arg2>"                    */
;* 			    "A dummy option with 2 args"))             */
;*        (print "One dummy option with 2 args"))                      */
;*       (("-help")                                                    */
;*        (usage))))                                                   */
;*---------------------------------------------------------------------*/
(define-expander args-parse
   (lambda (x e)
      ;; produce compile-time (expand-time) errors
      (define (expand-time-error)
	 (error "args-case" "Illegal `args-case' syntax" x))
      (define (expand-time-error-clause clause)
	 (error "args-case" "Illegal `args-case' clause" clause))
      ;; usage function
      (define (make-usage-body descrs)
	 (let* ((mlen (let loop ((mlen 0)
				 (dcrs descrs))
			 (cond
			    ((null? dcrs)
			     mlen)
			    ((>=fx
			      (string-length
			       (caar dcrs))
			      mlen)
			     (loop (string-length (caar dcrs))
				   (cdr dcrs)))
			    (else
			     (loop mlen
				   (cdr dcrs))))))
		(dop `(lambda (opt)
			 (let* ((name (car opt))
				(len  (string-length name))
				(desc (cdr opt))
				(tab  (make-string (-fx ,mlen len)
				       #\space)))
			    (print "   "
				   name
				   tab
				   " --  "
				   desc)))))
	    `(lambda ()
		(let ((descrs (reverse! ',descrs)))
		   (for-each ,dop descrs)
		   'usage-done))))
      ;; we parse the arguments of the macro
      (match-case x
	 ((?- ?exp . ?src-clauses)
	  (let* ((args           (gensym 'argv))
		 (a-runner       (gensym 'runner))
		 (arg            (gensym 'arg))
		 (a-arg          (gensym 'arg))
		 (a-loop         (gensym 'loop))
		 (a-done         (gensym 'done))
		 (ill-opt        'args-parse-error)
		 (usage          'args-parse-usage)
		 (option-marker1 (gensym 'parse-args))
		 (option-marker2 (gensym 'parse-args))
		 (mark-option!   (lambda (str mode)
				    (case mode
				       ((1)
					(putprop! (string->symbol str)
						  option-marker1
						  #t))
				       (else
					(putprop! (string->symbol str)
						  option-marker2
						  #t)))))
		 (marked-option? (lambda (str mode)
				    (case mode
				       ((1)
					(getprop (string->symbol str)
						 option-marker1))
				       (else
					(getprop (string->symbol str)
						 option-marker2)))))
		 (bind-option    (lambda (opt mode)
				    (if (marked-option? opt mode)
					(warning
					 "parse-args"
					 "Option overwriting (ignoring) -- "
					 opt)
					(mark-option! opt mode)))))
	     ;; checks an option arguments list in order to see
	     ;; if all are symbols beginning with #\?. If they are return
	     ;; the symbol-sans-? list otherwise return #f.
	     (define (get-opt-args-name-list args)
		(let loop ((args args)
			   (res  '()))
		   (cond
		      ((null? args)
		       (reverse! res))
		      ((symbol? (car args))
		       (let* ((str (symbol->string (car args)))
			      (len (string-length str)))
			  (if (or (=fx (string-length str) 0)
				  (not (char=? (string-ref str 0) #\?)))
			      #f
			      (loop (cdr args)
				    (cons (string->symbol
					   (string-upcase
					    (substring str 1 len)))
					  res)))))
		      (else
		       #f))))
	     ;; extract the argument of a line (?top ??args (synopsis))
	     (define (get-args runner end)
		(let loop ((runner runner)
			   (res    '()))
		   (if (or (null? runner)
			   (eq? (car runner) end))
		       (reverse! res)
		       (loop (cdr runner)
			     (cons (car runner) res)))))
	     ;; extract argument name from an option name (i.e. -I?dir)
	     (define (get-opt-arg-name-one opt)
		(let ((len (-fx (string-length opt) 1)))
		   (let loop ((i 0))
		      (cond
			 ((>=fx i len)
			  #f)
			 ((char=? (string-ref opt i) #\?)
			  (cons (substring opt 0 i)
				(substring opt (+fx 1 i) (+fx len 1))))
			 (else
			  (loop (+fx i 1)))))))
	     ;; let-binding, a variable destructuration
	     (define (let-binding larg arg)
		(let loop ((larg   larg)
			   (access `(cdr ,arg))
			   (res    '()))
		   (if (null? larg)
		       (cons access (reverse! res))
		       (loop (cdr larg)
			     `(cdr ,access)
			     (cons (list (car larg)
					 `(car ,access)) res)))))
	     ;; make synopsis name when no specified
	     (define (make-synopsis-name opt args)
		(let ((oarg (get-opt-arg-name-one opt))
		      (larg (get-opt-args-name-list args)))
		   (cond
		      ((and oarg (pair? larg))
		       (expand-time-error-clause clause))
		      ((not (or oarg (pair? larg)))
		       opt)
		      (oarg
		       (let* ((opt-name (car oarg))
			      (opt-arg  (cdr oarg)))
			  (string-append opt-name "<" opt-arg ">")))
		      (else
		       (string-append
			opt
			(let loop ((larg larg))
			   (if (null? larg)
			       ""
			       (string-append " <"
					      (string-downcase
					       (symbol->string
						(car larg)))
					      ">"
					      (loop (cdr larg))))))))))
	     ;; make one args-case clause
	     (define (make-clause clause opt args actions)
		(let ((oarg (get-opt-arg-name-one opt))
		      (larg (get-opt-args-name-list args)))
		   (cond
		      ((and oarg (pair? larg))
		       (expand-time-error-clause clause))
		      ((not (or oarg (pair? larg)))
		       (bind-option opt 1)
		       `((string=? ,arg ,opt)
			 (let* ((the-args  ,a-runner)
				(the-remaining-args (cdr ,a-runner))
				(action (begin ,@actions)))
			    (,a-loop the-remaining-args
				     action))))
		      (oarg
		       (let* ((opt-name (car oarg))
			      (opt-len  (string-length opt-name))
			      (opt-arg  (cdr oarg))
			      (opt-sarg (string->symbol
					 (string-upcase opt-arg))))
			  (bind-option opt-name 2)
			  `((and (>=fx (string-length ,arg) ,opt-len)
				 (string=? (substring ,arg 0 ,opt-len)
					   ,opt-name))
			    (let* ((,opt-sarg (substring ,arg
							 ,opt-len
							 (string-length ,arg)))
				   (the-args  ,a-runner)
				   (the-remaining-args (cdr ,a-runner))
				   (action (begin ,@actions)))
			       (,a-loop the-remaining-args
					action)))))
		      (else
		       (bind-option opt 1)
		       (let ((next&bdg (let-binding larg a-runner)))
			  `((string=? ,arg ,opt)
			    (let ,(cdr next&bdg)
			       (let* ((the-args ,a-runner)
				      (the-remaining-args ,(car next&bdg))
				      (action (begin ,@actions)))
				  (,a-loop the-remaining-args
					   action)))))))))
	     ;; the main job
	     (let loop ((runner  src-clauses)
			(clauses '())
			(descrs  '())
			(else    #f))
		(if (null? runner)
		    (e `(let* ((,args    ,exp)
			       (,ill-opt (lambda (culprit) 
					    (error "Illegal option"
						   "see `-help' option"
						   culprit)))
			       (,usage   ,(make-usage-body descrs)))
			   (cond
			      ((and (not (pair? ,args))
				    (not (null? ,args)))
			       (,ill-opt ,args))
			      (else
			       (let ((,a-arg ,args))
				  (try
				   (let ,a-loop ((,a-runner ,args)
						 (,a-done   #f))
					(cond
					   ((null? ,a-runner)
					    'parsing-done)
					   ((eq? ,a-done 'usage-done)
					    ,a-done)
					   ((not (pair? ,a-runner))
					    (,ill-opt ,a-runner))
					   (else
					    (let* ((,arg    (car ,a-runner))
						   (the-arg ,arg))
					       (set! ,a-arg ,a-runner)
					       (cond
						  ,@(reverse! clauses)
						  (else
						   (let ((else ,arg))
						      (,a-loop
						       (cdr ,a-runner)
						       ,(if (pair? else)
							    `(begin ,@else)
							    `(,ill-opt
							      ,a-runner))))))))))
				   (lambda (esc proc mes obj)
				      (,ill-opt ,a-arg)))))))
		       e)
		    (let ((run (car runner)))
		       (match-case run
			  ;; ((? opt ?args (synopsis ?name ?descr)) ?actions)
			  ((((and (? string?) ?opt)
			     ??-
			     (and ?syn (synopsis (and (? string?) ?name)
						 ?msg)))
			    . ?actions)
			   (let ((args (get-args (cdr (car run)) syn)))
			      (loop (cdr runner)
				    (cons (make-clause run opt args actions)
					  clauses)
				    (cons (cons name (eval msg)) descrs)
				    #f)))
			  ((((and (? string?) ?opt)
			     ??-
			     (and ?syn (synopsis ?msg)))
			    . ?actions)
			   (let ((args (get-args (cdr (car run)) syn)))
			      (loop (cdr runner)
				    (cons (make-clause run opt args actions)
					  clauses)
				    (cons (cons (make-synopsis-name opt args)
						(eval msg))
					  descrs)
				    #f)))
			  ;; ((? opt ?args) ?actions)
			  ((((and (? string?) ?opt)
			     ??-)
			    . ?actions)
			   (let ((args (get-args (cdr (car run)) '())))
			      (loop (cdr runner)
				    (cons (make-clause run opt args actions)
					  clauses)
				    descrs
				    #f)))
			  ;; (else ?actions)
			  ((else . ?actions)
			   (if (null? (cdr runner))
			       (loop (cdr runner)
				     clauses
				     descrs
				     actions)
			       (expand-time-error-clause run)))
			  (else
			   (expand-time-error-clause run))))))))
	 (else
	  (expand-time-error)))))
