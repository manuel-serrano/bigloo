;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/expdargs.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr  1 06:28:06 2000                          */
;*    Last change :  Tue Aug 13 07:29:47 2013 (serrano)                */
;*    Copyright   :  2001-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    args-parse expansion.                                            */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/cmdline.texi@                             */
;*       @node Command Line Parsing@                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __expander_args
   
   (import  __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __hash
	    __param
	    __object
	    __thread
	    __match_normalize
	     
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
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __r5_control_features_6_4
	    
	    __progn
	    __expand)
   
   (use     __type
	    __evenv
	    __bit)
   
   (export  (expand-args-parse ::pair-nil ::procedure)
	    (args-parse-usage::procedure ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    expand-time-error ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-time-error x)
   (expand-error "args-parse" "Illegal syntax" x))

;*---------------------------------------------------------------------*/
;*    expand-time-error-clause ...                                     */
;*---------------------------------------------------------------------*/
(define (expand-time-error-clause clause msg)
   (expand-error "args-parse" msg clause))
 
;*---------------------------------------------------------------------*/
;*    expand-args-parse ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-args-parse x e)
   (match-case x
      ((?- ?- ?- . ?-)
       (e (do-expand-args-parse x e) e))
      (else
       (expand-time-error x))))

;*---------------------------------------------------------------------*/
;*    do-expand-args-parse ...                                         */
;*---------------------------------------------------------------------*/
(define (do-expand-args-parse x e)
   (let* ((exp (cadr x))
	  (clauses (cddr x))
	  (otable (make-hashtable 20))
	  (parsers (map (lambda (c) (make-parser c otable)) clauses))
	  (last-parser `(lambda (a v)
			   (if (null? a)
			       (values 'end a v)
			       (error (car a) "Illegal option" "see -help"))))
	  (descrs (filter (lambda (x) x) (map make-help clauses))))
      `(let* ((args-parse-usage
		 (args-parse-usage
		    ,(list (if (any (lambda (f)
				       (and (pair? f)
					    (pair? (cdr f))
					    (eq? (cadr f) 'unquote)))
				  descrs)
			       'quasiquote
			       'quote)
			descrs)))
	      (p* (list ,last-parser))
	      (a* ,exp))
	  ,@(map (lambda (p) `(set! p* (cons ,p p*)))
	       (reverse! (filter pair? parsers)))
	  (if (not (list? a*))
	      (error 'args-parse "Illegal argument list" a*)
	      (let loop ((a* a*)
			 (v #f))
		 (let liip ((p* p*))
		    (multiple-value-bind (action na* nv)
		       ((car p*) a* v)
		       (case action
			  ((next)
			   (loop na* nv))
			  ((fail)
			   (liip (cdr p*)))
			  ((end)
			   nv)))))))))

;*---------------------------------------------------------------------*/
;*    make-help ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-help clause)
   (match-case clause
      ((section ?section)
       (cons 'section section))
      ((() . ?-)
       #f)
      ((else . ?-)
       #f)
      (((?-) . ?-)
       #f)
      ((?opt . ?-)
       (let ((syn (car (last-pair opt))))
	  (match-case syn
	     ((?synopsis ?name ?msg)
	      (if (not (synopsis? synopsis))
		  (expand-time-error-clause clause "Illegal help message")
		  (cons name (if (string? msg) msg (list 'unquote msg)))))
	     ((?synopsis ?msg)
	      (if (not (synopsis? synopsis))
		  (expand-time-error-clause clause "Illegal help message")
		  (cons (make-synopsis-name clause)
			(if (string? msg) msg (list 'unquote msg)))))
	     (else
	      #f))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    synopsis-arg ...                                                 */
;*---------------------------------------------------------------------*/
(define (synopsis-arg arg)
   (string-upcase arg))

;*---------------------------------------------------------------------*/
;*    make-synopsis-name ...                                           */
;*---------------------------------------------------------------------*/
(define (make-synopsis-name clause)
   (define (make-simple-synopsis-name opt o args)
      (multiple-value-bind (oid aid)
	 (fetch-option-embed-argument o)
	 (cond
	    ((and aid (pair? args))
	     (expand-time-error-clause clause "Illegal clause"))
	    ((not (or aid (pair? args)))
	     oid)
	    (aid
	     (string-append oid (synopsis-arg aid)))
	    (else
	     (string-append
	      oid
	      (let loop ((args args))
		 (if (null? args)
		     ""
		     (string-append " "
				    (synopsis-arg 
				     (fetch-argument-name (car args) clause))
				    (loop (cdr args))))))))))
   (define (make-multiple-synopsis-name opt o+ args)
      (define (concat l)
	 (if (null? (cdr l))
	     (car l)
	     (string-append (car l)
			    ","
			    (concat (cdr l)))))
      (multiple-value-bind (oid+ aid+)
	 (let loop ((o+ o+)
		    (oid+ '())
		    (aid+ '()))
	    (if (null? o+)
		(values (reverse! oid+) (reverse! aid+))
		(multiple-value-bind (oid aid)
		   (fetch-option-embed-argument (car o+))
		   (loop (cdr o+) (cons oid oid+) (cons aid aid+)))))
	 (cond
	    ((not (and (pair? aid+) (any (lambda (x) x) aid+)))
	     (string-append
	      (concat oid+)
	      (let loop ((args args))
		 (if (null? args)
		     ""
		     (string-append " "
				    (synopsis-arg
				     (fetch-argument-name (car args) clause))
				    (loop (cdr args)))))))
	    ((null? args)
	     (if (null? aid+)
		 (concat oid+)
		 (apply string-append
			(concat oid+)
			(map (lambda (a)
				(string-append " " (synopsis-arg a )))
			     aid+))))
	    (else
	     (expand-time-error-clause clause "Illegal options")))))
   (let* ((opt (car clause))
	  (o (car opt))
	  (args (fetch-option-arguments opt)))
      (cond
	 ((string? o)
	  (make-simple-synopsis-name opt o args))
	 ((and (list? o) (every string? o))
	  (make-multiple-synopsis-name opt o args))
	 (else
	  (expand-time-error-clause clause "Illegal clause")))))

;*---------------------------------------------------------------------*/
;*    make-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-parser clause otable)
   (match-case clause
      ((section ?section)
       #f)
      ((() . ?expr*)
       (let ((a (gensym))
	     (v (gensym)))
	  `(lambda (,a ,v)
	      (if (null? ,a)
		  (values 'end ,a (begin ,@expr*))
		  (values 'fail ,a ,v)))))
      ((else . ?expr*)
       (let ((a (gensym))
	     (v (gensym)))
	  `(lambda (,a ,v)
	      (if (pair? ,a)
		  (let ((else (car ,a))
			(rest ,a))
		     (values 'next (cdr ,a) (begin ,@expr*)))
		  (values 'fail ,a ,v)))))
      (((? list?) . ?-)
       (make-opt-parser clause otable))
      (else
       (expand-time-error-clause clause "Illegal clause"))))

;*---------------------------------------------------------------------*/
;*    bind-option! ...                                                 */
;*---------------------------------------------------------------------*/
(define (bind-option! otable oid clause)
   (let ((old (hashtable-get otable oid)))
      (if old
	  (warning 'args-parse oid " -- Option overridden:"
		   #\Newline "  " old
		   #\Newline "  " clause)
	  (hashtable-put! otable oid clause))))

;*---------------------------------------------------------------------*/
;*    make-opt-parser ...                                              */
;*---------------------------------------------------------------------*/
(define (make-opt-parser clause otable)
   (let* ((opt (car clause))
	  (o (car opt)))
      (cond
	 ((string? o)
	  (make-simple-opt-parser clause otable))
	 ((and (list? o) (every string? o))
	  (make-multiple-opt-parser clause otable))
	 (else
	  (expand-time-error-clause clause "Illegal option")))))

;*---------------------------------------------------------------------*/
;*    make-simple-opt-parser ...                                       */
;*---------------------------------------------------------------------*/
(define (make-simple-opt-parser clause otable)
   (let* ((opt (car clause))
	  (o (car opt))
	  (args (fetch-option-arguments opt))
	  (expr* (cdr clause)))
      (multiple-value-bind (oid aid)
	 (fetch-option-embed-argument o)
	 (cond
	    ((and aid (pair? args))
	     (expand-time-error-clause clause "Illegal options"))
	    ((string? aid)
	     (bind-option! otable oid clause)
	     (let ((a (gensym))
		   (v (gensym))
		   (oidl (string-length oid)))
		`(lambda (,a ,v)
		    (if (and (pair? ,a)
			     (substring=? ,oid (car ,a) ,oidl))
			(let* ((,(string->symbol aid)
				(substring (car ,a)
					   ,oidl
					   (string-length (car ,a))))
			       (the-remaining-args (cdr ,a))
			       (nv (begin ,@expr*)))
			   (values 'next the-remaining-args nv))
			(values 'fail ,a ,v)))))
	    (else
	     (bind-option! otable oid clause)
	     (let ((a (gensym))
		   (v (gensym))
		   (na 'the-remaining-args))
		`(lambda (,a ,v)
		    (if (and (pair? ,a) (string=? ,oid (car ,a)))
			(let* (,@(bind-option-arguments args a na clause)
				 (nv (begin ,@expr*)))
			   (values 'next ,na nv))
			(values 'fail ,a ,v)))))))))

;*---------------------------------------------------------------------*/
;*    make-multiple-opt-parser ...                                     */
;*---------------------------------------------------------------------*/
(define (make-multiple-opt-parser clause otable)
   (let* ((opt (car clause))
	  (o+ (car opt))
	  (args (fetch-option-arguments opt))
	  (expr* (cdr clause)))
      (multiple-value-bind (oid+ aid+)
	 (let loop ((o+ o+)
		    (oid+ '())
		    (aid+ '()))
	    (if (null? o+)
		(values (reverse! oid+) (reverse! aid+))
		(multiple-value-bind (oid aid)
		   (fetch-option-embed-argument (car o+))
		   (loop (cdr o+) (cons oid oid+) (cons aid aid+)))))
	 (cond
	    ((not (and (pair? aid+) (any (lambda (x) x) aid+)))
	     (for-each (lambda (o) (bind-option! otable o clause)) oid+)
	     (let ((a (gensym))
		   (v (gensym))
		   (na 'the-remaining-args))
		`(lambda (,a ,v)
		    (if (and (pair? ,a)
			     (or ,@(map (lambda (o)
					   `(string=? ,o (car ,a)))
					o+)))
			(let* (,@(bind-option-arguments args a na clause)
				 (nv (begin ,@expr*)))
			   (values 'next ,na nv))
			(values 'fail ,a ,v)))))
	    ((null? args)
	     (for-each (lambda (o) (bind-option! otable o clause)) oid+)
	     (let ((a (gensym))
		   (v (gensym))
		   (na 'the-remaining-args))
		`(lambda (,a ,v)
		    (cond
		       ((pair? ,a)
			(cond ,@(map (lambda (oid aid)
					(if aid
					    (let ((oidl (string-length oid)))
					       `((substring=? ,oid (car ,a) ,oidl)
						 (let* ((,(string->symbol aid)
							 (substring
							  (car ,a)
							  ,oidl
							  (string-length (car ,a))))
							,@(bind-option-arguments args a na clause)
							(nv (begin ,@expr*)))
						    (values 'next ,na nv))))
					    `(else (values 'fail ,a ,v))))
				     oid+ aid+)
			      (else (values 'fail ,a ,v))))
		       (else
			(values 'fail ,a ,v))))))
	    (else
	     (expand-time-error-clause clause "Illegal options"))))))

;*---------------------------------------------------------------------*/
;*    fetch-option-arguments ...                                       */
;*    -------------------------------------------------------------    */
;*    extract the argument of a line (?top ??args (help ...))          */
;*---------------------------------------------------------------------*/
(define (fetch-option-arguments opt)
   (let loop ((i (cdr opt))
	      (res '()))
      (cond
	 ((or (null? i) (help-message? (car i)))
	  (reverse! res))
	 (else
	  (loop (cdr i) (cons (car i) res))))))

;*---------------------------------------------------------------------*/
;*    bind-option-arguments ...                                        */
;*---------------------------------------------------------------------*/
(define (bind-option-arguments args a+ na clause)
   (cons `(,na (cdr ,a+))
	 (let loop ((args args))
	    (if (pair? args)
		(let ((id (fetch-argument-name (car args) clause)))
		   (cons* `(,(string->symbol id)
			    (if (pair? ,na)
				(car ,na)
				(error ',(car args) "missing argument" ',clause)))
			  `(,na (cdr ,na))
			  (loop (cdr args))))
		'()))))

;*---------------------------------------------------------------------*/
;*    fetch-argument-name ...                                          */
;*---------------------------------------------------------------------*/
(define (fetch-argument-name a clause)
   (if (not (symbol? a))
       (expand-time-error-clause clause "Illegal option argument")
       (let ((s (symbol->string a)))
	  (if (not (char=? (string-ref s 0) #\?))
	      (expand-time-error-clause
	       clause
	       (string-append "Illegal option argument `" s "'"))
	      (substring s 1 (string-length s))))))
      
;*---------------------------------------------------------------------*/
;*    fetch-option-embed-argument ...                                  */
;*    -------------------------------------------------------------    */
;*    extract argument name from an option name (i.e. -I?dir)          */
;*---------------------------------------------------------------------*/
(define (fetch-option-embed-argument opt)
   (let ((len (-fx (string-length opt) 1)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i len)
	     (values opt #f))
	    ((char=? (string-ref opt i) #\?)
	     (values (substring opt 0 i)
		     (substring opt (+fx 1 i) (+fx len 1))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    help-message? ...                                                */
;*---------------------------------------------------------------------*/
(define (help-message? exp)
   (match-case exp
      (((? synopsis?) . ?-) #t)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    synopsis? ...                                                    */
;*---------------------------------------------------------------------*/
(define (synopsis? sym)
   (or (eq? sym 'help) (eq? sym 'synopsis)))

;*---------------------------------------------------------------------*/
;*    args-parse-usage ...                                             */
;*---------------------------------------------------------------------*/
(define (args-parse-usage descrs)
   (lambda (manual?)
      (if manual? (print "("))
      (let ((descrs descrs)
	    (mlen-sym 0))
	 (for-each (lambda (opt)
		      (let ((name(car opt)))
			 (if (string? name)
			     (let ((len (string-length name)))
				(if (>fx len mlen-sym)
				    (set! mlen-sym len))))))
		   descrs)
	 (for-each (lambda (opt)
		      (let ((name (car opt)))
			 (cond
			    ((string? name)
			     (let* ((name (car opt))
				    (len  (string-length name))
				    (desc (cdr opt))
				    (tab  (make-string (-fx mlen-sym len)
						       #\space)))
				(if manual?
				    (begin
				       (write `(,name ,desc))
				       (newline))
				    (print "   " name tab " " desc))))
			    ((eq? name 'section)
			     (print #\Newline (cdr opt) ":")))))
		   descrs)
	 'usage-done)
      (if manual? (print ")"))))
