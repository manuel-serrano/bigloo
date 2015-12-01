;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/exit.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 21 15:03:35 1995                          */
;*    Last change :  Tue Dec  1 08:09:09 2015 (serrano)                */
;*    Copyright   :  1995-2015 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The macro expansion of the `exit' machinery.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_exit
   (include "Expand/expander.sch"
	    "Tools/trace.sch"
	    "Tools/location.sch")
   (import  tools_args
	    tools_speek
	    tools_misc
	    expand_expander
	    expand_eps
	    expand_lambda
	    engine_param
	    type_type
	    ast_ident
	    backend_backend
	    tools_location)
   (export  (expand-jump-exit      ::obj ::procedure)
	    (expand-set-exit       ::obj ::procedure)
	    (expand-bind-exit      ::obj ::procedure)
	    (expand-unwind-protect ::obj ::procedure)
	    (expand-with-handler   ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-jump-exit ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-jump-exit x e)
   (match-case x
      ((?- ?exit . ?value)
       (let ((new `(jump-exit ,(e exit e) ,(e (expand-progn value) e))))
	  (replace! x new)))
      (else
       (error #f "Illegal 'jump-exit' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-set-exit ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-set-exit x e)
   (match-case x
      ((?- (?exit) . ?body)
       (let ((new `(set-exit (,exit) ,(e (expand-progn body) e))))
	  (replace! x new)))
      (else
       (error #f "Illegal `set-exit' form" x))))
	  
;*---------------------------------------------------------------------*/
;*    expand-bind-exit ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-bind-exit x e)

   (define (find-in-body k body)
      (cond
	 ((eq? body k)
	  #t)
	 ((pair? body)
	  (unless (eq? (car body) 'quote)
	     (or (find-in-body k (car body)) (find-in-body k (cdr body)))))
	 (else #f)))
	  
   (match-case x
      ((?- (?exit) . ?body)
       (if (not (find-in-body exit body))
	   (replace! x (e `(begin ,@body) e))
	   (let ((an-exit  (mark-symbol-non-user! (gensym 'an_exit)))
		 (an-exitd (mark-symbol-non-user! (gensym 'an_exitd)))
		 (val      (mark-symbol-non-user! (gensym 'val)))
		 (res      (mark-symbol-non-user! (gensym 'res))))
	      (let ((new (e `(set-exit (,an-exit)
				(let ()
				   (push-exit! ,an-exit 1)
				   (let ((,an-exitd ($get-exitd-top)))
				      (labels ((,exit (,val)
						  ((@ unwind-until! __bexit)
						   ,an-exitd
						   ,val)))
					 (let ((,res (begin ,@body)))
					    (pop-exit!)
					    ,res)))))
			    e)))
		 (replace! x new)))))
      (else
       (error #f "Illegal `bind-exit' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-unwind-protect ...                                        */
;*---------------------------------------------------------------------*/
(define (expand-unwind-protect x e)

   (define (new-expander exp cleanup)
      (let ((exitd (mark-symbol-non-user! (gensym 'exitd)))
	    (protect (mark-symbol-non-user! (gensym 'protect)))
	    (tmp (mark-symbol-non-user! (gensym 'tmp))))
	 (let ((new `(let ((,exitd ($get-exitd-top))
			   (,protect (lambda () ,@cleanup)))
			((@ exitd-push-protect! __bexit) ,exitd ,protect)
			(let ((,tmp ,exp))
			   ((@ exitd-pop-protect! __bexit) ,exitd)
			   (,protect)
			   ,tmp))))
	    (replace! x (e new e)))))

   (match-case x
      ((?- ?exp . (and (? pair?) ?cleanup))
       (new-expander exp cleanup))
      (else
       (error #f "Illegal `unwind-protect' form" x))))
			  
;*---------------------------------------------------------------------*/
;*    expand-with-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (expand-with-handler x e)
   
  (define (expand handler body)
     (let ((ohs (gensym 'ohs))
	   (err (gensym 'err))
	   (res (gensym 'res))
	   (escape (gensym 'escape))
	   (hdl (gensym 'handler)))
	(e `(let ((,res #unspecified)
                  (,hdl ,handler))
               (if (bind-exit (,escape)
		      (let ((,ohs ($get-error-handler)))
			 (unwind-protect
			    (begin
			       ($set-error-handler!
				  (cons (lambda (e)
					   (set! ,res e)
					   (,escape #t))
				     ,ohs))
			       (set! ,res (begin ,@body))
			       #f)
			    ($set-error-handler! ,ohs))))
		   (,hdl ,res)
		   ,res))
	   e)))

   (define (add-trace body)
      (let ((loc (find-location x)))
	 (if (and (location? loc)
		  (>fx (bigloo-compiler-debug) 0)
		  (backend-trace-support (the-backend)))
	     (let ((loc `(at ,(location-full-fname loc) ,(location-pos loc)))
		   (vid (gensym))
		   (tmp1 (mark-symbol-non-user! (gensym 'name)))
		   (tmp2 (mark-symbol-non-user! (gensym 'loc))))
		`(let ((,tmp1 'with-handler)
		       (,tmp2 ',loc))
		    (let ()
		       ($push-trace ,tmp1 ,tmp2)
		       (let ((,vid ,body))
			  ,(econs '$pop-trace '() loc)
			  ,vid))))
	     body)))
   
   (match-case x
      ((?- ?handler . ?body)
       (replace! x (add-trace (expand handler body))))
      (else
       (error #f "Illegal `with-handler' form" x))))

   
			  
       
