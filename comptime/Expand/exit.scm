;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/exit.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 21 15:03:35 1995                          */
;*    Last change :  Sun Dec 12 14:52:41 2010 (serrano)                */
;*    Copyright   :  1995-2010 Manuel Serrano, see LICENSE file        */
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
   (match-case x
      ((?- (?exit) . ?body)
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
	     (replace! x new))))
      (else
       (error #f "Illegal `bind-exit' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-unwind-protect ...                                        */
;*---------------------------------------------------------------------*/
(define (expand-unwind-protect x e)
   (match-case x
      ((?- ?exp . (and (? pair?) ?cleanup))
       (let* ((val     (mark-symbol-non-user! (gensym 'val)))
	      (an-exit (mark-symbol-non-user! (gensym 'an_exit)))
	      (valbis  (mark-symbol-non-user! (gensym 'val)))
	      (eexp    (e exp e))
	      (aux     `(let ((,valbis ,eexp))
			   (pop-exit!)
			   ,valbis))
	      (eaux    (if (epair? eexp)
			   (econs (car aux) (cdr aux) (cer eexp))
			   aux)))
	  (let ((new `(let ((,val (set-exit (,an-exit)
					    (let ()
					       (push-exit! ,an-exit 0)
					       ,aux))))
			 ,(e (expand-progn cleanup) e)
			 (if (val-from-exit? ,val)
			     ((@ unwind-until! __bexit) (car ,val) (cdr ,val))
			     ,val))))
	     (replace! x new))))
      (else
       (error #f "Illegal `unwind-protect' form" x))))
			  
;*---------------------------------------------------------------------*/
;*    expand-with-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (expand-with-handler x e)
   
   (define (expand.old handler body)
      (let ((hdl (gensym 'handler))
	    (ohs (gensym 'handlers))
	    (nh (gensym 'handler))
	    (val (gensym 'val))
	    (exit (gensym 'exit))
	    (etop (gensym 'exitd))
	    (tmp (gensym 'tmp))
	    (ebody (e (expand-progn body) e)))
	 `(let ((,hdl ,(e handler e)))
	     (if (correct-arity? ,hdl 1)
		 (let ((,ohs ($get-error-handler)))
		    (let ((,val (set-exit (,exit)
					  (let ()
					     (push-exit! ,exit 0)
					     (let ((,etop ($get-exitd-top)))
						(let ((,nh (lambda (e)
							      ((@ unwind-until! __bexit)
							       ,etop
							       (,hdl e)))))
						   ($set-error-handler!
						    (cons ,nh ,ohs))
						   (let ((,tmp ,ebody))
						      (pop-exit!)
						      ,tmp)))))))
		       ($set-error-handler! ,ohs)
		       (if (val-from-exit? ,val)
			   ((@ unwind-until! __bexit) (car ,val) (cdr ,val))
			   ,val)))
		 (error 'with-handler
			"Incorrect handler arity"
			,hdl)))))
   
   (define (expand handler body)
      (let ((ohs (gensym 'ohs))
	    (err (gensym 'err))
	    (escape (gensym 'escape)))
	 (e `(bind-exit (,escape)
		(let ((,err (cons #f #unspecified))
		      (,ohs ($get-error-handler)))
		   (unwind-protect
		      (begin
			 ($set-error-handler!
			  (cons (lambda (e)
				   (set-car! ,err #t)
				   (set-cdr! ,err e)
				   (,escape e))
				,ohs))
			 ,@body)
		      (begin
			 ($set-error-handler! ,ohs)
			 (when (car ,err)
			    (,escape (,handler (cdr ,err))))))))
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

   
			  
       
