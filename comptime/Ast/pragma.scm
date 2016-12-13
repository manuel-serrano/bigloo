;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/pragma.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:11:13 1996                          */
;*    Last change :  Mon Dec 12 21:13:13 2016 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The creation of pragma forms.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_pragma
   (include "Ast/node.sch")
   (import  tools_error
	    tools_location
	    backend_backend
 	    ast_sexp
	    engine_param)
   (export (pragma/type->node::node ::bool ::obj ::type e s ::obj ::symbol)
	   (get-static-pragmas::pair-nil)
	   (add-static-pragma! ::node)))

;*---------------------------------------------------------------------*/
;*    *static-pragma-list* ...                                         */
;*---------------------------------------------------------------------*/
(define *static-pragma-list* '())

;*---------------------------------------------------------------------*/
;*    get-static-pragmas ...                                           */
;*---------------------------------------------------------------------*/
(define (get-static-pragmas)
   (reverse! *static-pragma-list*))

;*---------------------------------------------------------------------*/
;*    add-static-pragma! ...                                           */
;*---------------------------------------------------------------------*/
(define (add-static-pragma! p)
   (set! *static-pragma-list* (cons p *static-pragma-list*)))

;*---------------------------------------------------------------------*/
;*    pragma/type->node ...                                            */
;*---------------------------------------------------------------------*/
(define (pragma/type->node free effect type exp stack loc site)
   (if (not (backend-pragma-support (the-backend)))
       (begin
	  (user-warning/location loc
	     "pragma"
	     "Pragma ignored with this back-end"
	     exp)
	  (sexp->node #unspecified stack loc site))
       (match-case exp
	  ((?- (and (? string?) ?format) . ?values)
	   (let ((max-index (get-max-index format))
		 (loc (find-location/loc exp loc)))
	      (if (not (=fx max-index (length values)))
		  (error-sexp->node
		     "Wrong number of arguments in `pragma' form"
		     exp
		     loc)
		  (let loop ((exps values)
			     (nodes '()))
		     (if (null? exps)
			 (instantiate::pragma
			    (loc loc)
			    (type type)
			    (format format)
			    (expr* (reverse! nodes))
			    (side-effect (not free))
			    (effect effect))
			 (loop (cdr exps)
			    (cons
			       (sexp->node (car exps)
				  stack
				  (find-location/loc (car exps) loc)
				  (if free 'value 'set!))
			       nodes)))))))
	  ((?- ?ident)
	   (let ((v (sexp->node ident stack loc site)))
	      (if (isa? v var)
		  (with-access::var v (variable)
		     (with-access::variable variable (name removable)
			(instantiate::pragma
			   (loc loc)
			   (type type)
			   (format "")
			   (expr* (list v))
			   (side-effect (not free))
			   (effect effect))))
		  (error-sexp->node "Illegal `pragma' expression" exp
		     (find-location/loc exp loc)))))
	  (else
	   (error-sexp->node "Illegal \"pragma\" form" exp loc)))))

;*---------------------------------------------------------------------*/
;*    get-max-index ...                                                */
;*---------------------------------------------------------------------*/
(define (get-max-index fmt)
   (let ((parser (regular-grammar ()
		    ((: #\$ (+ (in (#\0 #\9))))
		     (string->number (the-substring 1 (the-length))))
 		    ((+ (out #\$))
		     (ignore))
		    (else
		     (the-failure))))
	 (port   (open-input-string fmt)))
      (let loop ((exp (read/rp parser port))
		 (max 0))
	 (cond
	    ((eof-object? exp)
	     max)
	    ((char? exp)
	     (loop (read/rp parser port) max))
	    (else
	     (loop (read/rp parser port) (if (>fx exp max) exp max)))))))
   

