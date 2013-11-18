;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tlift/capture.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 17 17:04:25 2013                          */
;*    Last change :  Sun Nov 17 17:40:26 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Mark captured local variables                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tlift_capture
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    ast_walk
	    ast_alphatize
	    ast_local
	    ast_occur
	    module_module
	    engine_param
	    tlift_types)
   (static  (wide-class local/mark::local
	       (depth::int read-only)
	       (captured::bool (default #f)))
	    (wide-class sfun/escape::sfun))
   (export  (mark-captured! ::variable)))

;*---------------------------------------------------------------------*/
;*    mark-captured! ...                                               */
;*---------------------------------------------------------------------*/
(define (mark-captured! v::variable)
   (with-access::variable v (value)
      (with-access::sfun value (body args)
	 (for-each (lambda (a)
		      (widen!::local/mark a
			 (depth 0)))
	    args)
	 (mark-captured-local body 0)
	 (for-each (lambda (a)
		      (with-access::local/mark a (captured)
			 (if captured
			     (shrink! a)
			     (widen!::local/tlift a))))
	    args))))

;*---------------------------------------------------------------------*/
;*    mark-captured-local ::node ...                                   */
;*    -------------------------------------------------------------    */
;*    Mark local captured local variables as non eligible for          */
;*    optimization.                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured-local n::node depth::int)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-captured-local ::var ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured-local n::var depth::int)
   (with-access::var n (variable)
      (when (isa? variable local/mark)
	 (with-access::local/mark variable ((d depth) captured)
	    (when (>fx depth d)
	       (set! captured #t))))))

;*---------------------------------------------------------------------*/
;*    mark-captured-local ::closure ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured-local n::closure depth::int)
   (call-default-walker)
   (with-access::closure n (variable)
      (when (isa? variable local)
	 (with-access::variable variable (value)
	    (when (isa? value sfun)
	       (widen!::sfun/escape value))))))

;*---------------------------------------------------------------------*/
;*    mark-captured-local ::let-var ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured-local n::let-var depth::int)
   (with-access::let-var n (bindings body)
      (for-each (lambda (b)
		   (widen!::local/mark (car b) (depth depth)))
	 bindings)
      (call-default-walker)
      (for-each (lambda (b)
		   (let ((a (car b)))
		      (with-access::local/mark a (captured)
			 (if captured
			     (shrink! a)
			     (widen!::local/tlift a)))))
	 bindings)))

;*---------------------------------------------------------------------*/
;*    mark-captured-local ::let-fun ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured-local n::let-fun depth::int)
   (with-access::let-fun n (locals body)
      (for-each (lambda (fun)
		   (let ((ndepth (+fx depth 1)))
		      (with-access::local fun (value)
			 (with-access::sfun value (body args)
			    (for-each (lambda (a)
					 (widen!::local/mark a
					    (depth ndepth)))
			       args)
			    (mark-captured-local body ndepth)
			    (for-each (lambda (a)
					 (with-access::local/mark a (captured)
					    (if captured
						(shrink! a)
						(widen!::local/tlift a))))
			       args)))))
	 locals)
      (mark-captured-local body depth)))
