;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cnst/initialize.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 20 15:50:19 1995                          */
;*    Last change :  Sat May 28 10:28:47 2016 (serrano)                */
;*    Copyright   :  1995-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The initialize function definition.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cnst_initialize
   (include "Tools/trace.sch"
	    "Ast/unit.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    engine_param
	    backend_backend
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_env
	    ast_build
	    ast_lvtype
	    coerce_coerce 
	    cnst_alloc
	    cnst_node)
   (export  (initialize-ast)))

;*---------------------------------------------------------------------*/
;*    initialize-ast ...                                               */
;*    -------------------------------------------------------------    */
;*    Now, we have finished the ast walk, we can set the               */
;*    initialization function definition.                              */
;*---------------------------------------------------------------------*/
(define (initialize-ast)
   (let ((body (case *init-mode*
			((lib)
			 (lib-initialize!))
			((read)
			 (read-initialize!))
			((intern)
			 (intern-initialize!))
			(else
			 (internal-error "intialize-stop!"
					 "Illegal init-mode"
					 *init-mode*)))))
      (if (pair? body)
	  (let ((unit (unit 'cnst 8 body #t (eq? *object-init-mode* 'staged))))
	     (let ((ast (build-ast (list unit))))
		(for-each (lambda (global)
			     (let ((body (sfun-body (global-value global))))
				(lvtype-node! body)
				(coerce! body global (global-type global) #f)))
			  ast)
		ast))
	  '())))

;*---------------------------------------------------------------------*/
;*    lib-initialize! ...                                              */
;*---------------------------------------------------------------------*/
(define (lib-initialize!)
   (global-name-set! (get-cnst-table) (backend-cnst-table-name (the-backend) 0))
   (global-user?-set! (get-cnst-table) #f)
   (get-cnst-sexp))

;*---------------------------------------------------------------------*/
;*    read-initialize! ...                                             */
;*---------------------------------------------------------------------*/
(define (read-initialize!)
   (if (=fx (get-cnst-offset) 0)
       (read-empty-cnst-initialize!)
       (read-full-cnst-initialize!)))

;*---------------------------------------------------------------------*/
;*    read-empty-cnst-initialize! ...                                  */
;*---------------------------------------------------------------------*/
(define (read-empty-cnst-initialize!)
   (global-name-set! (get-cnst-table) (backend-cnst-table-name (the-backend) 0))
   (global-user?-set! (get-cnst-table) #f)
   (get-cnst-sexp))

;*---------------------------------------------------------------------*/
;*    read-full-cnst-initialize! ...                                   */
;*---------------------------------------------------------------------*/
(define (read-full-cnst-initialize!)
   
   (define (read-full-cnst-initialize/small-string cnst-string)
      (let* ((var-string  (cnst-alloc-string cnst-string #f))
	     (sexp `(let ((cport::input-port
			   (open-input-string!
			      ,(global-id (var-variable var-string)))))
		       (labels ((loop (i::long)
				      (if (c-=fx i -1)
					  #unspecified
					  (begin
					     (cnst-table-set!
					      i
					      ((@ read __reader) cport))
					     (let ((aux::int (c--fx i 1)))
						(loop aux))))))
			  (loop ,(-fx (get-cnst-offset) 1))))))
	 (cons sexp (get-cnst-sexp))))
   
   (global-name-set! (get-cnst-table)
      (backend-cnst-table-name (the-backend) (get-cnst-offset)))
   (global-user?-set! (get-cnst-table) #f)
   
   (let ((cnst-string (cnst-set->cnst-string (get-cnst-set))))
      (read-full-cnst-initialize/small-string cnst-string)))

;*---------------------------------------------------------------------*/
;*    cnst-set->cnst-string ...                                        */
;*    -------------------------------------------------------------    */
;*    What we call a `set' is just a list of constants.                */
;*    To build the string, we just print it !                          */
;*---------------------------------------------------------------------*/
(define (cnst-set->cnst-string set)
   (let ((port (open-output-string)))
      (if (not (output-port? port))
	  (internal-error "cnst-set->cnst-string"
			  "Can't open output string port"
			  port)
	  (begin
	     (for-each (lambda (cnst)
			  (write cnst port)
			  (write-char #\space port))
		       set)
	     (close-output-port port)))))
  
;*---------------------------------------------------------------------*/
;*    intern-initialize! ...                                           */
;*---------------------------------------------------------------------*/
(define (intern-initialize!)
   (if (=fx (get-cnst-offset) 0)
       (intern-empty-cnst-initialize!)
       (intern-full-cnst-initialize!)))

;*---------------------------------------------------------------------*/
;*    intern-empty-cnst-initialize! ...                                */
;*---------------------------------------------------------------------*/
(define (intern-empty-cnst-initialize!)
   (global-name-set! (get-cnst-table)
		     (backend-cnst-table-name (the-backend) 0))
   (get-cnst-sexp))

;*---------------------------------------------------------------------*/
;*    intern-full-cnst-initialize! ...                                 */
;*---------------------------------------------------------------------*/
(define (intern-full-cnst-initialize!)

   (define (intern-full-cnst-initialize/small-string cnst-string)
      (let* ((var-string  (cnst-alloc-string cnst-string #f))
	     (sexp `(let ((cnst-tmp::vector
			     ((@ string->obj __intext)
			      ,(global-id
				  (var-variable var-string)))))
		       (labels ((loop (i::int)
				   (if (c-=fx i -1)
				       #unspecified
				       (begin
					  (cnst-table-set!
					     i
					     ($vector-ref-ur cnst-tmp i))
					  (let ((aux::int (c--fx i 1)))
					     (loop aux))))))
			  (loop ,(-fx (get-cnst-offset) 1))))))
	 (cons sexp (get-cnst-sexp))))
   
   (global-name-set! (get-cnst-table)
      (backend-cnst-table-name (the-backend) (get-cnst-offset)))
   
   (let* ((cnst-string (obj->string (list->vector (reverse! (get-cnst-set))))))
      (intern-full-cnst-initialize/small-string cnst-string)))

 
