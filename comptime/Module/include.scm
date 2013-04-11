;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/include.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 11:54:16 1996                          */
;*    Last change :  Thu Apr 11 08:52:23 2013 (serrano)                */
;*    Copyright   :  1996-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `include' clauses compilation                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_include
   (include "Ast/unit.sch")
   (import  module_module
	    tools_error
	    read_include)
   (export  (make-include-compiler)
	    (get-include-consumed-directive)
	    (reset-include-consumed-directive!)
	    (get-include-consumed-code)
	    (reset-include-consumed-code!)
	    (get-toplevel-unit)
	    (get-toplevel-unit-weight)))

;*---------------------------------------------------------------------*/
;*    make-include-compiler ...                                        */
;*---------------------------------------------------------------------*/
(define (make-include-compiler)
   (instantiate::ccomp
      (id 'include)
      (producer include-producer)
      (consumer include-consumer)
      (finalizer include-finalizer)))
 
;*---------------------------------------------------------------------*/
;*    *produced-code* ...                                              */
;*---------------------------------------------------------------------*/
(define *produced-code* '())

;*---------------------------------------------------------------------*/
;*    include-producer ...                                             */
;*---------------------------------------------------------------------*/
(define (include-producer clause)
   (define (include-error)
      (user-error "Parse error" "Illegal `include' clause" clause '()))
   (let loop ((files (cdr clause)))
      (cond
	 ((null? files)
	  '())
	 ((not (pair? files))
	  (include-error))
	 (else
	  (let ((file (car files)))
	     (if (not (string? file))
		 (include-error)
		 (let* ((src (read-include file))
			(directives (car src))
			(src-code (reverse! (cdr src))))
		    ;; we parse the directive clause
		    (when (pair? directives)
		       (for-each produce-module-clause! (cdr directives)))
		    (set! *produced-code* (append src-code *produced-code*))
		    (loop (cdr files)))))))))

;*---------------------------------------------------------------------*/
;*    *consumed-directive* ...                                         */
;*---------------------------------------------------------------------*/
(define *consumed-directive* '())

;*---------------------------------------------------------------------*/
;*    get-include-consumed-directive ...                               */
;*---------------------------------------------------------------------*/
(define (get-include-consumed-directive)
   *consumed-directive*)

;*---------------------------------------------------------------------*/
;*    reset-include-consumed-directive! ...                            */
;*---------------------------------------------------------------------*/
(define (reset-include-consumed-directive!)
   (set! *consumed-directive* '()))

;*---------------------------------------------------------------------*/
;*    *consumed-code* ...                                              */
;*---------------------------------------------------------------------*/
(define *consumed-code* '())

;*---------------------------------------------------------------------*/
;*    get-include-consumed-code ...                                    */
;*---------------------------------------------------------------------*/
(define (get-include-consumed-code)
   *consumed-code*)

;*---------------------------------------------------------------------*/
;*    reset-include-consumed-code! ...                                 */
;*---------------------------------------------------------------------*/
(define (reset-include-consumed-code!)
   (set! *consumed-code* '()))

;*---------------------------------------------------------------------*/
;*    include-consumer ...                                             */
;*---------------------------------------------------------------------*/
(define (include-consumer module::symbol clause)
   (define (include-error)
      (user-error "Parse error" "Illegal `include' clause" clause '()))
   (let loop ((files (cdr clause)))
      (cond
	 ((null? files)
	  '())
	 ((not (pair? files))
	  (include-error))
	 (else
	  (let ((file (car files)))
	     (if (not (string? file))
		 (include-error)
		 (let* ((src       (read-include file))
			(directive (car src))
			(src-code  (cdr src)))
		    (begin
		       ;; we parse the directive clause
		       (if (pair? directive)
			   (for-each
			    (lambda (d)
			       (set! *consumed-directive*
				     (append (consume-module-clause! module d)
					     *consumed-directive*)))
			    (cdr directive)))
		       (set! *consumed-code* (append src-code *consumed-code*))
		       (loop (cdr files))))))))))

;*---------------------------------------------------------------------*/
;*    *toplevel-unit* ...                                              */
;*---------------------------------------------------------------------*/
(define *toplevel-unit* #f)

;*---------------------------------------------------------------------*/
;*    get-toplevel-unit ...                                            */
;*---------------------------------------------------------------------*/
(define (get-toplevel-unit)
   (assert (*toplevel-unit*) (unit? *toplevel-unit*))
   *toplevel-unit*)

;*---------------------------------------------------------------------*/
;*    get-toplevel-unit-weight ...                                     */
;*---------------------------------------------------------------------*/
(define (get-toplevel-unit-weight)
   100)

;*---------------------------------------------------------------------*/
;*    include-finalizer ...                                            */
;*---------------------------------------------------------------------*/
(define (include-finalizer)
   (set! *toplevel-unit* (unit 'toplevel
			       (get-toplevel-unit-weight)
			       (reverse! *produced-code*)
			       #t
			       #f))
   (list *toplevel-unit*))

