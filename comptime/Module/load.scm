;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/load.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  7 08:44:07 1996                          */
;*    Last change :  Fri Jun 21 08:10:25 2013 (serrano)                */
;*    Copyright   :  1996-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The load clause compilation                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_load
   (import module_module
	   engine_param
	   tools_error
	   type_type
	   ast_var
	   ast_env
	   type_env
	   tools_speek
	   read_load)
   (export (make-load-compiler)))

;*---------------------------------------------------------------------*/
;*    make-load-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-load-compiler)
   (instantiate::ccomp (id 'load)
		       (producer load-producer)))

;*---------------------------------------------------------------------*/
;*    load-producer ...                                                */
;*---------------------------------------------------------------------*/
(define (load-producer clause)
   (match-case clause
      ((?- . ?protos)
       (for-each (lambda (proto) (load-parser proto clause)) protos))
      (else
       (user-error "Parse error"
		   (string-append "Illegal `load' clause")
		   clause
		   '()))))
   
;*---------------------------------------------------------------------*/
;*    load-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (load-parser proto clause)
   (match-case proto
      (((and ?module (? symbol?)) (and ?file (? string?)) . ?files)
       (let loop ((f files))
	  (cond
	     ((null? f)
	      (load-module module (cons file files)))
	     ((not (string? (car f)))
	      (user-error "Parse error" "Illegal load clause" clause '()))
	     (else
	      (loop (cdr f))))))
      ((? symbol?)
       (let* ((abase (map dirname *access-files*))
	      (b ((bigloo-module-resolver) proto '() abase)))
	  (if (not (pair? b))
	      (user-error "load" "Can't load module" proto)
	      (load-module proto b))))
      (else
       (user-error "Parse error" "Illegal load clause" clause '()))))

