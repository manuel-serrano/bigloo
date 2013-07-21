;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/main.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 11:51:01 1996                          */
;*    Last change :  Sun Jul 21 10:58:38 2013 (serrano)                */
;*    Copyright   :  1996-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The main clause compilation.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_main
   (import module_module
	   backend_backend
	   tools_error
	   type_type
	   type_cache
	   ast_var
	   ast_env
	   engine_param)
   (export (make-main-compiler)))

;*---------------------------------------------------------------------*/
;*    make-main-compiler ...                                           */
;*    -------------------------------------------------------------    */
;*    We don't need a checksum for main clauses because, for each      */
;*    main clause, an export clause has been generated. This export    */
;*    clause will be naturally checksummed.                            */
;*---------------------------------------------------------------------*/
(define (make-main-compiler)
   (instantiate::ccomp
      (id 'main)
      (producer main-producer)
      (consumer main-consumer)))

;*---------------------------------------------------------------------*/
;*    correct-main? ...                                                */
;*    -------------------------------------------------------------    */
;*    A main is a function, which has exactly one argument of          */
;*    type `obj' or `pair'.                                            */
;*---------------------------------------------------------------------*/
(define (correct-main?::bool global::global)
   (let ((sfun (global-value global)))
      (and (sfun? sfun)
	   (=fx (sfun-arity sfun) 1)
	   (let* ((args (car (sfun-args sfun)))
		  (type (local-type args)))
	      (or (eq? type *obj*) (eq? type *pair*))))))

;*---------------------------------------------------------------------*/
;*    main-producer ...                                                */
;*---------------------------------------------------------------------*/
(define (main-producer clause)
   (if (or (eq? *main* 'imported) (global? *main*))
       (duplicate-main-error clause)
       (match-case clause
	  ((?- (and (? symbol?) ?main))
	   (let ((global (find-global/module main *module*)))
	      (if (global? global)
		  (if (not (correct-main? global))
		      (user-error *module*
			 "Illegal declaration of main function" main '()))
		  (begin
		     (if (and (>fx *bdb-debug* 0)
			      (memq 'bdb
				    (backend-debug-support (the-backend))))
			 (produce-module-clause! `(export (,main argv::obj)))
			 (produce-module-clause! `(export (,main argv::pair))))
		     (set! *main* (find-global/module main *module*))))))
	  (else
	   (user-error "Parse error" "Illegal \"main\" clause" clause '())))))

;*---------------------------------------------------------------------*/
;*    main-consumer ...                                                */
;*---------------------------------------------------------------------*/
(define (main-consumer module clause)
   (if (global? *main*)
       (duplicate-main-error clause)
       (set! *main* 'imported))
   '())

;*---------------------------------------------------------------------*/
;*    duplicate-main-error ...                                         */
;*---------------------------------------------------------------------*/
(define (duplicate-main-error clause)
   (user-error "Parse error" "Duplicated main clause" clause '()))

