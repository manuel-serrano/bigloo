;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/creator.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Fri Nov 25 15:28:15 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the class constructors                                   */
;*    -------------------------------------------------------------    */
;*    In this module we cannot use consume-module-clause! because      */
;*    the importation are already done.                                */
;*    -------------------------------------------------------------    */
;*    This constructors does not require any importation information   */
;*    since all accessors are always static.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_creator
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    tools_location
	    tools_shape
	    engine_param
	    backend_backend
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_ident
	    ast_private
	    object_class
	    object_slots
	    object_tools
	    object_nil
	    module_module
	    module_impuse)
   (export  (import-java-class-creator ::jclass ::pair-nil ::pair)
	    (gen-java-class-constructors ::jclass ::pair-nil ::pair)))

;*---------------------------------------------------------------------*/
;*    inline-creators? ...                                             */
;*    -------------------------------------------------------------    */
;*    This function returns #t if the class creatorss must be          */
;*    inlined.                                                         */
;*---------------------------------------------------------------------*/
(define (inline-creators?)
   (and (>=fx *optim* 2)
	(or (not (number? *profile-mode*))
	    (<fx *profile-mode* 1))
	(not (memq *pass* '(make-heap make-add-heap)))))
   
;*---------------------------------------------------------------------*/
;*    gen-java-class-creator ...                                       */
;*---------------------------------------------------------------------*/
(define (gen-java-class-creator class src-def)
   (let* ((id (jclass-id class))
	  (alloc-id (symbol-append '%allocate- id)))
      (define (jvm-allocate)
	 `(define-inline (,alloc-id)
	     ,(make-private-sexp 'new id)))
      (produce-module-clause! `(static (inline ,alloc-id)))
      (list (epairify* (jvm-allocate) src-def))))

;*---------------------------------------------------------------------*/
;*    import-java-class-creator ...                                    */
;*---------------------------------------------------------------------*/
(define (import-java-class-creator class constrs src-def)
   (if (pair? constrs)
       (gen-java-class-creator class src-def)
       '()))

;*---------------------------------------------------------------------*/
;*    gen-java-class-constructors ...                                  */
;*---------------------------------------------------------------------*/
(define (gen-java-class-constructors class constrs src-def)
   (define (gen-one-constructor constr)
      (match-case constr
	 ((?ident . ?args-type)
	  ;; if we have reached that point, it means that the class
	  ;; declaration has alreay been enterily parsed so there is no need
	  ;; now to make any user correctness check
	  (let* ((jid (jclass-id class))
		 (loc (find-location constrs))
		 (cid (fast-id-of-id ident loc))
		 (tid (make-typed-ident (symbol-append jid '- cid) jid))
		 (args-id (map (lambda (_) (mark-symbol-non-user! (gensym)))
			     args-type))
		 (self (mark-symbol-non-user! (gensym)))
		 (tself (make-typed-ident self jid))
		 (targs (map (lambda (id type)
				(symbol-append id type))
			   args-id args-type))
		 (def `(define (,tid ,@targs)
			  ;; call the actual Java constructor
			  ,(apply make-private-sexp 'new jid
			      `',(map (lambda (a)
					 (type-id
					    (type-of-id a loc)))
				    args-type)
			      args-id)))
		 (scope (if (inline-creators?) 'static 'export))
		 (mod `(,scope (,tid ,@args-type))))
	     (produce-module-clause! mod)
	     (epairify* def constr)))))
   (map gen-one-constructor constrs))
