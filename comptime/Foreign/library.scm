;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Foreign/library.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 29 13:49:39 1999                          */
;*    Last change :  Sun Jun 10 14:42:31 2001 (serrano)                */
;*    Copyright   :  1999-2001 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Library and foreign type management.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_library
   (import engine_param
	   ast_node
	   ast_var
	   type_type
	   (find-global ast_env))
   (export (make-define-inline proto body)
	   (make-proto-inline proto)
	   (register-foreign-access-idents! . ids)
	   (prepare-foreign-access!)))

;*---------------------------------------------------------------------*/
;*    inline-enabled? ...                                              */
;*---------------------------------------------------------------------*/
(define (inline-enabled?)
   ;; otherwise, exporting foreign types in a library is bound to fail 
   #t)
;*    (not (or *lib-mode* (eq? *pass* 'make-add-heap))))               */
	    
;*---------------------------------------------------------------------*/
;*    make-define-inline ...                                           */
;*---------------------------------------------------------------------*/
(define (make-define-inline proto body)
   (if (inline-enabled?)
       `(define-inline ,proto ,body)
       `(define ,proto ,body)))

;*---------------------------------------------------------------------*/
;*    make-proto-inline ...                                            */
;*---------------------------------------------------------------------*/
(define (make-proto-inline proto)
   (if (inline-enabled?)
       `(inline ,@proto)
       proto))
	   
;*---------------------------------------------------------------------*/
;*    *registered-ident* ...                                           */
;*---------------------------------------------------------------------*/
(define *registered-ident* '())

;*---------------------------------------------------------------------*/
;*    register-foreign-access-idents! ...                              */
;*---------------------------------------------------------------------*/
(define (register-foreign-access-idents! . ids)
   (set! *registered-ident* (append ids *registered-ident*)))

;*---------------------------------------------------------------------*/
;*    prepare-foreign-access! ...                                      */
;*    -------------------------------------------------------------    */
;*    Before generating an additional heap, we have to mark all        */
;*    foreign accessors as exported.                                   */
;*    -------------------------------------------------------------    */
;*    THIS FUNCTION WAS CALLED BY MAKE-ADD-HEAP. IT IS NOT CALLED      */
;*    ANYMORE (BECAUSE FOREIGN TYPE ACCESSORS ARE RE-GENERATED         */
;*    WHEN A LIBRARY IS USED). I HAVE LEFT THIS CODE HERE JUST IN      */
;*    CASE I NEED IT IN THE FUTUR (10 jun 2001).                       */
;*    -------------------------------------------------------------    */
;*    @label foreign accesors@                                         */
;*    @ref ../Heap/make.scm:foreign accesors@                          */
;*---------------------------------------------------------------------*/
(define (prepare-foreign-access!)
   (for-each (lambda (id)
		(let ((g (find-global id)))
		   (if (global? g)
		       (begin
			  (global-import-set! g 'export)
			  ;; @label module change@
			  (global-module-set! g 'foreign))
		       (error "prepare-foreign-access!"
			      "Can't find global"
			      id))))
	     *registered-ident*)
   (set! *registered-ident* '()))
   
