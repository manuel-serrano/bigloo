;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/comptime/Module/extern5.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Jun 11 08:51:54 2026                          */
;*    Last change :  Mon Jun 15 07:47:07 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 extern plugins                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_extern5
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   
   (import engine_param
	   tools_error
	   tools_shape
 	   tools_location
	   tools_misc
	   read_jvm
	   backend_backend
	   module_module
	   module_class
	   module_checksum
	   module_pragma
	   module_foreign
	   module_java
	   module_type
	   module_eval
	   heap_restore
	   expand_eps
	   expand_object
	   expand_assert
	   ast_node
	   ast_var
	   ast_env
	   ast_glo-decl
	   ast_ident
	   ast_toplevel
	   ast_build
	   ast_sexp
	   ast_private
	   ast_walk
	   type_type
	   type_env
	   type_cache
	   object_class
	   object_slots
	   object_coercion
	   foreign_jtype)

   (export (class CDef::Def
	      (args read-only)
	      (name::bstring read-only)
	      (infix::bool read-only (default #f))
	      (macro::bool read-only (default #f))
	      (module::symbol read-only)
	      (modifiers::pair-nil read-only (default '())))

	   (class TDef::Def
	      (name::bstring read-only))

	   (class JDef::TDef
	      (super::obj read-only)
	      (package::bstring read-only))

	   (module5-extern-plugin-preprocessor cmd::bstring file::bstring x mod::Module)
	   (parse-ident id::symbol)
	   (error/loc mod msg obj container)))
	   
;*---------------------------------------------------------------------*/
;*    object-copy ::CDef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::CDef)
   (duplicate::CDef d))

;*---------------------------------------------------------------------*/
;*    object-copy ::TDef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::TDef)
   (duplicate::TDef d))

;*---------------------------------------------------------------------*/
;*    object-copy ::JDef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::JDef)
   (duplicate::JDef d))

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-preprocessor ...                           */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-preprocessor cmd::bstring file::bstring x mod::Module)
   (with-trace 'module_module5 "module5-extern-plugin-preprocessor"
      (trace-item "cmd=" cmd)
      (trace-item "file=" file)
      (let ((path (if (file-name-absolute? file)
		      file
		      (make-file-name (dirname (-> mod path)) file))))
	 (trace-item "path=" path)
	 (let* ((cache-dir (make-file-path *module-cache-dir* "preprocessor"))
		(lock-path (make-file-name cache-dir "LOCK"))
		(cache (make-file-name cache-dir
			  (string-append (string-replace file #\/ #\_)
			     ".bgh"))))
	    (trace-item "cache=" cache)
	    (make-directories cache-dir)
	    (unless (directory? cache-dir)
	       (error/loc mod "Cannot create cache directory"
		  cache-dir x))
	    (call-with-output-file lock-path
	       (lambda (lock)
		  (lockf lock 'lock)
		  (unwind-protect
		     (if (or (not (file-exists? cache))
			     (and (file-exists? path)
				  (<elong (file-modification-time cache)
				     (file-modification-time path))))
			 (let ((cmd (format "~a/~a -cp ~a -s --module5 ~a -o ~a"
				       (bigloo-config 'binary-directory)
				       cmd
				       (dirname (-> mod path))
				       (if (file-exists? path) path file)
				       cache)))
			    (trace-item "cmd=" cmd)
			    (if (=fx (system cmd) 0)
				cache
				(begin
				   (when (file-exists? cache)
				      (delete-file cache))
				   (error/loc mod
				      (format "~a Cannot preprocess" cmd)
				      file x))))
			 cache)
		     (lockf lock 'ulock))))))))

;*---------------------------------------------------------------------*/
;*    parse-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-ident id::symbol)
   (let* ((s (symbol->string id))
	  (l (string-length s)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i (-fx l 2))
	     (values id #unspecified))
	    ((char=? (string-ref s i) #\:)
	     (if (char=? (string-ref s (+fx i 1)) #\:)
		 (values (string->symbol (substring s 0 i))
		    (substring s (+fx i 2)))
		 (loop (+fx i 1))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc mod msg obj container)
   (let ((id (if (isa? mod Module)
		 (with-access::Module mod (id) id)
		 "module5")))
      (match-case (cond
		   ((epair? obj) (cer obj))
		   ((epair? container) (cer container))
		   (else #f))
	 ((at ?fname ?loc) (error/location id msg obj fname loc))
	 (else (error id msg obj)))))

