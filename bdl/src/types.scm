;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdl/src/types.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 22 11:11:11 2002                          */
;*    Last change :  Tue May  6 13:51:09 2003 (serrano)                */
;*    Copyright   :  2002-03 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    All the types used by the BDL library.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bdl_types

   (export (class bdl-location
	      (file::bstring read-only)
	      (line::int read-only))

	   ;; a program
	   (class bdl-program
	      (afile read-only)
	      (etags read-only)
	      (files::pair-nil read-only)
	      ;; hashtables
	      (modules read-only (default (make-hashtable)))
	      (functions read-only (default (make-hashtable)))
	      (generics read-only (default (make-hashtable)))
	      (methods read-only (default (make-hashtable)))
	      (macros read-only (default (make-hashtable)))
	      (variables read-only (default (make-hashtable)))
	      (types read-only (default (make-hashtable)))
	      (classs read-only (default (make-hashtable)))
	      (structures read-only (default (make-hashtable)))
	      (externs read-only (default (make-hashtable)))
	      ;; a virtual slots for getting all the hashtables
	      (hashtables read-only
			  (get (lambda (p::bdl-program)
				  (list (-> p modules)
					(-> p functions)
					(-> p generics)
					(-> p methods)
					(-> p macros )
					(-> p variables)
					(-> p types)
					(-> p classs)
					(-> p structures)
					(-> p externs))))))
	   
	   ;; a general Bdl entity
	   (abstract-class bdl-entity
	      (loc (default #unspecified))
	      (ident::bstring read-only))
	   
	   ;; a module definition
	   (class bdl-module::bdl-entity
	      ;; the files the module span to
	      files::pair-nil
	      ;; the module functions
	      (functions::pair-nil (default '()))
	      ;; the module variables 
	      (variables::pair-nil (default '()))
	      ;; the module classes
	      (classes::pair-nil (default '()))
	      ;; the module methods
	      (methods::pair-nil (default '()))
	      ;; the module structures
	      (structures::pair-nil (default '()))
	      ;; the module externs
	      (externs::pair-nil (default '()))
	      ;; the module macros
	      (macros::pair-nil (default '())))
	   
	   ;; a binding
	   (abstract-class bdl-binding::bdl-entity
	      (module::bdl-module read-only))
	   
	   ;; a variable definition
	   (class bdl-variable::bdl-binding
	      (proto (default #unspecified)))
	   
	   ;; a function definition
	   (class bdl-function::bdl-binding
	      (proto (default #unspecified)))
	   
	   ;; a method definition
	   (class bdl-method::bdl-function
	      (generic (default #unspecified))
	      select-type
	      return-type)
	   
	   ;; a generic function definition
	   (class bdl-generic::bdl-function
	      (methods::pair-nil (default '())))
	   
	   ;; a macro definition
	   (class bdl-macro::bdl-binding
	      (proto (default #unspecified)))
	   
	   ;; a type definition
	   (class bdl-type::bdl-binding)
	   
	   ;; a class definition
	   (class bdl-class::bdl-type
	      (super (default #unspecified))
	      (kind::symbol (default 'plain)))
	   
	   ;; a structure
	   (class bdl-structure::bdl-binding)
	   
	   ;; an external (foreign) binding
	   (class bdl-extern::bdl-binding)))


