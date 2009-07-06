(module msil_names
   (import type_type ast_var ast_node
	   object_class
	   object_slots
	   tvector_tvector
	   foreign_jtype
	   read_jvm
	   backend_cplib
	   )
   (export (msil-set-type-names! type::type)
	   (msil-set-global-names! var)
	   (global-fullname::bstring var::global)
	   (global-simplename::bstring var::global)
	   (std-typename id)) )

;;
;; Associate a name to a global
;;
(define (global-fullname::bstring var::global)
   (global-name var) )

(define (global-simplename::bstring var::global)
   (let ( (s (global-name var)) )
      (let ( (n (string-length s)) )
	 (let loop ( (i 0) )
	    (if (char=? (string-ref s i) #\:)
		(substring s (+fx i 2) n)
		(loop (+fx i 1))) ))))

(define (msil-set-global-names! var)
   (with-access::global var (type module name)
      (global-name-set! var
			(string-append (type-name type) " '"
				       (module->qualified-type module) "'::'"
				       name "'" ))))

;;
;; Associate name to a type
;;
(define **basic-type**
   '((bool . "bool")
     (char . "unsigned int8")
     (uchar . "int32")
     (byte . "int8")
     (ubyte . "unsigned int8")
     (ucs2 . "char")
     (void . "void")
     (short . "int16")
     (ushort . "unsigned int16")
     (int . "int32")
     (uint . "unsigned int32")
     (long . "int32")
     (ulong . "unsigned int32")
     (elong . "int64")
     (uelong . "unsigned int64")
     (llong . "int64")
     (ullong . "unsigned int64")
     (float . "float32")
     (double . "float64")
     (magic . "class System.Object")
     (obj . "class System.Object")
     (pair-nil . "class System.Object")
     (vector . "class System.Object[]")
     (void* . "class System.Object")
     (tvector . "class System.Object")
     (s8vector . "class bigloo.s8vector")
     (cnst* . "class System.Object[]")
     (bstring . "unsigned int8[]")
     (ucs2string . "char[]")
     (string . "unsigned int8[]")
     (procedure-el . "class System.Object[]")
     (procedure-el1 . "class System.Object")
     (dynamic-env . "class bigloo.bgldynamic")
     ;; Change names
     (struct . "class bigloo.bstruct")
     (epair . "class bigloo.extended_pair")
     ;; MANU
     (output-port . "class bigloo.output_port")
     (input-port . "class bigloo.input_port")
     (binary-port . "class bigloo.binary_port")
     ))

(define **lib-type**
   '((exception . "class System.Exception")
     (bexception . "class bigloo.bexception")
     (exit . "class 'bigloo.exit'")
     (fstrings . "class System.String[]")
     (fstring . "class System.String")
     (belong . "class bigloo.belong")
     (bllong . "class bigloo.bllong")
     ))
		       
(define (std-typename id)
   (cond
      ((assq id **basic-type**) => cdr)
      ((assq id **lib-type**) => cdr)
      (else #f) ))

(define (msil-set-type-names! type::type)
   (type-name-set! type (build-type-name type)) )

(define (build-type-name::bstring type::type)
   (define (vname t) (string-append (build-type-name t) "[]"))
   (define (prefix-class name) (string-append "class '" name "'"))
   (cond
      ((tclass? type)
       (if (eq? (type-id type) 'object)
	   "class bigloo.bobject"
	   (prefix-class (qualified-tclass-name type)) ))
      ((wclass? type)
       (prefix-class (qualified-wclass-name type)) )
      ((jclass? type)
       (prefix-class (qualified-jclass-name type)) )
      ((tvec? type)
       (vname (tvec-item-type type)) )
      ((jarray? type)
       (vname (jarray-item-type type)) )
      (else
       (let ( (id (type-id type)) )
	  (let ( (slot (assq id **basic-type**)) )
	     (if slot
		 (cdr slot)
		 (prefix-class (qualified-type-name type)) ))))))
