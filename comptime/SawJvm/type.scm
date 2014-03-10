(module saw_jvm_type
   (import type_type
	   ast_var
	   ast_node
	   backend_backend
	   backend_bvm
	   backend_jvm_class
	   saw_defs
	   saw_jvm_names)
   (export (typeSize::int type::type)
	   (size-dest::int ins) ))

;;
;; Size
;;
(define **long-types** '(elong llong uelong ullong double int64 uint64))

(define (typeSize::int type::type)
   (let ( (id (type-id type)) )
      (cond ((memq id **long-types**) 2)
	    ((eq? id 'void) 0)
	    (else 1) )))

(define (size-dest::int ins)
   (if (rtl_reg? ins)
       (typeSize (rtl_reg-type ins))
       (with-access::rtl_ins ins (dest fun args)
	  (if dest
	      (typeSize (rtl_reg-type dest))
	      (size-dest-fun fun ins) ))))


;;
;;
(define-generic (size-dest-fun::int fun::rtl_fun ins::rtl_ins);
   ;; default for function which have an associated type
   (typeSize (type-dest-fun fun)) )

(define-method (size-dest-fun::int fun::rtl_last ins) 0)
(define-method (size-dest-fun::int fun::rtl_notseq ins) 0)
(define-method (size-dest-fun::int fun::rtl_effect ins) 0)

;; Pure functions default return pointers
(define-method (size-dest-fun::int fun::rtl_pure ins) 1)

(define-method (size-dest-fun::int fun::rtl_mov ins)
   (size-dest (car (rtl_ins-args ins))) )

(define-method (size-dest-fun::int fun::rtl_loadi ins);
   (let ( (constant (rtl_loadi-constant fun)) )
      (let ( (value (atom-value constant)) )
	 (if (number? value)
	     (typeSize (node-type constant))
	     1 ))))

(define-method (size-dest-fun::int fun::rtl_loadg ins);
   (typeSize (global-type (rtl_loadg-var fun))) )

(define-method (size-dest-fun::int fun::rtl_getfield ins);
   (typeSize (rtl_getfield-type fun)) )

(define-method (size-dest-fun::int fun::rtl_vref ins);
   (typeSize (rtl_vref-type fun)) )

;; Others
(define-method (size-dest-fun::type fun::rtl_new ins) 1)
(define-method (size-dest-fun::type fun::rtl_apply ins) 1)
(define-method (size-dest-fun::type fun::rtl_lightfuncall ins) 1)
(define-method (size-dest-fun::type fun::rtl_funcall ins) 1)
(define-method (size-dest-fun::type fun::rtl_cast ins) 1)

;;
;;
(define-generic (type-dest-fun::type fun::rtl_fun)
   ;; CARE nothing is tested in recette
   (error 'size-dest-fun "no method for" (class-name (object-class fun))) )

(define-method (type-dest-fun::type fun::rtl_call)
   (global-type (rtl_call-var fun)) )



;;;     (class rtl_protect::rtl_fun)
