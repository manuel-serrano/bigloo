;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/backend.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  4 14:08:50 2003                          */
;*    Last change :  Sat Jan 30 03:45:00 2016 (serrano)                */
;*    Copyright   :  2003-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The declaration of the backend structure.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_backend
   
   (import engine_param)

   (include "BackEnd/backend.sch")
   
   (export (abstract-class backend
	      (backend-initialize!)
	      (language::symbol (default 'none))
	      (srfi0::symbol (default 'none))
	      (name::bstring (default "dummy"))
	      (extern-variables (default '()))
	      (extern-functions (default '()))
	      (extern-types (default '()))
	      (variables (default '()))
	      (functions (default '()))
	      (types (default '()))
	      (typed::bool (default #f))
	      (heap-suffix::bstring (default "heap"))
	      (heap-compatible::symbol (default 'native))
	      (callcc::bool (default #t))
	      (qualified-types::bool (default #f))
	      (effect+::bool (default #f))
	      (remove-empty-let::bool (default #f))
	      (foreign-closure::bool (default #t))
	      (typed-eq::bool (default #f))
	      (trace-support::bool (default #t))
	      (foreign-clause-support::pair-nil (default '(foreign extern)))
	      (debug-support::pair-nil (default '(c bdb module)))
	      (pragma-support::bool (default #t))
	      (tvector-descr-support::bool (default #t))
	      (require-tailc::bool (default #f))
	      (registers::pair-nil (default '()))
	      (pregisters::pair-nil (default '()))
	      (bound-check::bool (default #t))
	      (type-check::bool (default #t))
	      (typed-funcall::bool (default #t))))
   
   (export (generic backend-initialize! ::backend)
	   (generic backend-compile ::backend)
	   (generic backend-compile-functions ::backend)
	   (generic backend-link ::backend result)
	   (generic backend-subtype? ::backend ::obj ::obj)
	   (generic backend-cnst-table-name ::backend ::int)
	   (generic backend-link-objects ::backend ::pair-nil)
	   (generic backend-instr-reset-registers::pair-nil ::backend ::obj)
	   (generic backend-check-inlines ::backend)
	   (generic backend-gc-init ::backend))

   (export (set-backend! ::symbol)
	   (the-backend)
	   (register-backend! ::symbol ::procedure)))

;*---------------------------------------------------------------------*/
;*    *the-backend* ...                                                */
;*---------------------------------------------------------------------*/
(define *the-backend* #unspecified)

;*---------------------------------------------------------------------*/
;*    set-backend! ...                                                 */
;*---------------------------------------------------------------------*/
(define (set-backend! language)
   (let ((c (assq language *backends*)))
      (if (not (pair? c))
	  (error "backend" "Unimplemented target language" language)
	  (set! *the-backend* ((cdr c))))))

;*---------------------------------------------------------------------*/
;*    the-backend ...                                                  */
;*---------------------------------------------------------------------*/
(define (the-backend)
   *the-backend*)

;*---------------------------------------------------------------------*/
;*    *backends* ...                                                   */
;*---------------------------------------------------------------------*/
(define *backends* '())

;*---------------------------------------------------------------------*/
;*    register-backend! ...                                            */
;*---------------------------------------------------------------------*/
(define (register-backend! id builder)
   (set! *backends* (cons (cons id builder) *backends*)))

;*---------------------------------------------------------------------*/
;*    backend-initialize! ::backend ...                                */
;*---------------------------------------------------------------------*/
(define-generic (backend-initialize! b::backend)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    backend-compile ::backend ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (backend-compile b::backend))

;*---------------------------------------------------------------------*/
;*    backend-compile-functions ::backend ...                          */
;*---------------------------------------------------------------------*/
(define-generic (backend-compile-functions b::backend))

;*---------------------------------------------------------------------*/
;*    backend-link ::backend ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (backend-link b::backend result))

;*---------------------------------------------------------------------*/
;*    backend-subtype? ::backend ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (backend-subtype? b::backend t1::obj t2::obj))

;*---------------------------------------------------------------------*/
;*    backend-cnst-table-name ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (backend-cnst-table-name b::backend o))

;*---------------------------------------------------------------------*/
;*    backend-link-objects ::backend ...                               */
;*---------------------------------------------------------------------*/
(define-generic (backend-link-objects b::backend l))

;*---------------------------------------------------------------------*/
;*    backend-instr-reset-registers ::backend ...                      */
;*---------------------------------------------------------------------*/
(define-generic (backend-instr-reset-registers b::backend i)
   (with-access::backend b (registers)
      (values '() registers registers registers)))

;*---------------------------------------------------------------------*/
;*    backend-check-inlines ::backend ...                              */
;*---------------------------------------------------------------------*/
(define-generic (backend-check-inlines b::backend)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    backend-gc-init ::backend ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (backend-gc-init b::backend)
   #unspecified)
