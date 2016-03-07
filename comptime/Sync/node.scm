;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Sync/node.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 18 08:38:02 2012                          */
;*    Last change :  Thu Mar  3 14:16:58 2016 (serrano)                */
;*    Copyright   :  2012-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    SYNC2NODE, this expands a SYNC node into a plain node using      */
;*    explicitly lock/unlock and push/pop operations. Used by the      */
;*    C backend. The JVM backend should, someday, compile directly     */
;*    a synchronize block and should not use this expansion.           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sync_node
   
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   
   (import  tools_error
	    tools_shape
	    engine_param
	    type_type
	    type_tools
	    type_cache
	    type_typeof
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_local
	    ast_sexp
	    ast_app
	    ast_dump
	    backend_backend
	    inline_app
	    effect_effect
	    backend_cplib
	    sync_failsafe)
   
   (export (sync->sequence::node ::sync)))

;*---------------------------------------------------------------------*/
;*    lock cache                                                       */
;*---------------------------------------------------------------------*/
(define mlock #f)
(define mlockprelock #f)
(define mulock #f)
(define mpush #f)
(define mpop #f)
(define getexitdtop #f)
(define exitd-mutex-profile #f)
(define failsafe-mutex-profile #f)

;*---------------------------------------------------------------------*/
;*    init-sync! ...                                                   */
;*---------------------------------------------------------------------*/
(define (init-sync! loc)
   (unless mlock
      (set! getexitdtop (sexp->node '$get-exitd-top '() loc 'app))
      (set! mlock (sexp->node '$mutex-lock '() loc 'app))
      (set! mlockprelock (sexp->node '$mutex-lock-prelock '() loc 'app))
      (set! mulock (sexp->node '$mutex-unlock '() loc 'app))
      (case (backend-language (the-backend))
	 ((c c-saw)
	  (when *sync-profiling*
	     (set! exitd-mutex-profile
		(sexp->node '$exitd-mutex-profile '() loc 'app))
	     (set! failsafe-mutex-profile
		(sexp->node '$failsafe-mutex-profile '() loc 'app))
	     (set-variable-name! (var-variable exitd-mutex-profile))
	     (set-variable-name! (var-variable failsafe-mutex-profile)))
	  (set! mpush (sexp->node '$exitd-push-protect! '() loc 'app))
	  (set! mpop (sexp->node '$exitd-pop-protect! '() loc 'app)))
	 (else
	  (set! mpush (sexp->node '(@ exitd-push-protect! __bexit) '() loc 'app))
	  (set! mpop (sexp->node '(@ exitd-pop-protect! __bexit) '() loc 'app))))
      (set-variable-name! (var-variable getexitdtop))
      (set-variable-name! (var-variable mlock))
      (set-variable-name! (var-variable mlockprelock))
      (set-variable-name! (var-variable mulock))
      (set-variable-name! (var-variable mpush))
      (set-variable-name! (var-variable mpop))))

;*---------------------------------------------------------------------*/
;*    sync->sequence ...                                               */
;*    -------------------------------------------------------------    */
;*    This function performs the following expansions:                 */
;*    1- if prelock is NIL:                                            */
;*        (sync m body)                                                */
;*      =>                                                             */
;*        (begin                                                       */
;*           ($mutex-lock m)                                           */
;*           ((@ exitd-push-protect! __bexit) m)                       */
;*           (let ((tmp body))                                         */
;*              ((@ exitd-pop-protect! __bexit))                       */
;*              ($mutex-unlock m)))                                    */
;*    2- if prelock is not NIL:                                        */
;*        (sync m prelock body)                                        */
;*      =>                                                             */
;*        (begin                                                       */
;*           ($mutex-lock-prelock m prelock)                           */
;*           ((@ exitd-push-protect! __bexit) m)                       */
;*           (let ((tmp body))                                         */
;*              ((@ exitd-pop-protect! __bexit))                       */
;*              ($mutex-unlock m)))                                    */
;*---------------------------------------------------------------------*/
(define (sync->sequence node::sync)
   
   (define (app expr loc)
      (application->node expr '() loc 'value))

   (define (profsync node loc)
      (if (when exitd-mutex-profile)
	  (list (app `(,exitd-mutex-profile) loc) node)
	  (list node)))

   (define (proffailsafe node loc)
      (if (when failsafe-mutex-profile)
	  (list (app `(,failsafe-mutex-profile) loc) node)
	  (list node)))

   (define (failsafe-sync->sequence node)
      ;; (tprint "FAILSAFE synchronize " *src-files*)
      ;; no exception raised, avoid pushing/poping mutexes
      (with-access::sync node (loc body mutex type prelock)
	 (let* ((tmp (make-local-svar (gensym 'tmp) type))
		(lock (if (atom? prelock)
			  (app `(,mlock ,mutex) loc)
			  (app `(,mlockprelock ,mutex ,prelock) loc)))
		(unlock (app `(,mulock ,mutex) loc))
		(vref (instantiate::var
			 (loc loc)
			 (type type)
			 (variable tmp)))
		(lbody (instantiate::let-var
			  (loc loc)
			  (type type)
			  (bindings (list (cons tmp body)))
			  (body (instantiate::sequence
				   (loc loc)
				   (type type)
				   (nodes (list unlock vref)))))))
	    (instantiate::sequence
	       (loc loc)
	       (type type)
	       (nodes (cons lock (proffailsafe lbody loc)))))))
   
   (define (effect-sync->sequence node)
      ;; (tprint "FULL synchronize " *src-files*)
      ;; exceptions potentially raised, slow path compilation
      (with-access::sync node (loc body mutex type prelock)
	 (let* ((tmp (make-local-svar (gensym 'tmp) type))
		(top (make-local-svar (gensym 'top) *obj*))
		(topref (instantiate::var
			   (loc loc)
			   (type *obj*)
			   (variable top)))
		(gettop (app `(,getexitdtop) loc))
		(lock (if (atom? prelock)
			  (app `(,mlock ,mutex) loc)
			  (app `(,mlockprelock ,mutex ,prelock) loc)))
		(push (app `(,mpush ,topref ,mutex) loc))
		(pop (app `(,mpop ,topref) loc))
		(unlock (app `(,mulock ,mutex) loc))
		(vref (instantiate::var
			 (loc loc)
			 (type type)
			 (variable tmp)))
		(lbody (instantiate::let-var
			  (loc loc)
			  (type type)
			  (bindings (list (cons tmp body)))
			  (body (instantiate::sequence
				   (loc loc)
				   (type type)
				   (nodes (list pop unlock vref)))))))
	    (instantiate::let-var
	       (loc loc)
	       (type type)
	       (bindings (list (cons top gettop)))
	       (body (instantiate::sequence
			(loc loc)
			(type type)
			(nodes (cons* lock push (profsync lbody loc)))))))))
   
   (with-access::sync node (loc)
      (init-sync! loc))
   
   (if (failsafe-sync? node)
       (failsafe-sync->sequence node)
       (effect-sync->sequence node)))
