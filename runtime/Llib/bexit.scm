;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/bexit.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 31 15:00:41 1995                          */
;*    Last change :  Thu Sep 16 14:14:04 2021 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `bind-exit' manipulation.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bexit
   
   ;; disable debugging traces when compiling this module otherwise
   ;; the Bigloo error handling is all wrong
   (option  (set! *compiler-debug* 0))
   
   (import  __error
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __bignum
	    __evaluate
	    __bit
	    
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_strings_6_7
	    __r4_symbols_6_4
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __evenv)
   
   (extern  (macro push-exit!::obj (::exit ::long) "PUSH_EXIT")
	    (macro $env-push-exit!::obj (::dynamic-env ::exit ::long) "PUSH_ENV_EXIT")
	    (macro pop-exit!::obj () "POP_EXIT")
	    (macro $env-pop-exit!::obj (::dynamic-env) "POP_ENV_EXIT")
	    (macro call/cc-jump-exit::obj (::exit ::obj) "CALLCC_JUMP_EXIT")
	    (macro $exitd->exit::exit (::obj) "EXITD_TO_EXIT")
	    (macro exitd-call/cc?::bool (::obj) "EXITD_CALLCCP")
	    (macro exitd-stamp::bint (::obj) "EXITD_STAMP")
	    (macro $get-exitd-top::obj () "BGL_EXITD_TOP_AS_OBJ")
	    (macro $env-get-exitd-top::obj (::dynamic-env) "BGL_ENV_EXITD_TOP_AS_OBJ")
	    (macro $exitd-bottom?::bool (::obj) "BGL_EXITD_BOTTOMP")
	    (macro $set-exitd-top!::obj (::obj) "BGL_EXITD_TOP_SET")
	    (macro $get-exitd-val::obj () "BGL_EXITD_VAL")
	    (macro $get-exitd-val::obj (::dynamic-env) "BGL_EXITD_VAL")
	    (macro $set-exitd-val!::obj (::obj) "BGL_EXITD_VAL_SET")
	    (macro $env-get-exitd-val::obj (::dynamic-env) "BGL_ENV_EXITD_VAL")
	    (macro $env-set-exitd-val!::obj (::dynamic-env ::obj) "BGL_ENV_EXITD_VAL_SET")
	    
	    (macro $exitd-protect::obj (::exit) "BGL_EXITD_PROTECT")
	    (macro $exitd-protect-set!::void (::exit ::obj) "BGL_EXITD_PROTECT_SET")
	    (macro $exitd-push-protect!::void (::exit ::obj) "BGL_EXITD_PUSH_PROTECT")
	    (macro $exitd-pop-protect!::void (::exit) "BGL_EXITD_POP_PROTECT")
	    
	    (export $failsafe-mutex-profile "bgl_failsafe_mutex_profile")
	    (export $exitd-mutex-profile "bgl_exitd_mutex_profile")
	    (export unwind-stack-until! "unwind_stack_until")
	    (export unwind-stack-value? "unwind_stack_value_p")
	    
	    (export default-uncaught-exception-handler "bgl_uncaught_exception_handler"))
   
   (cond-expand (bigloo-c
		 (export
		    (inline env-get-exitd-val::obj ::dynamic-env)
		    (inline env-set-exitd-val!::obj ::dynamic-env ::obj)
		    ($exitd-mutex-profile)
		    ($failsafe-mutex-profile))))
   
   (java    (class foreign
	       (method static push-exit!::obj (::exit ::long)
		  "PUSH_EXIT")
	       (method static $env-push-exit!::obj (::dynamic-env ::exit ::long)
		  "PUSH_ENV_EXIT")
	       (method static pop-exit!::obj ()
		  "POP_EXIT")
	       (method static $env-pop-exit!::obj (::dynamic-env)
		  "POP_ENV_EXIT")
	       (method static call/cc-jump-exit::obj (::exit ::obj)
		  "CALLCC_JUMP_EXIT")
	       (method static $exitd->exit::exit (::obj)
		  "EXITD_TO_EXIT")
	       (method static exitd-call/cc?::bool (::obj)
		  "EXITD_CALLCCP")
	       (method static exitd-stamp::bint (::obj)
		  "EXITD_STAMP")
	       (method static $get-exitd-top::obj ()
		  "BGL_EXITD_TOP")
	       (method static $env-get-exitd-top::obj (::dynamic-env)
		  "BGL_ENV_EXITD_TOP_AS_OBJ")
	       (method static $exitd-bottom?::bool (::obj)
		  "BGL_EXITD_BOTTOMP")
	       (method static $set-exitd-top!::obj (::obj)
		  "BGL_EXITD_TOP_SET")
	       (method static $get-exitd-val::obj ()
		  "BGL_EXITD_VAL")
	       
	       (method static $exitd-protect::pair-nil (::exit)
		  "BGL_EXITD_PROTECT")
	       (method static $exitd-protect-set!::void (::exit ::pair-nil)
		  "BGL_EXITD_PROTECT_SET")
	       (method static $exitd-push-protect!::void (::exit ::obj)
		  "BGL_EXITD_PUSH_PROTECT")))
   
   (export  (val-from-exit? ::obj)
	    (unwind-stack-value?::bool ::obj)
	    (unwind-until! exitd ::obj)
	    (unwind-stack-until! exitd ::obj ::obj ::obj ::obj)
	    (default-uncaught-exception-handler ::obj)
	    (inline exitd-protect-set! ::obj m::obj)
	    (inline exitd-push-protect! ::obj m::obj)
	    (inline exitd-pop-protect! ::obj))
   
   (cond-expand (bigloo-c
		 (pragma
		    ($failsafe-mutex-profile fail-safe)
		    ($exitd-mutex-profile fail-safe))))
   
   (pragma (exitd-protect-set! (args-noescape m))
	   ($exitd-protect-set! (args-noescape 1))
	   (exitd-push-protect! (args-noescape m))
	   ($exitd-push-protect! (args-noescape 1))))

;*---------------------------------------------------------------------*/
;*    val-from-exit? ...                                               */
;*---------------------------------------------------------------------*/
(define (val-from-exit? val)
   (eq? val ($get-exitd-val)))

;*---------------------------------------------------------------------*/
;*    unwind-stack-value? ...                                          */
;*    -------------------------------------------------------------    */
;*    MS has introduced this function just because it helps the        */
;*    bootstrap. val-from-exit? is used inside the compiler with       */
;*    the prototype obj->obj and MS wanted to use it inside callcc.c   */
;*    with the prototype obj->bool.                                    */
;*---------------------------------------------------------------------*/
(define (unwind-stack-value? val)
   (val-from-exit? val))

;*---------------------------------------------------------------------*/
;*    unwind-until! ...                                                */
;*    -------------------------------------------------------------    */
;*    This function is used by unwind-protect and bind-exit. It        */
;*    unwinds a stack which must not be used by call/cc. Call/cc       */
;*    directly invoke unwind-stack-until!                              */
;*---------------------------------------------------------------------*/
(define (unwind-until! exitd val)
   (unwind-stack-until! exitd #f val #f #f))

;*---------------------------------------------------------------------*/
;*    unwind-stack-until! ...                                          */
;*    -------------------------------------------------------------    */
;*    This function unwinds a stack until an exit is found or the      */
;*    stack bottom is reached, in which case, the proc arg is called.  */
;*    This function is used by unwind-until! (introduced by any        */
;*    unwind-protect) and by call/cc.                                  */
;*---------------------------------------------------------------------*/
(define (unwind-stack-until! exitd estamp val proc-bottom tracestk)
   (let loop ()
      (let ((exitd-top ($get-exitd-top)))
	 (if ($exitd-bottom? exitd-top)
	     (begin
		(exitd-exec-and-pop-protects! exitd-top)
		(if (procedure? proc-bottom)
		    (proc-bottom val)
		    (let ((hdl ($get-uncaught-exception-handler)))
		       (if (procedure? hdl)
			   (hdl val)
			   (default-uncaught-exception-handler val)))))
	     (begin
		;; execute the unwind protects pushed above the exitd block
		(exitd-exec-and-pop-protects! exitd-top)
		;; then, remove the exitd block from the stack
		(pop-exit!)
		(if tracestk
		    ($set-trace-stacksp tracestk)
		    ($init-trace-stacksp))
		;; unwind the stack
		(cond  
		   ((and (eq? exitd-top exitd) 
			 (or (not (fixnum? estamp))
			     (=fx (exitd-stamp exitd-top) estamp)))
		    (if (exitd-call/cc? exitd-top)
			;; this exit has been pushed by call/cc
			(call/cc-jump-exit ($exitd->exit exitd-top) val)
			;; this is a regular exit
			(jump-exit ($exitd->exit exitd-top) val))
		    #unspecified)
		   (else
		    (loop))))))))

;*---------------------------------------------------------------------*/
;*    exitd-exec-and-pop-protects! ...                                 */
;*    -------------------------------------------------------------    */
;*    Remove and then execute all the exec and protect on the          */
;*    exitd block.                                                     */
;*---------------------------------------------------------------------*/
(define (exitd-exec-and-pop-protects! exitd)
   (let loop ((l ($exitd-protect exitd)))
      (when (pair? l)
	 (let ((p (car l)))
	    ($exitd-protect-set! exitd (cdr l))
	    (exitd-exec-protect p)
	    (loop (cdr l))))))

;*---------------------------------------------------------------------*/
;*    exitd-exec-protect ...                                           */
;*---------------------------------------------------------------------*/
(define (exitd-exec-protect p)
   (cond
      ((mutex? p) (mutex-unlock! p))
      ((procedure? p) (p))
      ((pair? p) ($set-error-handler! p))
      ((integer? p) (evaluate2-restore-bp! p))
      ((vector? p) (evaluate2-restore-state! p))))
      
;*---------------------------------------------------------------------*/
;*    exitd-protect-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (exitd-protect-set! exitd p)
   ($exitd-protect-set! exitd p))

;*---------------------------------------------------------------------*/
;*    exitd-push-protect! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (exitd-push-protect! exitd m)
   ($exitd-protect-set! exitd (cons m ($exitd-protect exitd))))

;*---------------------------------------------------------------------*/
;*    exitd-pop-protect! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (exitd-pop-protect! exitd)
   (when (pair? ($exitd-protect exitd))
      ($exitd-protect-set! exitd (cdr ($exitd-protect exitd)))))

;*---------------------------------------------------------------------*/
;*    default-uncaught-exception-handler ...                           */
;*---------------------------------------------------------------------*/
(define (default-uncaught-exception-handler val)
   (error "unwind-protect" "exit out of dynamic scope" val))

;*---------------------------------------------------------------------*/
;*    $exitd-mutex-profile                                             */
;*    -------------------------------------------------------------    */
;*    This C function is empty. This definition is a placeholder for   */
;*    LD_PRELOAD dynamic libraries that can override it.               */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo-c
    (define ($exitd-mutex-profile) #unspecified)
    (define ($failsafe-mutex-profile) #unspecified)))

;*---------------------------------------------------------------------*/
;*    env-get-exitd-val ...                                            */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo-c
    (define-inline (env-get-exitd-val env) ($env-get-exitd-val env))
    (define-inline (env-set-exitd-val! env obj) ($env-set-exitd-val! env obj))))
   
