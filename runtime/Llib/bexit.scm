;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/bexit.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 31 15:00:41 1995                          */
;*    Last change :  Sun Aug 25 09:08:06 2019 (serrano)                */
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
	    (macro pop-exit!::obj () "POP_EXIT")
	    (macro call/cc-jump-exit::obj (::exit ::obj) "CALLCC_JUMP_EXIT")
	    (macro $exitd->exit::exit (::obj) "EXITD_TO_EXIT")
	    (macro exitd-user?::bool (::obj) "EXITD_USERP")
	    (macro exitd-call/cc?::bool (::obj) "EXITD_CALLCCP")
	    (macro exitd-stamp::bint (::obj) "EXITD_STAMP")
	    (macro $get-exitd-top::obj () "BGL_EXITD_TOP_AS_OBJ")
	    (macro $exitd-bottom?::bool (::obj) "BGL_EXITD_BOTTOMP")
	    (macro $set-exitd-top!::obj (::obj) "BGL_EXITD_TOP_SET")
	    (macro $get-exitd-val::obj () "BGL_EXITD_VAL")

	    (macro $exitd-protect0::obj (::exit) "BGL_EXITD_PROTECT0")
	    (macro $exitd-protect0-set!::void (::exit ::obj) "BGL_EXITD_PROTECT0_SET")
	    (macro $exitd-protect1::obj (::exit) "BGL_EXITD_PROTECT1")
	    (macro $exitd-protect1-set!::void (::exit ::obj) "BGL_EXITD_PROTECT1_SET")
	    (macro $exitd-protectn::pair-nil (::exit) "BGL_EXITD_PROTECTN")
	    (macro $exitd-protectn-set!::void (::exit ::pair-nil) "BGL_EXITD_PROTECTN_SET")
	    (macro $exitd-push-protect!::void (::exit ::obj) "BGL_EXITD_PUSH_PROTECT")
	    (macro $exitd-pop-protect!::void (::exit) "BGL_EXITD_POP_PROTECT")

	    (export $failsafe-mutex-profile "bgl_failsafe_mutex_profile")
	    (export $exitd-mutex-profile "bgl_exitd_mutex_profile")
	    (export unwind-stack-until! "unwind_stack_until")
	    (export unwind-stack-value? "unwind_stack_value_p")

	    (export default-uncaught-exception-handler "bgl_uncaught_exception_handler"))

   (cond-expand (bigloo-c
		 (export
		    ($exitd-mutex-profile)
		    ($failsafe-mutex-profile))))
   
   (java    (class foreign
	       (method static push-exit!::obj (::exit ::long)
		  "PUSH_EXIT")
	       (method static pop-exit!::obj ()
		  "POP_EXIT")
	       (method static call/cc-jump-exit::obj (::exit ::obj)
		  "CALLCC_JUMP_EXIT")
	       (method static $exitd->exit::exit (::obj)
		  "EXITD_TO_EXIT")
	       (method static exitd-user?::bool (::obj)
		  "EXITD_USERP")
	       (method static exitd-call/cc?::bool (::obj)
		  "EXITD_CALLCCP")
	       (method static exitd-stamp::bint (::obj)
		  "EXITD_STAMP")
	       (method static $get-exitd-top::obj ()
		  "BGL_EXITD_TOP")
	       (method static $exitd-bottom?::bool (::obj)
		  "BGL_EXITD_BOTTOMP")
	       (method static $set-exitd-top!::obj (::obj)
		  "BGL_EXITD_TOP_SET")
	       (method static $get-exitd-val::obj ()
		  "BGL_EXITD_VAL")
	       
	       (method static $exitd-protect0::obj (::exit)
		  "BGL_EXITD_PROTECT0")
	       (method static $exitd-protect0-set!::void (::exit ::obj)
		  "BGL_EXITD_PROTECT0_SET")
	       (method static $exitd-protect1::obj (::exit)
		  "BGL_EXITD_PROTECT1")
	       (method static $exitd-protect1-set!::void (::exit ::obj)
		  "BGL_EXITD_PROTECT1_SET")
	       (method static $exitd-protectn::pair-nil (::exit)
		  "BGL_EXITD_PROTECTN")
	       (method static $exitd-protectn-set!::void (::exit ::pair-nil)
		  "BGL_EXITD_PROTECTN_SET")))
      
   (export  (val-from-exit? ::obj)
	    (unwind-stack-value?::bool ::obj)
	    (unwind-until! exitd ::obj)
	    (unwind-stack-until! exitd ::obj ::obj ::obj)
	    (default-uncaught-exception-handler ::obj)
	    (exitd-push-protect! ::obj ::obj)
	    (exitd-pop-protect! ::obj))

   (cond-expand (bigloo-c
		 (pragma
		    ($failsafe-mutex-profile fail-safe)
		    ($exitd-mutex-profile fail-safe)))))

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
   (if (pair? exitd)
       (unwind-stack-until! (car exitd) #f val (cdr exitd))
       (unwind-stack-until! exitd #f val #f)))

;*---------------------------------------------------------------------*/
;*    unwind-stack-until! ...                                          */
;*    -------------------------------------------------------------    */
;*    This function unwinds a stack until an exit is found or the      */
;*    stack bottom is reached, in which case, the proc arg is called.  */
;*    This function is used by unwind-until! (introduced by any        */
;*    unwind-protect) and by call/cc.                                  */
;*---------------------------------------------------------------------*/
(define (unwind-stack-until! exitd estamp val proc-bottom)
   (let loop ()
      (let ((exitd-top ($get-exitd-top)))
	 (if ($exitd-bottom? exitd-top)
	     (begin
		(exitd-exec-and-pop-protects! exitd-top)
		(if (procedure? proc-bottom)
		    (proc-bottom val)
		    (let ((hdl ($get-uncaught-exception-handler)))
		       ((if (procedure? hdl)
			    hdl
			    default-uncaught-exception-handler)
			val))))
	     (begin
		;; execute the unwind protects pushed above the exitd block
		(exitd-exec-and-pop-protects! exitd-top)
		;; then, remove the exitd block from the stack
		(pop-exit!)
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
		   ((not (exitd-user? exitd-top))
		    (let ((p ($get-exitd-val)))
		       (set-car! (car p) exitd)
		       (set-cdr! (car p) proc-bottom)
		       (set-cdr! p val)
		       (jump-exit ($exitd->exit exitd-top) p))
		    #unspecified)
		   (else
		    (loop))))))))

;*---------------------------------------------------------------------*/
;*    exitd-exec-protect ...                                           */
;*---------------------------------------------------------------------*/
(define (exitd-exec-protect p)
   (cond
      ((mutex? p) (mutex-unlock! p))
      ((procedure? p) (p))
      ((integer? p) (evaluate2-restore-bp! p))
      ((vector? p) (evaluate2-restore-state! p))))
      
;*---------------------------------------------------------------------*/
;*    exitd-exec-and-pop-protects! ...                                 */
;*    -------------------------------------------------------------    */
;*    Remove and then execute all the exec and protect on the          */
;*    exitd block.                                                     */
;*---------------------------------------------------------------------*/
(define (exitd-exec-and-pop-protects! exitd)
   (let loop ((l ($exitd-protectn exitd)))
      (when (pair? l)
	 (let ((p (car l)))
	    ($exitd-protectn-set! exitd (cdr l))
	    (exitd-exec-protect p)
	    (loop (cdr l)))))
   (let ((p ($exitd-protect1 exitd)))
      (when p
	 ($exitd-protect1-set! exitd #f)
	 (exitd-exec-protect p)))
   (let ((p ($exitd-protect0 exitd)))
      ;; don't need to test protect0, it is always true
      ($exitd-protect0-set! exitd #f)
      (exitd-exec-protect p)))

;*---------------------------------------------------------------------*/
;*    exitd-push-protect! ...                                          */
;*    -------------------------------------------------------------    */
;*    The C backend used an inlined version (BGL_EXITD_PUSH_PROTECT)   */
;*    to get synchronized as fast as possible.                         */
;*---------------------------------------------------------------------*/
(define (exitd-push-protect! exitd m)
   (cond
      ((not ($exitd-protect0 exitd))
       ($exitd-protect0-set! exitd m))
      ((not ($exitd-protect1 exitd))
       ($exitd-protect1-set! exitd m))
      (else
       ($exitd-protectn-set! exitd (cons m ($exitd-protectn exitd))))))

;*---------------------------------------------------------------------*/
;*    exitd-pop-protect! ...                                           */
;*---------------------------------------------------------------------*/
(define (exitd-pop-protect! exitd)
   (cond
      ((not ($exitd-protect1 exitd))
       ($exitd-protect0-set! exitd #f))
      ((null? ($exitd-protectn exitd))
       ($exitd-protect1-set! exitd #f))
      (else
       ($exitd-protectn-set! exitd (cdr ($exitd-protectn exitd))))))

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
