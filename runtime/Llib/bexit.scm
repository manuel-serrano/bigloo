;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/bexit.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 31 15:00:41 1995                          */
;*    Last change :  Thu Apr 22 09:55:05 2010 (serrano)                */
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
	    
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_strings_6_7
	    __r4_symbols_6_4
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
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
	    
	    (export unwind-stack-until! "unwind_stack_until")
	    (export unwind-stack-value? "unwind_stack_value_p")

	    (export default-uncaught-exception-handler "bgl_uncaught_exception_handler"))

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
		       "BGL_EXITD_VAL")))
   
   (export  (val-from-exit? ::obj)
	    (unwind-stack-value?::bool ::obj)
	    (unwind-until! exitd ::obj)
	    (unwind-stack-until! exitd ::obj ::obj ::obj)
	    (default-uncaught-exception-handler ::obj)))

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
;*    This function is used by unwind-protect and bind-exit. It just   */
;*    unwinds a stack which must not be used by call/cc. The former    */
;*    should directly use unwind-stack-until!                          */
;*---------------------------------------------------------------------*/
(define (unwind-until! exitd val)
   (if (pair? exitd)
       (unwind-stack-until! (car exitd) #f val (cdr exitd))
       (unwind-stack-until! exitd #f val #f)))

;*---------------------------------------------------------------------*/
;*    unwind-stack-until! ...                                          */
;*    -------------------------------------------------------------    */
;*    This function unwind a stack until an exit is found or the       */
;*    stack bottom is reached. In such a case, the proc arg is called. */
;*    This function is used by unwind-until! (introduced by any        */
;*    unwind-protect) and by call/cc.                                  */
;*---------------------------------------------------------------------*/
(define (unwind-stack-until! exitd estamp val proc)
   (let loop ()
      (let ((exitd-top ($get-exitd-top)))
	 (if ($exitd-bottom? exitd-top)
	     (if (procedure? proc)
		 (proc val)
		 (let ((hdl ($get-uncaught-exception-handler)))
		    ((if (procedure? hdl)
			 hdl
			 default-uncaught-exception-handler)
		     val)))
	     (begin
		(pop-exit!)
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
		       (set-cdr! (car p) proc)
		       (set-cdr! p val)
		       (jump-exit ($exitd->exit exitd-top) p))
		    #unspecified)
		   (else
		    (loop))))))))

;*---------------------------------------------------------------------*/
;*    default-uncaught-exception-handler ...                           */
;*---------------------------------------------------------------------*/
(define (default-uncaught-exception-handler val)
   (error 'unwind-until! "exit out of dynamic scope" val))
