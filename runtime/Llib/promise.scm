;*=====================================================================*/
;*    serrano/bigloo/5.0a/runtime/Llib/promise.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  8 05:19:50 2004                          */
;*    Last change :  Fri Feb  6 17:13:28 2026 (serrano)                */
;*    Copyright   :  2004-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript like promise for Bigloo.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __promise
   
   (import  __error
	    __object)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    __structure
	    __date
	    __os
	    __bit
	    __thread

	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
	    __r4_characters_6_6
	    __r4_equivalence_6_2 
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_output_6_10_3

	    __r5_control_features_6_4
	    
	    __foreign
	    __evenv)

   (export  (class promise
	       (state::symbol (default 'pending))
	       (val::obj (default #unspecified))
	       (thens::pair-nil (default '()))
	       (catches::pair-nil (default '()))
	       (name (default #f))
	       (resolver (default #f))
	       (rejecter (default #f)))
	    (make-promise::promise ::procedure)
	    (then ::promise ::procedure #!optional fail)
	    (then* ::promise ::procedure . rest)
	    (run-promises)))

;*---------------------------------------------------------------------*/
;*    *promise-mutex* ...                                              */
;*---------------------------------------------------------------------*/
(define *promise-mutex* (make-mutex "promises"))
(define *promise-condv* (make-condition-variable "promises"))
(define *promise-count* 0)

;*---------------------------------------------------------------------*/
;*    make-promise ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-promise executor::procedure)
   (let ((o (instantiate::promise)))
      (with-access::promise o (state resolver rejecter thens catches name)
	 (set! state 'pending)
	 (set! thens '())
	 (set! catches '())
	 (multiple-value-bind (resolve reject)
	    (create-resolving-functions o #t)
	    (set! resolver resolve)
	    (set! rejecter reject)
	    (with-handler
	       (lambda (e)
		  (reject e)
		  o)
	       (begin
		  (executor resolve reject)
		  o))))))

;*---------------------------------------------------------------------*/
;*    promise-inc! ...                                                 */
;*---------------------------------------------------------------------*/
(define (promise-inc!)
   (synchronize *promise-mutex*
      (tprint "promise-inc! " *promise-count*)
      (set! *promise-count* (+fx *promise-count* 1))))

;*---------------------------------------------------------------------*/
;*    promise-dec! ...                                                 */
;*---------------------------------------------------------------------*/
(define (promise-dec!)
   (tprint "promise decl mutex=" *promise-mutex*)
   (synchronize *promise-mutex*
      (tprint "promise-dec! " *promise-count*)
      (set! *promise-count* (-fx *promise-count* 1))
      (condition-variable-broadcast! *promise-condv*)))

;*---------------------------------------------------------------------*/
;*    then ...                                                         */
;*---------------------------------------------------------------------*/
(define (then o::promise proc::procedure #!optional fail)
   (let ((no (instantiate::promise)))
      (promise-then-catch o proc fail no)))

;*---------------------------------------------------------------------*/
;*    then* ...                                                        */
;*---------------------------------------------------------------------*/
(define (then* o::promise proc::procedure . rest)
   (cond
      ((null? rest)
       (then o proc))
      ((null? (cdr rest))
       (then o proc (car rest)))
      (else
       (let loop ((rest rest)
		  (p (then o proc)))
	  (if (null? (cddr rest))
	      (then p (car rest) (cadr rest))
	      (loop (cdr rest) (then p (car rest))))))))
   
;*---------------------------------------------------------------------*/
;*    promise-then-catch ...                                           */
;*---------------------------------------------------------------------*/
(define (promise-then-catch::promise o::promise proc fail no::promise)
   (with-access::promise o (thens catches state val)
      (let ((fullfill (cons no (if (procedure? proc) proc 'identity)))
	    (reject (cons no (if (procedure? fail) fail 'thrower))))
	 (tprint "promise-then-catch state=" state)
	 (case state
	    ((pending)
	     (set! thens (cons fullfill thens))
	     (set! catches (cons reject catches)))
	    ((fullfilled)
	     (push-action!
		(lambda ()
		   (tprint "IN ACT.1")
		   (promise-reaction-job fullfill val))))
	    ((rejected)
	     (push-action!
		(lambda ()
		   (tprint "IN ACT.2")
		   (promise-reaction-job reject val)))))
	 no)))

;*---------------------------------------------------------------------*/
;*    create-resolving-functions ...                                   */
;*---------------------------------------------------------------------*/
(define (create-resolving-functions o::promise notify::bool)
   (tprint "create-resolving-functions notify=" notify)
   (when notify (promise-inc!))
   (let* ((resolved #f)
	  (resolve (lambda (resolution)
		      (tprint "IN resolve function resolved=" resolved " notify=" notify)
		      (unless resolved
			 (when notify (promise-dec!))
			 (set! resolved #t)
			 (promise-resolve o resolution))))
	  (reject (lambda (reason)
		     (unless resolved
			(when notify (promise-dec!))
			(set! resolved #t)
			(promise-reject o reason)))))
      (values resolve reject)))

;*---------------------------------------------------------------------*/
;*    promise-reject ...                                               */
;*---------------------------------------------------------------------*/
(define (promise-reject o::promise reason)
   (with-access::promise o (state)
      (when (eq? state 'pending)
	 (reject o reason))))

;*---------------------------------------------------------------------*/
;*    promise-resolve ...                                              */
;*---------------------------------------------------------------------*/
(define (promise-resolve o::promise resolution)
   
   (define (promise-resolve-thenable o::promise thenable then)
      (multiple-value-bind (resolve reject)
	 (create-resolving-functions o #f)
	 (with-handler
	    (lambda (e)
	       (reject e))
	    (then resolve reject))))
   
   (with-access::promise o (%this worker)
      (cond
	 ((eq? o resolution)
	  (reject o
	     (instantiate::&error
		(proc "promise")
		(msg "self resolution error")
		(obj o))))
	 (else
	  (fullfill o resolution)))))

;*---------------------------------------------------------------------*/
;*    promise-resolve-value ...                                        */
;*---------------------------------------------------------------------*/
(define (promise-resolve-value o::promise val)
   (if (isa? val promise)
       val
       (let ((promise (instantiate::promise (name "resolve"))))
	  (promise-resolve promise val)
	  promise)))

;*---------------------------------------------------------------------*/
;*    reject ...                                                       */
;*---------------------------------------------------------------------*/
(define (reject o::promise reason)
   (with-access::promise o (state val worker thens catches name)
      (push-action! 
	 (lambda ()
	    (let ((reactions catches))
	       (set! val reason) 
	       (set! thens '())
	       (set! catches '())
	       (set! state 'rejected)
	       (if (null? reactions)
		   (begin
		      (when (isa? reason &exception)
			 (exception-notify reason))
		      reason)
		   (begin
		      (trigger-reactions reactions reason))))))))

;*---------------------------------------------------------------------*/
;*    fullfill ...                                                     */
;*---------------------------------------------------------------------*/
(define (fullfill o::promise result)
   (with-access::promise o (state val worker thens catches)
      (push-action! 
	 (lambda ()
	    (let ((reactions thens))
	       (set! val result)
	       (set! thens '())
	       (set! catches '())
	       (set! state 'fullfilled)
	       (trigger-reactions reactions result))))))
   
;*---------------------------------------------------------------------*/
;*    trigger-reactions ...                                            */
;*---------------------------------------------------------------------*/
(define (trigger-reactions reactions::pair-nil arg)
   (for-each (lambda (reaction)
		(promise-reaction-job reaction arg))
      (reverse! reactions))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    unresolved ...                                                   */
;*---------------------------------------------------------------------*/
(define unresolved (cons #f #t))

;*---------------------------------------------------------------------*/
;*    promise-reaction-job ...                                         */
;*---------------------------------------------------------------------*/
(define (promise-reaction-job reaction arg)
   (let ((promise (car reaction))
	 (handler (cdr reaction)))
      (with-access::promise promise (%this resolver)
	 (tprint "reaction-job " handler " " arg)
	 (cond
	    ((eq? handler 'identity)
	     (if (procedure? resolver)
		 (resolver arg)
		 arg))
	    ((eq? handler 'thrower)
	     (promise-reject promise arg))
	    (else
	     (with-handler
		(lambda (e)
		   (promise-reject promise e))
		(let ((hresult (handler arg)))
		   (unless (eq? hresult unresolved)
		      (promise-resolve promise hresult)))))))))

;*---------------------------------------------------------------------*/
;*    *actions* ...                                                    */
;*---------------------------------------------------------------------*/
(define *actions* (cons 'promises '()))
(define *last-actions* *actions*)

;*---------------------------------------------------------------------*/
;*    push-action ...                                                  */
;*---------------------------------------------------------------------*/
(define (push-action! action::procedure)
   (let ((last (cons action '())))
      (set-cdr! *last-actions* last)
      (set! *last-actions* last)))

;*---------------------------------------------------------------------*/
;*    run-promises ...                                                 */
;*---------------------------------------------------------------------*/
(define (run-promises)
   
   (define (flush-actions)
      (synchronize *promise-mutex*
	 (let loop ()
	    (let ((actions (cdr *actions*)))
	       (cond
		  ((pair? actions)
		   (set-cdr! *actions* '())
		   (set! *last-actions* *actions*)
		   actions)
		  ((>fx *promise-count* 0)
		   (condition-variable-wait! *promise-condv* *promise-mutex*)
		   (loop))
		  (else
		   '()))))))
   
   (let loop ()
      (let ((actions (flush-actions)))
	 (tprint "run-actions actions=" actions " " *promise-count*)
	 (when (pair? actions)
	    (for-each (lambda (a) (a)) actions)
	    (loop)))))
