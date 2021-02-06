;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/thread.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  8 05:19:50 2004                          */
;*    Last change :  Sun Aug 25 09:10:10 2019 (serrano)                */
;*    Copyright   :  2004-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Not an implementation of threads (see Fthread for instance).     */
;*    This is simply an implementation of lock and synchronization     */
;*    mechanism.                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __thread
   
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
	    
	    __foreign
	    __evenv)
   
   (extern  (macro $dynamic-env?::bool (::obj)
		   "BGL_DYNAMIC_ENVP")
	    (macro $current-dynamic-env::dynamic-env ()
		   "BGL_CURRENT_DYNAMIC_ENV")

            (macro $mutex?::bool (::obj)
		   "BGL_MUTEXP")
	    (macro $make-mutex::mutex (::obj)
		   "bgl_make_mutex")
	    (macro $make-spinlock::mutex (::obj)
		   "bgl_make_spinlock")
	    ($make-nil-mutex::mutex () "bgl_make_nil_mutex")
	    (macro $mutex-name::obj (::mutex)
		   "BGL_MUTEX_NAME")
	    (macro $mutex-backend::obj (::mutex)
		   "BGL_MUTEX_BACKEND")
	    (macro $mutex-lock::int (::mutex)
		   "BGL_MUTEX_LOCK")
	    (macro $mutex-lock-prelock::int (::mutex ::pair-nil)
		   "BGL_MUTEX_LOCK_PRELOCK")
	    (macro $mutex-timed-lock::int (::mutex ::long)
		   "BGL_MUTEX_TIMED_LOCK")
	    (macro $mutex-unlock::int (::mutex)
		   "BGL_MUTEX_UNLOCK")
	    (macro $mutex-state::obj (::mutex)
		   "BGL_MUTEX_STATE")
	    
	    (macro $condvar?::bool (::obj)
		   "BGL_CONDVARP")
	    (macro $make-condvar::condvar (::obj)
		   "bgl_make_condvar")
	    ($make-nil-condvar::condvar () "bgl_make_nil_condvar")
	    (macro $condvar-name::obj (::condvar)
		   "BGL_CONDVAR_NAME")
	    (macro $condvar-wait!::bool (::condvar ::mutex)
		   "BGL_CONDVAR_WAIT")
	    (macro $condvar-timed-wait!::bool (::condvar ::mutex ::long)
		   "BGL_CONDVAR_TIMED_WAIT")
	    (macro $condvar-broadcast!::bool (::condvar)
		   "BGL_CONDVAR_BROADCAST")
	    (macro $condvar-signal!::bool (::condvar)
		   "BGL_CONDVAR_SIGNAL")

	    (macro $thread-parameters::obj ()
		   "BGL_PARAMETERS")
	    (macro $thread-parameters-set!::obj (::obj)
		   "BGL_PARAMETERS_SET")
	    
	    (macro $thread-backend::obj ()
		   "BGL_THREAD_BACKEND")
	    (macro $thread-backend-set!::void (::obj)
		   "BGL_THREAD_BACKEND_SET"))
	    
   (java    (class foreign
	       (method static $dynamic-env?::bool (::obj)
		       "BGL_DYNAMIC_ENVP")
	       (method static $current-dynamic-env::dynamic-env ()
		       "BGL_CURRENT_DYNAMIC_ENV")
	       
	       (method static $mutex?::bool (::obj)
		       "BGL_MUTEXP")
	       (method static $make-mutex::mutex (::obj)
		       "bgl_make_mutex")
	       (method static $make-spinlock::mutex (::obj)
		       "bgl_make_mutex")
	       (method static $make-nil-mutex::mutex ()
		       "bgl_make_nil_mutex")
	       (method static $mutex-name::obj (::mutex)
		       "BGL_MUTEX_NAME")
	       (method static $mutex-backend::obj (::mutex)
		       "BGL_MUTEX_BACKEND")
	       (method static $mutex-lock::int (::mutex)
		       "bgl_mutex_lock")
	       (method static $mutex-lock-prelock::int (::mutex ::pair-nil)
		       "bgl_mutex_lock_prelock")
	       (method static $mutex-timed-lock::int (::mutex ::long)
		       "bgl_mutex_timed_lock")
	       (method static $mutex-unlock::int (::mutex)
		       "bgl_mutex_unlock")
	       (method static $mutex-state::obj (::mutex)
		       "bgl_mutex_state")
	       
	       (method static $condvar?::bool (::obj)
		       "BGL_CONDVARP")
	       (method static $make-condvar::condvar (::obj)
		       "bgl_make_condvar")
	       (method static $make-nil-condvar::condvar ()
		       "bgl_make_nil_condvar")
	       (method static $condvar-name::obj (::condvar)
		       "BGL_CONDVAR_NAME")
	       (method static $condvar-wait!::bool (::condvar ::mutex)
		       "bgl_condvar_wait")
	       (method static $condvar-timed-wait!::bool (::condvar ::mutex ::int)
		       "bgl_condvar_timed_wait")
	       (method static $condvar-broadcast!::bool (::condvar)
		       "bgl_condvar_broadcast")
	       (method static $condvar-signal!::bool (::condvar)
		       "bgl_condvar_signal")

	       (method static $thread-parameters::obj ()
		       "BGL_PARAMETERS")
	       (method static $thread-parameters-set!::obj (::obj)
		       "BGL_PARAMETERS_SET")

	       (method static $thread-backend::obj ()
		       "BGL_THREAD_BACKEND")
	       (method static $thread-backend-set!::void (::obj)
		       "BGL_THREAD_BACKEND_SET")))

   (static  (class nothread-backend::thread-backend))

   (export  (class thread-backend
	       (name::bstring read-only))

	    (abstract-class thread
	       (thread-initialize!)
	       (name (default (gensym 'thread)))
	       (specific (get thread-specific) (set thread-specific-set!))
	       (cleanup (get thread-cleanup) (set thread-cleanup-set!)))

	    (class nothread::thread
	       (body::procedure read-only)
	       (%specific::obj (default #unspecified))
	       (%cleanup::obj (default #f))
	       (end-result::obj (default #unspecified))
	       (end-exception::obj (default #unspecified)))
	    
	    ;; dynamic env (per thread env)
            (inline dynamic-env?::bool ::obj)
	    (inline current-dynamic-env::dynamic-env)

	    ;; thread backend
	    (inline current-thread-backend)
	    (current-thread-backend-set! ::thread-backend)
	    (default-thread-backend)
	    (default-thread-backend-set! ::thread-backend)
	    (get-thread-backend ::bstring)
	    (generic tb-make-thread::thread ::thread-backend ::procedure ::obj)
	    (generic tb-mutex-initialize!::mutex ::thread-backend ::mutex)
	    (generic tb-condvar-initialize!::condvar ::thread-backend ::condvar)
	    (generic tb-current-thread::obj ::thread-backend)
	    
	    ;; thread
	    (generic thread-initialize! ::thread)
	    (generic thread-start! ::thread . scd)
	    (generic thread-start-joinable! ::thread)
	    (generic thread-join! ::thread . timeout)
	    (generic thread-terminate! ::thread)
	    (generic thread-specific::obj ::thread)
	    (generic thread-specific-set!::obj ::thread ::obj)
	    (generic thread-cleanup::obj ::thread)
	    (generic thread-cleanup-set!::obj ::thread ::obj)
	    (inline make-thread::thread ::procedure #!optional (name (gensym 'thread)))
	    (generic %user-current-thread ::thread)
	    (generic %user-thread-yield! ::thread)
	    (generic %user-thread-sleep! ::thread obj)
	    (current-thread::obj)
	    (inline thread-yield!)
	    (inline thread-sleep! ::obj)
	    (thread-parameter ::symbol)
	    (thread-parameter-set! ::symbol ::obj)
	    
	    ;; mutex
	    (inline mutex?::bool ::obj)
	    (inline make-mutex::mutex #!optional (name (gensym 'mutex)))
	    (inline make-spinlock::mutex #!optional (name (gensym 'spinlock)))
	    (mutex-nil::mutex)
	    (inline mutex-name::obj ::mutex)
	    (inline mutex-lock!::obj ::mutex #!optional (timeout::long 0))
	    (inline mutex-unlock!::obj ::mutex)
	    (inline mutex-state ::mutex)
	    (with-lock ::mutex ::procedure)
	    (with-timed-lock ::mutex ::int ::procedure)
   
	    ;; condition variable
	    (inline condition-variable?::bool ::obj)
	    (inline make-condition-variable::condvar #!optional (name (gensym 'condition-variable)))
	    (condition-variable-nil::condvar)
	    (inline condition-variable-name::obj ::condvar)
	    (inline condition-variable-wait!::bool ::condvar ::mutex #!optional (timeout::long 0))
	    (inline condition-variable-signal!::bool ::condvar)
	    (inline condition-variable-broadcast!::bool ::condvar))

   (pragma  ($current-dynamic-env nesting)
            ($dynamic-env? (predicate-of dynamic-env))
	    ($mutex? fail-safe)
	    ($make-mutex fail-safe)
	    ($make-spinlock fail-safe)
	    ($mutex-name fail-safe)
	    ($mutex-backend fail-safe)
	    ($mutex-state fail-safe)
	    ($mutex-lock fail-safe)
	    ($mutex-timed-lock fail-safe)
	    ($mutex-unlock fail-safe)
	    ($condvar? fail-safe)
	    ($make-condvar fail-safe)
	    ($condvar-name fail-safe)
	    ($condvar-broadcast! fail-safe)
	    ($condvar-signal! fail-safe)
	    ($condvar-wait! fail-safe)
	    ($condvar-timed-wait! fail-safe)))

;*---------------------------------------------------------------------*/
;*    dynamic-env? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (dynamic-env? obj)
   ($dynamic-env? obj))

;*---------------------------------------------------------------------*/
;*    current-dynamic-env ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (current-dynamic-env)
   ($current-dynamic-env))

;*---------------------------------------------------------------------*/
;*    *nothread-backend* ...                                           */
;*---------------------------------------------------------------------*/
(define *nothread-backend*
   (instantiate::nothread-backend
      (name "nothread")))

;*---------------------------------------------------------------------*/
;*    *thread-backends* ...                                            */
;*---------------------------------------------------------------------*/
(define *thread-backends*
   (list *nothread-backend*))

;*---------------------------------------------------------------------*/
;*    current-thread-backend ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (current-thread-backend)
   ($thread-backend))
      
;*---------------------------------------------------------------------*/
;*    current-thread-backend-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (current-thread-backend-set! tb)
   ($thread-backend-set! tb)
   tb)
      
;*---------------------------------------------------------------------*/
;*    default-thread-backend ...                                       */
;*---------------------------------------------------------------------*/
(define (default-thread-backend)
   (car *thread-backends*))

;*---------------------------------------------------------------------*/
;*    default-thread-backend-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (default-thread-backend-set! tb)
   (set! *thread-backends* (remq! tb *thread-backends*))
   (set! *thread-backends* (cons tb *thread-backends*)))

;*---------------------------------------------------------------------*/
;*    get-thread-backend ...                                           */
;*---------------------------------------------------------------------*/
(define (get-thread-backend name)
   (let loop ((tbs *thread-backends*))
      (when (pair? tbs)
	 (with-access::thread-backend (car tbs) ((tname name))
	    (if (string=? tname name)
		(car tbs)
		(loop (cdr tbs)))))))

;*---------------------------------------------------------------------*/
;*    tb-make-thread ::thread-backend ...                              */
;*---------------------------------------------------------------------*/
(define-generic (tb-make-thread tb::thread-backend body name))

;*---------------------------------------------------------------------*/
;*    tb-current-thread ::thread-backend ...                           */
;*---------------------------------------------------------------------*/
(define-generic (tb-current-thread tb::thread-backend))

;*---------------------------------------------------------------------*/
;*    *nothread-current* ...                                           */
;*---------------------------------------------------------------------*/
(define *nothread-current* #f)

;*---------------------------------------------------------------------*/
;*    tb-make-thread ::nothread-backend ...                            */
;*---------------------------------------------------------------------*/
(define-method (tb-make-thread tb::nothread-backend body name)
   (instantiate::nothread
      (body body)
      (name name)))

;*---------------------------------------------------------------------*/
;*    tb-current-thread ::nothread-backend ...                         */
;*---------------------------------------------------------------------*/
(define-method (tb-current-thread tb::nothread-backend)
   *nothread-current*)

;*---------------------------------------------------------------------*/
;*    tb-mutex-initialize! ::thread-backend ...                        */
;*---------------------------------------------------------------------*/
(define-generic (tb-mutex-initialize! tb::thread-backend mutex))

;*---------------------------------------------------------------------*/
;*    tb-condvar-initialize! ::thread-backend ...                      */
;*---------------------------------------------------------------------*/
(define-generic (tb-condvar-initialize! tb::thread-backend condvar))

;*---------------------------------------------------------------------*/
;*    object-display ::thread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (object-display o::thread . port)
   (with-output-to-port (if (pair? port) (car port) (current-output-port))
      (lambda ()
         (with-access::thread o (name)
            (display* "#<" (class-name (object-class o)) ":" name ">")))))

;*---------------------------------------------------------------------*/
;*    object-write ::thread ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-write o::thread . port)
   (with-output-to-port (if (pair? port) (car port) (current-output-port))
      (lambda ()
         (with-access::thread o (name)
            (display* "#<" (class-name (object-class o)) ":" name ">")))))

;*---------------------------------------------------------------------*/
;*    object-print ::thread ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-print o::thread port print-slot)
   (object-write o port))

;*---------------------------------------------------------------------*/
;*    thread-initialize! ::thread ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (thread-initialize! th::thread)
   th)

;*---------------------------------------------------------------------*/
;*    thread-start! ::thread ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (thread-start! th::thread . sc))

;*---------------------------------------------------------------------*/
;*    thread-start-joinable! ::thread ...                              */
;*---------------------------------------------------------------------*/
(define-generic (thread-start-joinable! th::thread))

;*---------------------------------------------------------------------*/
;*    thread-join! ::thread ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (thread-join! th::thread . timeout))

;*---------------------------------------------------------------------*/
;*    thread-terminate! ::thread ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (thread-terminate! th::thread))

;*---------------------------------------------------------------------*/
;*    thread-specific ::thread ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (thread-specific th::thread))

;*---------------------------------------------------------------------*/
;*    thread-specific-set! ::thread ...                                */
;*---------------------------------------------------------------------*/
(define-generic (thread-specific-set! th::thread v::obj))

;*---------------------------------------------------------------------*/
;*    thread-cleanup ::thread ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (thread-cleanup th::thread))

;*---------------------------------------------------------------------*/
;*    thread-cleanup-set! ::thread ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (thread-cleanup-set! th::thread v::obj))

;*---------------------------------------------------------------------*/
;*    make-thread ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (make-thread body #!optional (name (gensym 'thread)))
   (tb-make-thread (default-thread-backend) body name))

;*---------------------------------------------------------------------*/
;*    %user-current-thread ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (%user-current-thread o::thread)
   o)

;*---------------------------------------------------------------------*/
;*    %current-thread ...                                              */
;*---------------------------------------------------------------------*/
(define (%current-thread)
   (let ((tb (current-thread-backend)))
      (when (isa? tb thread-backend)
 	 (tb-current-thread tb))))

;*---------------------------------------------------------------------*/
;*    current-thread ...                                               */
;*---------------------------------------------------------------------*/
(define (current-thread)
   (let ((th (%current-thread)))
      (when (isa? th thread)
	 (%user-current-thread th))))

;*---------------------------------------------------------------------*/
;*    %user-thread-sleep! ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (%user-thread-sleep! o::thread d)
   (cond
      ((date? d)
       (let* ((cdt (date->seconds (current-date)))
	      (dt (date->seconds d))
	      (a (* (-elong dt cdt) 1000000)))
	  (when (>elong a #e0) (sleep (elong->fixnum a)))))
      ((fixnum? d)
       (sleep (* d 1000)))
      ((elong? d)
       (sleep (* (elong->fixnum d) 1000)))
      ((llong? d)
       (sleep (* (llong->fixnum d) 1000)))
      ((real? d)
       (sleep (flonum->fixnum (* d 1000000.0))))
      (else
       (bigloo-type-error 'thread-sleep! "date, real, or integer" d))))

;*---------------------------------------------------------------------*/
;*    thread-sleep! ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (thread-sleep! obj)
   (%user-thread-sleep! (current-thread) obj))

;*---------------------------------------------------------------------*/
;*    %user-thread-yield! ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (%user-thread-yield! o::thread)
   #unspecified)
  
;*---------------------------------------------------------------------*/
;*    thread-yield! ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (thread-yield!)
   (%user-thread-yield! (current-thread)))
   
;*---------------------------------------------------------------------*/
;*    thread-parameter ...                                             */
;*---------------------------------------------------------------------*/
(define (thread-parameter id)
   (let ((c (assq id ($thread-parameters))))
      (if (pair? c)
	  (cdr c)
	  #f)))

;*---------------------------------------------------------------------*/
;*    thread-parameter-set! ...                                        */
;*---------------------------------------------------------------------*/
(define (thread-parameter-set! id val)
   (let ((c (assq id ($thread-parameters))))
      (if (pair? c)
	  (set-cdr! c val)
	  (begin
	     ($thread-parameters-set!
		(cons (cons id val) ($thread-parameters)))
	     val))))

;*---------------------------------------------------------------------*/
;*    thread-start! ::nothread ...                                     */
;*---------------------------------------------------------------------*/
(define-method (thread-start! th::nothread . scd)
   (let ((thread *nothread-current*))
      (unwind-protect
	 (with-access::nothread th (end-result end-exception body)
	    (set! *nothread-current* th)
	    (with-handler
	       (lambda (e)
		  (set! end-exception e)
		  (raise e))
	       (set! end-result (body)))
	    th)
	 (set! *nothread-current* thread))))

;*---------------------------------------------------------------------*/
;*    thread-start-joinable! ::nothread ...                            */
;*---------------------------------------------------------------------*/
(define-method (thread-start-joinable! th::nothread)
   (thread-start! th))

;*---------------------------------------------------------------------*/
;*    thread-join! ::nothread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (thread-join! th::nothread . timeout)
   (with-access::nothread th (end-result end-exception)
      (if (isa? end-exception &exception)
	  (raise end-exception)
	  end-result)))

;*---------------------------------------------------------------------*/
;*    thread-terminate! ::nothread ...                                 */
;*---------------------------------------------------------------------*/
(define-method (thread-terminate! th::nothread)
   (with-access::nothread th (%cleanup)
      (when (procedure? %cleanup) (%cleanup))
      (exit 0)))

;*---------------------------------------------------------------------*/
;*    thread-specific ::nothread ...                                   */
;*---------------------------------------------------------------------*/
(define-method (thread-specific th::nothread)
   (with-access::nothread th (%specific) %specific))

;*---------------------------------------------------------------------*/
;*    thread-specific-set! ::nothread ...                              */
;*---------------------------------------------------------------------*/
(define-method (thread-specific-set! th::nothread v)
   (with-access::nothread th (%specific) (set! %specific v)))

;*---------------------------------------------------------------------*/
;*    thread-cleanup ::nothread ...                                    */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup th::nothread)
   (with-access::nothread th (%cleanup) %cleanup))

;*---------------------------------------------------------------------*/
;*    thread-cleanup-set! ::nothread ...                               */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup-set! th::nothread v)
   (with-access::nothread th (%cleanup) (set! %cleanup v)))

;*---------------------------------------------------------------------*/
;*    mutex? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (mutex? obj)
   ($mutex? obj))

;*---------------------------------------------------------------------*/
;*    make-mutex ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (make-mutex #!optional (name (gensym 'mutex)))
   ($make-mutex name))

;*---------------------------------------------------------------------*/
;*    make-spinlock ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (make-spinlock #!optional (name (gensym 'spinlock)))
   ($make-spinlock name))

;*---------------------------------------------------------------------*/
;*    mutex-nil ...                                                    */
;*---------------------------------------------------------------------*/
(define *mutex-nil* ($make-nil-mutex))
(define (mutex-nil) *mutex-nil*)

;*---------------------------------------------------------------------*/
;*    mutex-name ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (mutex-name obj)
   ($mutex-name obj))

;*---------------------------------------------------------------------*/
;*    mutex-lock! ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (mutex-lock! m #!optional (timeout::long 0))
   (if (=fx timeout 0)
       (=fx ($mutex-lock m) 0)
       (=fx ($mutex-timed-lock m timeout) 0)))

;*---------------------------------------------------------------------*/
;*    mutex-unlock! ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (mutex-unlock! m)
   (=fx ($mutex-unlock m) 0))

;*---------------------------------------------------------------------*/
;*    mutex-state ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (mutex-state mutex)
   ($mutex-state mutex))

;*---------------------------------------------------------------------*/
;*    mutex-backend ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (mutex-backend obj)
   ($mutex-backend obj))

;*---------------------------------------------------------------------*/
;*    with-lock ...                                                    */
;*---------------------------------------------------------------------*/
(define (with-lock mutex thunk)
   (synchronize mutex
      (thunk)))

;*---------------------------------------------------------------------*/
;*    with-timed-lock ...                                              */
;*---------------------------------------------------------------------*/
(define (with-timed-lock mutex timeout thunk)
   (when (mutex-lock! mutex timeout)
      (unwind-protect
	 (thunk)
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    condition-variable? ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (condition-variable? obj)
   ($condvar? obj))

;*---------------------------------------------------------------------*/
;*    make-condition-variable ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (make-condition-variable #!optional
					(name (gensym 'condition-variable)))
   ($make-condvar name))

;*---------------------------------------------------------------------*/
;*    condition-variable-nil ...                                       */
;*---------------------------------------------------------------------*/
(define *condition-variable-nil* ($make-nil-condvar))
(define (condition-variable-nil) *condition-variable-nil*)

;*---------------------------------------------------------------------*/
;*    condition-variable-name ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (condition-variable-name obj)
   ($condvar-name obj))

;*---------------------------------------------------------------------*/
;*    condition-variable-wait! ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (condition-variable-wait! c m #!optional (timeout::long 0))
   (if (=fx timeout 0)
       ($condvar-wait! c m)
       ($condvar-timed-wait! c m timeout)))

;*---------------------------------------------------------------------*/
;*    condition-variable-signal! ...                                   */
;*---------------------------------------------------------------------*/
(define-inline (condition-variable-signal! c)
   ($condvar-signal! c))

;*---------------------------------------------------------------------*/
;*    condition-variable-broadcast! ...                                */
;*---------------------------------------------------------------------*/
(define-inline (condition-variable-broadcast! c)
   ($condvar-broadcast! c))

