;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/trace.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 11 10:01:47 2003                          */
;*    Last change :  Sun Aug 25 09:10:19 2019 (serrano)                */
;*    Copyright   :  2003-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple tracing facilities                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __trace

   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __bignum
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
	   __foreign
	   __error
	   __evenv
	   __os
	   __pp_circle
	   __bit)

   (import __param
	   __object
	   __thread

	   __r4_output_6_10_3)

   (extern (macro $debug-alist::pair-nil () "BGL_DEBUG_ALIST_GET")
	   (macro $debug-alist-set!::void (::pair-nil) "BGL_DEBUG_ALIST_SET"))
	   
   (java   (class foreign
	      (method static $debug-alist::pair-nil ()
		      "BGL_DEBUG_ALIST_GET")
	      (method static $debug-alist-set!::void (::pair-nil)
		      "BGL_DEBUG_ALIST_SET")))
	   
   (export (trace-port::output-port)
	   (trace-port-set! ::output-port)
	   (trace-margin::bstring)
	   (trace-margin-set! ::bstring)
	   (trace-color::bstring ::int . ::obj)
	   (trace-bold::bstring . ::obj)
	   (trace-string ::obj)
	   (trace-item . ::obj)
	   (trace-active? ::obj)
	   
	   (%with-trace ::obj ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    *trace-mutex* ...                                                */
;*---------------------------------------------------------------------*/
(define *trace-mutex* (make-mutex "trace"))

;*---------------------------------------------------------------------*/
;*    trace-mutex ...                                                  */
;*---------------------------------------------------------------------*/
(define (trace-mutex)
   *trace-mutex*)

;*---------------------------------------------------------------------*/
;*    trace-alist ...                                                  */
;*---------------------------------------------------------------------*/
(define (trace-alist)
   (let ((al ($debug-alist)))
      (if (pair? al)
	  al
	  (let ((new-al (list (cons 'port (current-error-port))
			      (cons 'depth 0)
			      (cons 'margin "")
			      (cons 'margin-level 0))))
	     ($debug-alist-set! new-al)
	     new-al))))

;*---------------------------------------------------------------------*/
;*    trace-alist-get ...                                              */
;*---------------------------------------------------------------------*/
(define (trace-alist-get alist key)
   (let ((c (assq key alist)))
      (if (pair? c)
	  (cdr c)
	  (error 'trace-alist-get "Can't find trace-value" key))))

;*---------------------------------------------------------------------*/
;*    trace-alist-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (trace-alist-set! alist key val)
   (let ((c (assq key alist)))
      (if (pair? c)
	  (set-cdr! c val)
	  (error 'trace-alist-set! "Can't find trace-value" key))))

;*---------------------------------------------------------------------*/
;*    trace-port ...                                                   */
;*---------------------------------------------------------------------*/
(define (trace-port)
   (trace-alist-get (trace-alist) 'port))

;*---------------------------------------------------------------------*/
;*    trace-port-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (trace-port-set! p)
   (trace-alist-set! (trace-alist) 'port p))

;*---------------------------------------------------------------------*/
;*    trace-margin ...                                                 */
;*---------------------------------------------------------------------*/
(define (trace-margin)
   (trace-alist-get (trace-alist) 'margin))

;*---------------------------------------------------------------------*/
;*    trace-margin-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (trace-margin-set! m)
   (trace-alist-set! (trace-alist) 'margin m))

;*---------------------------------------------------------------------*/
;*    trace-color ...                                                  */
;*---------------------------------------------------------------------*/
(define (trace-color col::int . o)
   (with-output-to-string
      (if (bigloo-trace-color)
	  (lambda ()
	     (display* "[0m[1;" (+fx 31 col) "m")
	     (for-each display-circle o)
	     (display "[0m"))
	  (lambda ()
	     (for-each display-circle o)))))

;*---------------------------------------------------------------------*/
;*    trace-bold ...                                                   */
;*---------------------------------------------------------------------*/
(define (trace-bold . o)
   (apply trace-color -30 o))

;*---------------------------------------------------------------------*/
;*    tty-trace-color ...                                              */
;*---------------------------------------------------------------------*/
(define (tty-trace-color col::int . o)
   (if (output-port-isatty? (trace-port))
       (apply trace-color col o)
       (with-output-to-string
	  (lambda ()
	     (for-each display-circle o)))))

;*---------------------------------------------------------------------*/
;*    trace-string ...                                                 */
;*---------------------------------------------------------------------*/
(define (trace-string o)
   (with-output-to-string
      (lambda ()
	 (write o))))

;*---------------------------------------------------------------------*/
;*    trace-item ...                                                   */
;*---------------------------------------------------------------------*/
(define (trace-item . args)
   (when (>fx (bigloo-debug) 0)
      (let ((al (trace-alist)))
	 (when (trace-active? (trace-alist-get al 'margin-level))
	    (let ((p (trace-port)))
	       (synchronize (trace-mutex)
		  (display (trace-alist-get al 'margin) p)
		  (display (tty-trace-color (-fx (trace-alist-get al 'depth) 1) "- ") p)
		  (for-each (lambda (a) (display-circle a p)) args)
		  (newline p)
		  (flush-output-port p)))))))

;*---------------------------------------------------------------------*/
;*    trace-active? ...                                                */
;*---------------------------------------------------------------------*/
(define (trace-active? lvl)
   (cond
      ((integer? lvl) (>=fx (bigloo-debug) lvl))
      ((symbol? lvl) (memq lvl (bigloo-trace)))))

;*---------------------------------------------------------------------*/
;*    %with-trace ...                                                  */
;*---------------------------------------------------------------------*/
(define (%with-trace lvl lbl thunk)
   (let* ((al (trace-alist))
	  (ol (trace-alist-get al 'margin-level)))
      (trace-alist-set! al 'margin-level lvl)
      (if (trace-active? lvl)
	  (let* ((d (trace-alist-get al 'depth))
		 (om (trace-alist-get al 'margin))
		 (ma (tty-trace-color d "  |")))
	     (synchronize (trace-mutex)
		(with-output-to-port (trace-port)
		   (lambda ()
		      (display (trace-alist-get al 'margin))
		      (display (if (=fx d 0)
				   (tty-trace-color d "+ " lbl)
				   (tty-trace-color d "--+ " lbl)))
		      (newline)
		      (flush-output-port (current-output-port)))))
	     (trace-alist-set! al 'depth (+fx d 1))
	     (trace-alist-set! al 'margin (string-append om ma))
	     (unwind-protect
		(thunk)
		(begin
		   (trace-alist-set! al 'depth d)
		   (trace-alist-set! al 'margin om)
		   (trace-alist-set! al 'margin-level ol))))
	  (unwind-protect
	     (thunk)
	     (trace-alist-set! al 'margin-level ol)))))

;*---------------------------------------------------------------------*/
;*    example                                                          */
;*---------------------------------------------------------------------*/
;; (%with-trace 0 'foo1.1
;; 	     (lambda ()
;; 		(trace-item 'foo2.1)
;; 		(trace-item 'foo2.2)
;; 		(%with-trace 0 'foo2.3
;; 			     (lambda ()
;; 				(trace-item 'foo3.1)
;; 				(%with-trace 0 'foo3.2
;; 					     (lambda ()
;; 						(trace-item 'foo4.1)
;; 						(trace-item 'foo4.2)))
;; 				(trace-item 'foo3.3)))
;; 		(trace-item 'foo2.4)))
		
