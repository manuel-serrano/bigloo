;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/trace.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 11 10:01:47 2003                          */
;*    Last change :  Fri Dec 31 12:17:30 2010 (serrano)                */
;*    Copyright   :  2003-10 Manuel Serrano                            */
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
	   __pp_circle)

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
	   
	   (%with-trace ::int ::obj ::procedure)))

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
	 (when (>=fx (bigloo-debug) (trace-alist-get al 'margin-level))
	    (let ((p (trace-port)))
	       (display (trace-alist-get al 'margin) p)
	       (display (trace-color (-fx (trace-alist-get al 'depth) 1) "- "))
	       (for-each (lambda (a) (display-circle a p)) args)
	       (newline p))))))

;*---------------------------------------------------------------------*/
;*    %with-trace ...                                                  */
;*---------------------------------------------------------------------*/
(define (%with-trace lvl lbl thunk)
   (mutex-lock! (trace-mutex))
   (let* ((al (trace-alist))
	  (ol (trace-alist-get al 'margin-level)))
      (trace-alist-set! al 'margin-level lvl)
      (let ((r (if (>=fx (bigloo-debug) lvl)
		   (with-output-to-port (trace-port)
		      (lambda ()
			 (let* ((d (trace-alist-get al 'depth))
				(om (trace-alist-get al 'margin))
				(de (trace-alist-get al 'depth))
				(ma (trace-color de "  |")))
			    (display (trace-alist-get al 'margin))
			    (display (if (=fx d 0)
					 (trace-color d "+ " lbl)
					 (trace-color d "--+ " lbl)))
			    (newline)
			    (trace-alist-set! al 'depth (+fx de 1))
			    (trace-alist-set! al 'margin (string-append om ma))
			    (mutex-unlock! (trace-mutex))
			    (let ((res (thunk)))
			       (trace-alist-set! al 'depth de)
			       (trace-alist-set! al 'margin om)
			       res))))
		   (begin
		      (mutex-unlock! (trace-mutex))
		      (thunk)))))
	 (trace-alist-set! al 'margin-level ol)
	 r)))

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
		
