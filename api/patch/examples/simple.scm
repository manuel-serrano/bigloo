;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/patch/examples/simple.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  2 08:46:51 2017                          */
;*    Last change :  Fri Jun  2 15:45:32 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Simple example of self-modifying code                            */
;*    -------------------------------------------------------------    */
;*    To be compiled with:                                             */
;*      -runtime-code-patching -fruntime-code-patching                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foo
   (library patch)
   (main main))

;*---------------------------------------------------------------------*/
;*    patch indexes                                                    */
;*---------------------------------------------------------------------*/
(define p0 (patch-index))

;*---------------------------------------------------------------------*/
;*    Clazz ...                                                        */
;*---------------------------------------------------------------------*/
(define Clazz (cons 1 2))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((n (length argv)))
      (patch-set! (the-patch-table) p0 3)
      (print "bar=" (bar n))
      (print "gee=" (get/cache (cons Clazz '#(1 2 3))))
      (print "gee=" (get/cache (cons Clazz '#(1 2 3))))))

;*---------------------------------------------------------------------*/
;*    bar ...                                                          */
;*---------------------------------------------------------------------*/
(define (bar x)
   (+fx x (patch p0 0)))

;*---------------------------------------------------------------------*/
;*    get/cache ...                                                    */
;*---------------------------------------------------------------------*/
(define (get/cache o)
   (let ((pid0 (patch-index))
	 (pid1 (patch-index)))
      (if (in-cache? o (patch pid0 '()))
	  (begin
	     (print "in cache")
	     (vector-ref (cdr o) (patch pid1 0)))
	  (fill-cache! o (the-patch-table) pid0 pid1))))

(define (in-cache? o clazz)
   (eq? (car o) clazz))

(define (fill-cache! o table pclass pref)
   (print "fill cache")
   (patch-set! table pclass (car o))
   (patch-set! table pref 2)
   (vector-ref (cdr o) 2))
