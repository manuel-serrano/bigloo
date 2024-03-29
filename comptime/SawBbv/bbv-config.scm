;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawBbv/bbv-config.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 07:16:40 2022                          */
;*    Last change :  Wed Jan 17 07:49:44 2024 (serrano)                */
;*    Copyright   :  2022-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    bbv global configuration                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-config
   (export *bbv-debug*
	   *bbv-log*
	   *bbv-assert*
	   *bbv-blocks-cleanup*
	   *bbv-blocks-gc*
	   *bbv-merge-strategy*
	   *bbv-optim-vlength*
	   *bbv-optim-bound*
	   *bbv-optim-alias*
	   *bbv-verbose*
	   *max-block-merge-versions*
	   *max-block-limit*
	   *type-call*
	   *type-loadi*
	   *type-loadg*))

;*---------------------------------------------------------------------*/
;*    *bbv-debug* ...                                                  */
;*---------------------------------------------------------------------*/
(define *bbv-debug*
   (let ((e (getenv "BIGLOOBBVDEBUG")))
      (cond
	 ((not e) #f)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 (else (error "bbv-debug" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-log* ...                                                    */
;*---------------------------------------------------------------------*/
(define *bbv-log*
   (let ((e (getenv "BIGLOOBBVLOG")))
      (cond
	 ((not e) #f)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 (else (error "bbv-log" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-assert* ...                                                 */
;*---------------------------------------------------------------------*/
(define *bbv-assert*
   (let ((e (getenv "BIGLOOBBVASSERT")))
      (if (not e)
	  0
	  (or (string->number e)
	      (error "bbv-assert" "illegal value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-blocks-cleanup* ...                                         */
;*---------------------------------------------------------------------*/
(define *bbv-blocks-cleanup*
   (let ((e (getenv "BIGLOOBBVCLEANUP")))
      (cond
	 ((not e) #t)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 ((every (lambda (s)
		     (member s '("gc" "coalesce" "simplify" "goto" "nop")))
	     (string-split e " "))
	  e)
	 (else ((error "bbv-blocks-cleanup" "unknown value" e))))))

;*---------------------------------------------------------------------*/
;*    *bbv-blocks-gc* ...                                              */
;*---------------------------------------------------------------------*/
(define *bbv-blocks-gc*
   (let ((e (getenv "BIGLOOBBVGC")))
      (cond
	 ((not e) #t)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 (else (error "bbv-blocks-gc" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-merge-strategy* ...                                         */
;*---------------------------------------------------------------------*/
(define *bbv-merge-strategy*
   (let ((e (getenv "BIGLOOBBVSTRATEGY")))
      (cond
	 ((not e) 'nearobj)
	 ((string=? e "first") 'first)
	 ((string=? e "nearobj") 'nearobj)
	 ((string=? e "size") 'size)
	 ((string=? e "random") 'random)
	 ((string=? e "distance") 'distance)
	 (else (error "bbv-merge-strategy" "unknown strategy" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-optim-vlength* ...                                          */
;*---------------------------------------------------------------------*/
(define *bbv-optim-vlength*
   (let ((e (getenv "BIGLOOBBVVLENGTH")))
      (cond
	 ((not e) #f)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 (else (error "bbv-optim-vlength" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-optim-bound* ...                                            */
;*---------------------------------------------------------------------*/
(define *bbv-optim-bound*
   (let ((e (or (getenv "BIGLOOBBVBOUND") (getenv "BIGLOOBBVVLENGTH"))))
      (cond
	 ((not e) #f)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 (else (error "bbv-optim-bound" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-optim-alias* ...                                            */
;*---------------------------------------------------------------------*/
(define *bbv-optim-alias*
   (let ((e (getenv "BIGLOOBBVALIAS")))
      (cond
	 ((not e) #t)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 (else (error "bbv-optim-alias" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-verbose* ...                                                */
;*---------------------------------------------------------------------*/
(define *bbv-verbose*
   (let ((e (getenv "BIGLOOBBVVERBOSE")))
      (if (not e)
	  1
	  (or (string->number e)
	      (error "bbv-verbose" "illegal value" e)))))

;*---------------------------------------------------------------------*/
;*    basic-block versioning configuration                             */
;*---------------------------------------------------------------------*/
;; the maximum number of block versions
(define *max-block-merge-versions*
   (let ((e (getenv "BIGLOOBBVVERSIONLIMIT")))
      (if (string? e)
	  (string->integer e)
	  4)))
(define *max-block-limit* *max-block-merge-versions*)

;; various optimizations
(define *type-call* #t)
(define *type-loadi* #t)
(define *type-loadg* #t)

