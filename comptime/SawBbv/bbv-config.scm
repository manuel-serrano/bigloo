;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawBbv/bbv-config.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 07:16:40 2022                          */
;*    Last change :  Fri Oct 28 07:04:42 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    bbv global configuration                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-config
   (export *bbv-debug*
	   *bbv-blocks-cleanup*
	   *bbv-merge-strategy*
	   *max-block-merge-versions*
	   *max-block-limit*
	   *type-call*
	   *type-loadi*
	   *type-loadg*))

;*---------------------------------------------------------------------*/
;*    *bbv-blocks-debug* ...                                           */
;*---------------------------------------------------------------------*/
(define *bbv-debug*
   (let ((e (getenv "BIGLOOBBVDEBUG")))
      (cond
	 ((not e) #f)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 (else (error "bbv-blocks-debug" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-blocks-cleanup* ...                                         */
;*---------------------------------------------------------------------*/
(define *bbv-blocks-cleanup*
   (let ((e (getenv "BIGLOOBBVCLEANUP")))
      (cond
	 ((not e) #t)
	 ((string=? e "false") #f)
	 ((string=? e "true") #t)
	 (else (error "bbv-blocks-cleanup" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-merge-strategy* ...                                         */
;*---------------------------------------------------------------------*/
(define *bbv-merge-strategy*
   (let ((e (getenv "BIGLOOBBVSTRATEGY")))
      (cond
	 ((not e) 'size)
	 ((string=? e "first") 'first)
	 ((string=? e "size") 'size)
	 ((string=? e "random") 'random)
	 ((string=? e "distance") 'distance)
	 (else (error "bbv-merge-strategy" "unknown strategy" e)))))

;*---------------------------------------------------------------------*/
;*    basic-block versioning configuration                             */
;*---------------------------------------------------------------------*/
;; the maximum number of block versions
(define *max-block-merge-versions* 3)
(define *max-block-limit* 6)


;; various optimizations
(define *type-call* #t)
(define *type-loadi* #t)
(define *type-loadg* #t)
