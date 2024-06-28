;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawBbv/bbv-config.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 07:16:40 2022                          */
;*    Last change :  Fri Jun 28 07:37:04 2024 (serrano)                */
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
	   *bbv-dump-json*
	   *bbv-dump-cfg*
	   *bbv-profile*
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
	 ((string=? e "") #t)
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
	 ((not e) 'cnt)
	 ((string=? e "false") #f)
	 ((string=? e "ssr") 'ssr)
	 ((string=? e "cnt") 'cnt)
	 (else (error "bbv-blocks-gc" "unknown value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-merge-strategy* ...                                         */
;*---------------------------------------------------------------------*/
(define *bbv-merge-strategy*
   (let ((e (getenv "BIGLOOBBVSTRATEGY")))
      (cond
	 ((not e) 'score-)
	 ((string=? e "adn") 'adn) ;; adn in BIGLOOBBVADN (see bbv-merge.scm)
	 ((string=? e "score-") 'score-)
	 ((string=? e "score2") 'score2)
	 ((string=? e "score+") 'score+)
	 ((string=? e "score*") 'score*)
	 ((string=? e "score") 'score)
	 ((string=? e "sametypes") 'sametypes)
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
	 ((not e) #t)
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
;*    *bbv-dump-json* ...                                              */
;*---------------------------------------------------------------------*/
(define *bbv-dump-json*
   (let ((e (getenv "BIGLOOBBVDUMPJSON")))
      (cond
	 ((not e) #f)
	 ((string=? e "true") #t)
	 ((string=? e "false") #f)
	 ((string=? e "") #f)
	 (else (error "bbv-dump-json" "illegal value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-dump-cfg* ...                                               */
;*---------------------------------------------------------------------*/
(define *bbv-dump-cfg*
   (let ((e (getenv "BIGLOOBBVDUMPCFG")))
      (cond
	 ((not e) #f)
	 ((string=? e "true") #t)
	 ((string=? e "false") #f)
	 ((string=? e "") #f)
	 (else (error "bbv-dump-cfg" "illegal value" e)))))

;*---------------------------------------------------------------------*/
;*    *bbv-profile* ...                                                */
;*---------------------------------------------------------------------*/
(define *bbv-profile*
   (let ((e (getenv "BIGLOOBBVPROFILE")))
      (cond
	 ((not e) #f)
	 ((string=? e "true") #t)
	 ((string=? e "false") #f)
	 (else (error "bbv-profile" "illegal value" e)))))

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

