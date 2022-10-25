;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawBbv/bbv-config.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 07:16:40 2022                          */
;*    Last change :  Tue Oct 25 10:08:54 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    bbv global configuration                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-config
   (export *max-block-merge-versions*
	   *max-block-nomerge-versions*
	   *type-call*
	   *type-loadi*
	   *type-loadg*
	   *merge-ctx-costs*))

;*---------------------------------------------------------------------*/
;*    basic-block versioning configuration                             */
;*---------------------------------------------------------------------*/
;; the maximum number of block versions
(define *max-block-merge-versions* 3)
(define *max-block-nomerge-versions* 6)


;; various optimizations
(define *type-call* #t)
(define *type-loadi* #t)
(define *type-loadg* #t)

;*---------------------------------------------------------------------*/
;*    Merge block and contexts operators ...                           */
;*---------------------------------------------------------------------*/
(define *merge-ctx-costs* +) ;; any n-ary int functions, e.g., max, min, +

