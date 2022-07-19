;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-config.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 19 07:16:40 2022                          */
;*    Last change :  Tue Jul 19 16:12:03 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    bbv global configuration                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-config
   (export *max-block-versions*
	   *type-call*
	   *type-loadi*
	   *merge-ctx-costs*))

;*---------------------------------------------------------------------*/
;*    basic-block versioning configuration                             */
;*---------------------------------------------------------------------*/
;; the maximum number of block versions
(define *max-block-versions* 5)

;; various optimizations
(define *type-call* #t)
(define *type-loadi* #t)

;*---------------------------------------------------------------------*/
;*    Merge block and contexts operators ...                           */
;*---------------------------------------------------------------------*/
(define *merge-ctx-costs* +) ;; any n-ary int functions, e.g., max, min, +

