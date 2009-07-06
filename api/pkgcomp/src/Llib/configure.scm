;; automatically generated. Don't edit
;*=====================================================================*/
;*    .../project/bigloo/api/pkgcomp/src/Llib/configure.scm.in         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 11 15:24:57 2006                          */
;*    Last change :  Wed May 30 07:48:21 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Pkgcomp configuration                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module @configure
   (include "configure.sch")
   (export  (pkgcomp-version::bstring)
	    (pkgcomp-languages::pair-nil)
	    (pkgcomp-language-mark::symbol)
	    (pkgcomp-default-suffix::bstring)
	    (pkgcomp-root-exception::symbol)
	    (pkgcomp-root-record::symbol)
	    (pkgcomp-interface-keywords::pair-nil)))

;*---------------------------------------------------------------------*/
;*    pkgcomp-version ...                                              */
;*---------------------------------------------------------------------*/
(define (pkgcomp-version)
   "0.0.1")

