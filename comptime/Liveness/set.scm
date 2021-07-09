;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Liveness/set.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 22 09:48:04 2013                          */
;*    Last change :  Fri Jul  9 07:35:56 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Liveness set handling                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module liveness_set
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    module_module
	    engine_param
	    liveness_types)
   (export  (in? ::local ::pair-nil)
	    (subset? ::pair-nil ::pair-nil)
	    (union::pair-nil . ::pair-nil)
	    (add ::local/liveness pair-nil)
	    (intersection::pair-nil . ::pair-nil)
	    (disjonction::pair-nil ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    variable-reset! ...                                              */
;*---------------------------------------------------------------------*/
(define (variable-reset! v)
   (with-access::local/liveness v (%count)
      (set! %count 0)))

;*---------------------------------------------------------------------*/
;*    in? ...                                                          */
;*---------------------------------------------------------------------*/
(define (in? el set)
   (when (isa? el local/liveness)
      (memq el set)))

;*---------------------------------------------------------------------*/
;*    subset? ...                                                      */
;*    -------------------------------------------------------------    */
;*    is set1 a subset of set2?                                        */
;*---------------------------------------------------------------------*/
(define (subset? set1 set2)
   
   (define (variable-mark! v)
      (with-access::local/liveness v (%count)
	 (set! %count 1)))

   (for-each variable-reset! set1)
   (for-each variable-mark! set2)

   (every (lambda (v)
	     (with-access::local/liveness v (%count)
		(=fx %count 1)))
      set1))

;*---------------------------------------------------------------------*/
;*    union ...                                                        */
;*    -------------------------------------------------------------    */
;*    union of N lists of local variables                              */
;*---------------------------------------------------------------------*/
(define (union . ls)
   
   (define res '())
   
   (define (variable-mark! v)
      (with-access::local/liveness v (%count)
	 (when (=fx %count 0)
	    (set! %count 1)
	    (set! res (cons v res)))))

   (for-each (lambda (l) (for-each variable-reset! l)) ls)
   (for-each (lambda (l) (for-each variable-mark! l)) ls)
   res)

;*---------------------------------------------------------------------*/
;*    add ...                                                          */
;*---------------------------------------------------------------------*/
(define (add el l)
   (union (list el) l))

;*---------------------------------------------------------------------*/
;*    intersection ...                                                 */
;*    -------------------------------------------------------------    */
;*    intersection of N lists of local variables                       */
;*---------------------------------------------------------------------*/
(define (intersection . ls)
   
   (define (variable-mark! v)
      (with-access::local/liveness v (%count)
	 (set! %count (+fx 1 %count))))
   
   (define (mark-list! l)
      (for-each variable-mark! l))
   
   (for-each (lambda (l) (for-each variable-reset! l)) ls)
   (for-each mark-list! ls)
   
   (let ((%count (length ls)))
      (filter (lambda (l)
		 (with-access::local/liveness l ((c %count))
		    (=fx c %count)))
	 (car ls))))

;*---------------------------------------------------------------------*/
;*    disjonction ...                                                  */
;*    -------------------------------------------------------------    */
;*    disjonction of 2 lists of local variables                        */
;*---------------------------------------------------------------------*/
(define (disjonction l1 l2)
   
   (define (variable-mark! v)
      (with-access::local/liveness v (%count)
	 (set! %count 1)))
   
   (for-each variable-reset! l1)
   (for-each variable-mark! l2)
   
   (filter (lambda (v)
	      (with-access::local/liveness v (%count)
		 (=fx %count 0)))
      l1))
