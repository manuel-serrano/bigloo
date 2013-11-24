;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/set.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 22 09:48:04 2013                          */
;*    Last change :  Fri Nov 22 14:36:06 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Narrow set handling                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module narrow_set
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
	    narrow_types)
   (export  (in? ::local ::pair-nil)
	    (union::pair-nil . ::pair-nil)
	    (add ::local/narrow pair-nil)
	    (intersection::pair-nil . ::pair-nil)
	    (disjonction::pair-nil ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    variable-reset! ...                                              */
;*---------------------------------------------------------------------*/
(define (variable-reset! v)
   (with-access::local/narrow v (%count)
      (set! %count 0)))

;*---------------------------------------------------------------------*/
;*    in? ...                                                          */
;*---------------------------------------------------------------------*/
(define (in? el set)
   (when (isa? el local/narrow)
      (memq el set)))

;*---------------------------------------------------------------------*/
;*    union ...                                                        */
;*    -------------------------------------------------------------    */
;*    union of N lists of local variables                              */
;*---------------------------------------------------------------------*/
(define (union . ls)
   
   (define res '())
   
   (define (variable-mark! v)
      (with-access::local/narrow v (%count)
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
      (with-access::local/narrow v (%count)
	 (set! %count (+fx 1 %count))))
   
   (define (mark-list! l)
      (for-each variable-mark! l))
   
   (for-each (lambda (l) (for-each variable-reset! l)) ls)
   (for-each mark-list! ls)
   
   (let ((%count (length ls)))
      (filter (lambda (l)
		 (with-access::local/narrow l ((c %count))
		    (=fx c %count)))
	 (car ls))))

;*---------------------------------------------------------------------*/
;*    disjonction ...                                                  */
;*    -------------------------------------------------------------    */
;*    disjonction of 2 lists of local variables                        */
;*---------------------------------------------------------------------*/
(define (disjonction l1 l2)
   
   (define (variable-mark! v)
      (with-access::local/narrow v (%count)
	 (set! %count 1)))
   
   (for-each variable-reset! l1)
   (for-each variable-mark! l2)
   
   (filter (lambda (v)
	      (with-access::local/narrow v (%count)
		 (=fx %count 0)))
      l1))
