;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo0.3/Translate/eval.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 12 15:56:08 1996                          */
;*    Last change :  Tue Apr 16 11:54:31 1996 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The emition of Eval's stubs                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module translate_eval
   (import engine_param
	   tools_speek)
   (export (translate-eval-declarations)
	   (translate-eval-stubs)
	   (add-eval-function! <symbol> <symbol-list>)))

;*---------------------------------------------------------------------*/
;*    *fun* ...                                                        */
;*---------------------------------------------------------------------*/
(define *fun* '())

;*---------------------------------------------------------------------*/
;*    add-eval-function! ...                                           */
;*---------------------------------------------------------------------*/
(define (add-eval-function! fun-id args-list)
   (if (and (pair? args-list) (not (null? (cdr (last-pair args-list)))))
       (warning "cigloo" "Can't emit eval stub for va-args C function" fun-id)
       (set! *fun* (cons (cons fun-id args-list) *fun*))))

;*---------------------------------------------------------------------*/
;*    translate-eval-declarations ...                                  */
;*---------------------------------------------------------------------*/
(define (translate-eval-declarations)
   (if (pair? *fun*)
       (begin
	  (fprint *oport* " (eval")
	  (for-each (lambda (fun)
		       (fprint *oport* "   (export " (car fun) ")"))
		    *fun*)
	  (fprint *oport* "   )"))))

;*---------------------------------------------------------------------*/
;*    translate-eval-stubs ...                                         */
;*---------------------------------------------------------------------*/
(define (translate-eval-stubs)
   (define (translate-fun-stub fun)
      (let ((fun-id   (car fun))
	    (fun-args (map gensym (cdr fun))))
	 (fprint *oport* `(define ,(cons fun-id fun-args)
			     ((@ ,fun-id foreign) ,@fun-args)))))
   (for-each translate-fun-stub *fun*))
	     
 
