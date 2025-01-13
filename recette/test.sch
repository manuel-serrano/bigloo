;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/wasm/recette/test.sch                */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov  2 17:21:16 1992                          */
;*    Last change :  Mon Jan 13 07:44:29 2025 (serrano)                */
;*                                                                     */
;*    La macro qui fait un appel a la fonction de test                 */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define-expander test
   (lambda (x e)
      (match-case x
	 ((?- ?name ?expr ?res)
	  (e `(do-test ,name (lambda () ,expr) ,res) e))
	 (else
	  (error "recette" "Illegal test" x)))))
	     
;*---------------------------------------------------------------------*/
;*    when-call/cc ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (when-call/cc . body)
   (if (memq *target-language* '(jvm wasm))
       #unspecified
       `(begin ,@body)))


