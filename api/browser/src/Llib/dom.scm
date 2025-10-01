;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/api/browser/src/Llib/dom.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Sep 11 08:10:30 2025                          */
;*    Last change :  Wed Oct  1 15:52:52 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Bigloo DOM binding for the wasm backend.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __browser_dom

   (cond-expand
      (bigloo-c
       (extern
	  (type $Element void* "void *")))
      (bigloo-wasm
       (extern
	  (type $Element void* "void *")
	  ($get-element-by-id::$Element (::bstring) "bgl_get_element_by_id")
	  ($inner-html-set!::obj (::$Element ::bstring) "bgl_inner_html_set")
	  ($inner-html::bstring (::$Element) "bgl_inner_html_get"))))

   (export
      (class Element
	 ($builtin::$Element read-only))
      (get-element-by-id::obj ::bstring)
      (generic element-inner-html ::Element)
      (generic element-inner-html-set! ::Element html::bstring)))
   
;*---------------------------------------------------------------------*/
;*    get-element-by-id ...                                            */
;*---------------------------------------------------------------------*/
(define (get-element-by-id id::bstring)
   (cond-expand
      (bigloo-c
       #unspecified)
      (else
       (let ((e ($get-element-by-id id)))
	  (if (void*-null? e)
	      #unspecified
	      (instantiate::Element
		 ($builtin e)))))))

;*---------------------------------------------------------------------*/
;*    element-inner-html ::Element ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (element-inner-html o::Element)
   (cond-expand
      (bigloo-c
       #unspecified)
      (else
       (with-access::Element o ($builtin)
	  (flush-output-port (current-error-port))
	  ($inner-html $builtin)))))
       
;*---------------------------------------------------------------------*/
;*    element-inner-html-set! ::Element ...                            */
;*---------------------------------------------------------------------*/
(define-generic (element-inner-html-set! o::Element html)
   (cond-expand
      (bigloo-c
       #unspecified)
      (else
       (with-access::Element o ($builtin)
	  ($inner-html-set! $builtin html)))))
       
