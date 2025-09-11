;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/api/dom/src/Llib/dom.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Sep 11 08:10:30 2025                          */
;*    Last change :  Thu Sep 11 08:36:11 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Bigloo DOM binding for the wasm backend.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __dom_dom
   
   (extern
      (type $Element void* "void *")
      ($get-element-by-id::$Element (::bstring) "bgl_get_element_by_id"))
   
   (export
      (class Element
	 (id::bstring read-only))
      (get-element-by-id::obj ::bstring))
   
   (static
      (class %Element::Element
	 ($builtin::$Element read-only))))

;*---------------------------------------------------------------------*/
;*    get-element-by-id ...                                            */
;*---------------------------------------------------------------------*/
(define (get-element-by-id id::bstring)
   (let ((e ($get-element-by-id id)))
      (if (void*-null? e)
	  #unspecified
	  (instantiate::%Element
	     (id id)
	     ($builtin e)))))
