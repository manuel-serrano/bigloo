;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Translate/ident.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 30 09:03:10 2007                          */
;*    Last change :  Sun Dec 30 09:13:24 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Idents conversion                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module translate_ident
   (import engine_param)
   (export (ident->ident x)))

;*---------------------------------------------------------------------*/
;*    ident->ident ...                                                 */
;*---------------------------------------------------------------------*/
(define (ident->ident id)
   (case *ident-style*
      ((scheme)
       (ident->scheme-ident id))
      (else
       id)))

;*---------------------------------------------------------------------*/
;*    ident->scheme-ident ...                                          */
;*---------------------------------------------------------------------*/
(define (ident->scheme-ident s)
   (let* ((s1 (if (substring-at? s "bgl_" 0)
		  (string-append "$" (substring s 4 (string-length s)))
		  (string-append "$" s))))
      (string-replace! s1 #\_ #\-)))
