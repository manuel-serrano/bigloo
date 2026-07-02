;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/runtime/Rgc/rgc-generic.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  1 10:13:29 2024                          */
;*    Last change :  Wed Jul  1 21:52:51 2026 (serrano)                */
;*    Copyright   :  2024-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Rgc generic implementation                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export (inline $$rgc-buffer-symbol::symbol ::input-port)
	   (inline $$rgc-buffer-upcase-symbol::symbol ::input-port)
	   (inline $$rgc-buffer-downcase-symbol::symbol ::input-port)
	   (inline $$rgc-buffer-subsymbol::symbol ::input-port ::long ::long)
	   (inline $$rgc-buffer-upcase-subsymbol::symbol ::input-port ::long ::long)
	   (inline $$rgc-buffer-downcase-subsymbol::symbol ::input-port ::long ::long)
	   (inline $$rgc-buffer-keyword::keyword ::input-port)
	   (inline $$rgc-buffer-downcase-keyword::keyword ::input-port)
	   (inline $$rgc-buffer-upcase-keyword::keyword ::input-port)))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-symbol ...                                          */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-symbol ip::input-port)
   (string->symbol (rgc-buffer-substring ip 0 (rgc-buffer-length ip))))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-upcase-symbol ...                                   */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-upcase-symbol ip::input-port)
   (string->symbol
      (string-upcase! (rgc-buffer-substring ip 0 (rgc-buffer-length ip)))))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-downcase-symbol ...                                 */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-downcase-symbol ip::input-port)
   (string->symbol
      (string-downcase! (rgc-buffer-substring ip 0 (rgc-buffer-length ip)))))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-subsymbol ...                                       */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-subsymbol ip::input-port start stop)
   (string->symbol (rgc-buffer-substring ip start stop)))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-uopcase-subsymbol ...                               */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-upcase-subsymbol ip::input-port start stop)
   (string->symbol (string-upcase! (rgc-buffer-substring ip start stop))))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-uopcase-subsymbol ...                               */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-downcase-subsymbol ip::input-port start stop)
   (string->symbol (string-downcase! (rgc-buffer-substring ip start stop))))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-keyword ...                                         */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-keyword ip::input-port)
   (let* ((start ($rgc-matchstart ip))
	  (end (rgc-buffer-length ip))
	  (str (if (=fx ($rgc-buffer-get-char ip start) (char->integer #\:))
		   (rgc-buffer-substring ip 1 end)
		   (rgc-buffer-substring ip 0 (-fx end 1)))))
      (string->keyword str)))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-upcase-keyword ...                                  */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-upcase-keyword ip::input-port)
   (let* ((start ($rgc-matchstart ip))
	  (end (rgc-buffer-length ip))
	  (str (if (=fx ($rgc-buffer-get-char ip start) (char->integer #\:))
		   (rgc-buffer-substring ip 1 end)
		   (rgc-buffer-substring ip 0 (-fx end 1)))))
      (string->keyword (string-upcase! str))))

;*---------------------------------------------------------------------*/
;*    $$rgc-buffer-downcase-keyword ...                                */
;*---------------------------------------------------------------------*/
(define-inline ($$rgc-buffer-downcase-keyword ip::input-port)
   (let* ((start ($rgc-matchstart ip))
	  (end (rgc-buffer-length ip))
	  (str (if (=fx ($rgc-buffer-get-char ip start) (char->integer #\:))
		   (rgc-buffer-substring ip 1 end)
		   (rgc-buffer-substring ip 0 (-fx end 1)))))
      (string->keyword (string-downcase! str))))
   
