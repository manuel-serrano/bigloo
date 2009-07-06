;*=====================================================================*/
;*    serrano/prgm/project/ude/dbg/dbg-docline.el                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  1 07:58:46 1998                          */
;*    Last change :  Sun Nov 15 21:34:42 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Dbg online documentation. This file uses `xinfo' for         */
;*    displaying Info files.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-docline)
(require 'dbg-config)
(require 'ude-autoload)
(require 'info)

;*---------------------------------------------------------------------*/
;*    dbg-pop-to-info ...                                              */
;*    -------------------------------------------------------------    */
;*    Pop up a buffer displaying an info file.                         */
;*---------------------------------------------------------------------*/
(defun dbg-pop-to-info ()
  (let ((pop-up-frames t))
    (pop-to-buffer "*info*")))

;*---------------------------------------------------------------------*/
;*    dbg-info-ref-internal ...                                        */
;*    -------------------------------------------------------------    */
;*    Search in the info documentation for a function (or variable)    */
;*    definition.                                                      */
;*---------------------------------------------------------------------*/
(defun dbg-info-ref-internal (ident)
  (save-window-excursion
    (info)
    (Info-find-node dbg-info-file "Top")
    (if (not (string= ident ""))
	(Info-index ident)))
  (dbg-pop-to-info))

;*---------------------------------------------------------------------*/
;*    dbg-fetch-then-request-identifier ...                            */
;*---------------------------------------------------------------------*/
(defun dbg-fetch-then-request-identifier (pos prompt)
  (let ((char (char-after (point)))
	(word (current-word)))
    (if (or (not char)
	    (eq char ?\ )
	    (string= word ""))
	(read-string prompt)
      word)))

;*---------------------------------------------------------------------*/
;*    dbg-docline ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function calls an online documentation with respect         */
;*    to the active context.                                           */
;*    -------------------------------------------------------------    */
;*    If a region is active, this function print the documentation     */
;*    for the region.                                                  */
;*---------------------------------------------------------------------*/
(defun dbg-docline ()
  "Popup an online documentation according to context."
  (interactive)
  (dbg-info-ref-internal (ude-fetch-then-request-identifier (point)
							    "help for: ")))
	 
	    
