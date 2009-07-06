;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb1/Bdb/Tools/date.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 07:24:41 1999                          */
;*    Last change :  Tue Jul 27 08:23:19 1999 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The compilation date for bdb                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_date
   (export (bdb-date)))

;*---------------------------------------------------------------------*/
;*    This macro builds the expansion date for the macro itself (i.e.  */
;*    when does this macro been expanded).                             */
;*---------------------------------------------------------------------*/
(define-macro (get-expansion-date)
   (let ((port (open-input-file "| date")))
      (let ((date (read-line port)))
	 (close-input-port port)
	 date)))

;*---------------------------------------------------------------------*/
;*    bdb-date ...                                                     */
;*---------------------------------------------------------------------*/
(define (bdb-date)
   (get-expansion-date))
