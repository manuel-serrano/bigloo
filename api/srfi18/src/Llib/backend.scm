;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0a/api/srfi18/src/Llib/backend.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 24 06:42:48 2008                          */
;*    Last change :  Sun Mar  8 21:02:06 2026 (serrano)                */
;*    Copyright   :  2008-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Posix thread backend                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi18_backend

   (library pthread)
   
   (include "srfi18.sch")
   
   (import __srfi18_thread)
   
   (static (class srfi18-backend::thread-backend))
   
   (export (srfi18-setup-backend!)
	   (get-srfi18-backend)))

;*---------------------------------------------------------------------*/
;*    Initialization                                                   */
;*---------------------------------------------------------------------*/
(srfi18-setup-backend!)
