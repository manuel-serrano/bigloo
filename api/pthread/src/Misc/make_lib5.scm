;*=====================================================================*/
;*    .../project/bigloo/5.0a/api/pthread/src/Misc/make_lib5.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Feb  6 08:33:40 2026                          */
;*    Last change :  Fri Feb  6 08:35:13 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 heap file.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module pthread
   (export :version 4 "../Llib/pbackend.scm")
   (export :version 4 "../Llib/pcondvar.scm")
   (export :version 4 "../Llib/pmutex.scm")
   (export :version 4 "../Llib/psemaphore.scm")
   (export :version 4 "../Llib/pthread.scm"))
