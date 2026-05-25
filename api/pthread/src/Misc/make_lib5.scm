;*=====================================================================*/
;*    .../project/bigloo/5.0.x/api/pthread/src/Misc/make_lib5.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Feb  6 08:33:40 2026                          */
;*    Last change :  Mon May 25 08:21:41 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 heap file.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module pthread
   (import :version 4 "../Llib/pthread.scm")
   (export :version 4 "../Llib/pcondvar.scm")
   (export :version 4 "../Llib/pmutex.scm")
   (export :version 4 "../Llib/psemaphore.scm")
   (export :version 4 "../Llib/pthread.scm"))

;*---------------------------------------------------------------------*/
;*    Initialization                                                   */
;*---------------------------------------------------------------------*/
(pthread-setup-backend!)
