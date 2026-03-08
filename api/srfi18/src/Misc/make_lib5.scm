;*=====================================================================*/
;*    .../project/bigloo/5.0a/api/srfi18/src/Misc/make_lib5.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sun Mar  8 21:09:47 2026 (serrano)                */
;*    Copyright   :  2001-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap5 file.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi18_makelib
   (export :version 4 "../Llib/backend.scm")
   (export :version 4 "../Llib/thread.scm")
   (export :version 4 "../Llib/mutex.scm")
   (export :version 4 "../Llib/condvar.scm"))
