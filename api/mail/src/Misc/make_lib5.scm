;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0a/api/mail/src/Misc/make_lib5.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Mar 10 14:28:37 2026 (serrano)                */
;*    Copyright   :  2001-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file and the _e library        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mail
   (export :version 4 "../Llib/mailbox.scm")
   (export :version 4 "../Llib/maildir.scm")
   (export :version 4 "../Llib/imap.bgl" "../Llib/imap.stk")
   (export :version 4 "../Llib/rfc2045.scm")
   (export :version 4 "../Llib/rfc2047.scm")
   (export :version 4 "../Llib/rfc2822.scm")
   (export :version 4 "../Llib/vcard.scm"))
