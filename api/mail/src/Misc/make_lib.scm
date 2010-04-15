;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mail/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sat Dec 13 17:42:37 2008 (serrano)                */
;*    Copyright   :  2001-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file and the _e library        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mail_makelib
   
   (import __mail_mailbox
	   __mail_maildir
	   __mail_imap
	   __mail_rfc2045
	   __mail_rfc2047
	   __mail_rfc2822
	   __mail_vcard)
   
   (eval   (export-all)

	   (class mailbox)
	   (class maildir)
	   (class imap)
	   (class vcard)))
