;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mail/src/Llib/mailbox.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  4 18:36:04 2007                          */
;*    Last change :  Fri Dec 13 12:04:51 2013 (serrano)                */
;*    Copyright   :  2007-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The generic OO api for mailboxes (e.g., imap or maildir).        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mail_mailbox
   
   (option (set! *dlopen-init-gc* #t))
   
   (export (class &mailbox-error::&error)
	   
	   (abstract-class mailbox
	      (%mutex::mutex read-only (default (make-mutex)))
	      (%selection-cache::obj (default #f))
	      (folder-selection::obj (default #f))
	      (label::bstring read-only (default "")))
	   
	   (generic mailbox-close ::mailbox)
	   (generic mailbox-separator::bstring ::mailbox)
	   (generic mailbox-prefix ::mailbox)
	   (generic mailbox-hostname ::mailbox)
	   
	   (generic mailbox-folders::pair-nil ::mailbox)
	   (generic mailbox-folder-select! ::mailbox ::bstring)
	   (generic mailbox-folder-unselect! ::mailbox)
	   (generic mailbox-folder-create! ::mailbox ::bstring)
	   (generic mailbox-folder-delete! ::mailbox ::bstring)
	   (generic mailbox-folder-rename! ::mailbox ::bstring ::bstring)
	   (generic mailbox-folder-move! ::mailbox ::bstring ::bstring)
	   (generic mailbox-folder-subscribe! ::mailbox ::bstring)
	   (generic mailbox-folder-unsubscribe! ::mailbox ::bstring)
	   (generic mailbox-folder-exists?::bool ::mailbox ::bstring)
	   (generic mailbox-folder-status::obj ::mailbox ::bstring)
	   (generic mailbox-folder-poll ::mailbox)
	   (generic mailbox-folder-expunge! ::mailbox)
	   (generic mailbox-folder-search ::mailbox request)
	   (generic mailbox-folder-search-header ::mailbox header value)
	   (generic mailbox-folder-search-keyword ::mailbox keyword)
	   (generic mailbox-folder-uids::pair-nil ::mailbox)
	   (generic mailbox-folder-dates::pair-nil ::mailbox)
	   (generic mailbox-folder-delete-messages! ::mailbox)
	   
	   (generic mailbox-folder-messages::pair-nil ::mailbox)
	   (generic mailbox-folder-headers::pair-nil ::mailbox)
	   (generic mailbox-folder-bodies::pair-nil ::mailbox)
	   (generic mailbox-folder-sizes::pair-nil ::mailbox)
	   (generic mailbox-folder-flags::pair-nil ::mailbox)
	   (generic mailbox-folder-infos::pair-nil ::mailbox)
	   (generic mailbox-folder-header-fields::pair-nil ::mailbox ::bstring)
	   
	   (generic mailbox-message ::mailbox ::int)
	   (generic mailbox-message-path ::mailbox ::int)
	   (generic mailbox-message-body::bstring ::mailbox ::int . len)
	   (generic mailbox-message-header::bstring ::mailbox ::int)
	   (generic mailbox-message-header-list::pair-nil ::mailbox ::int)
	   (generic mailbox-message-header-field::bstring ::mailbox ::int ::bstring)
	   (generic mailbox-message-size::int ::mailbox ::int)
	   (generic mailbox-message-info ::mailbox ::int)
	   (generic mailbox-message-flags::pair-nil ::mailbox ::int)
	   (generic mailbox-message-flags-set! ::mailbox ::int ::pair-nil)
	   (generic mailbox-message-delete! ::mailbox ::int)
	   (generic mailbox-message-move! ::mailbox ::int ::bstring)
	   (generic mailbox-message-create! ::mailbox ::bstring ::bstring)
	   (generic mailbox-message-copy! ::mailbox ::int ::bstring)))

;*---------------------------------------------------------------------*/
;*    generic declarations                                             */
;*---------------------------------------------------------------------*/
(define-generic (mailbox-close m::mailbox)
   #unspecified)
(define-generic (mailbox-prefix m::mailbox)
   "")
(define-generic (mailbox-separator::bstring m::mailbox)
   ".")
(define-generic (mailbox-hostname m::mailbox)
   "")

(define-generic (mailbox-folders::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-select! m::mailbox s::bstring)
   '())
(define-generic (mailbox-folder-unselect! m::mailbox)
   #unspecified)

(define-generic (mailbox-folder-create! m::mailbox s::bstring)
   #unspecified)
(define-generic (mailbox-folder-delete! m::mailbox s::bstring)
   #unspecified)
(define-generic (mailbox-folder-rename! m::mailbox s1::bstring s2::bstring)
   #unspecified)

(define-generic (mailbox-folder-move! m::mailbox folder::bstring to::bstring)
   ;; it is possible here to define a generic definition for folder-move some
   ;; implementation (such as maildir) may support a more efficient version
   
   (define (is-subfolder? folder parent)
      ;; this predicate returns #t iff folder is a subfolder of parent
      (let ((lenf (string-length folder))
	    (lenp (string-length parent)))
	 (and (>fx lenf lenp)
	      (substring-at? folder parent 0)
	      (string-contains folder (mailbox-separator m) lenp))))

   (define (folder-move! m s1 dest)
      ;; create the new folder
      (let ((folder (mailbox-folder-create! m dest)))
	 ;; copy all the messsages
	 (mailbox-folder-select! m s1)
	 (for-each (lambda (uid)
		      (mailbox-message-move! m uid dest))
		   (mailbox-folder-uids m))
	 ;; delete the folder
	 (mailbox-folder-delete! m s1)))
   
   (let ((i (string-index-right folder (string-ref (mailbox-separator m) 0))))
      (if (and i (string-contains folder (mailbox-separator m) i))
	  (let* ((len1 (string-length folder))
		 (base (substring folder i len1))
		 (dest (string-append to base)))
	     (folder-move! m folder dest)
	     (for-each (lambda (f)
			  (when (is-subfolder? f folder)
			     (let* ((base (substring f i (string-length f)))
				    (dest (string-append to base)))
				(folder-move! m f dest))))
		       (mailbox-folders m))
	     (mailbox-folder-select! m to))
	  (raise
	   (instantiate::&mailbox-error
	      (proc 'mailbox-folder-move!)
	      (msg (format "Illegal folder name ~s " folder))
	      (obj m))))))

(define-generic (mailbox-folder-subscribe! m::mailbox s::bstring)
   #unspecified)
(define-generic (mailbox-folder-unsubscribe! m::mailbox s::bstring)
   #unspecified)
(define-generic (mailbox-folder-exists?::bool m::mailbox s::bstring)
   #f)
(define-generic (mailbox-folder-status::obj m::mailbox s::bstring)
   #f)
(define-generic (mailbox-folder-poll m::mailbox)
   #unspecified)
(define-generic (mailbox-folder-expunge! m::mailbox)
   #unspecified)
(define-generic (mailbox-folder-search m::mailbox request)
   #unspecified)
(define-generic (mailbox-folder-search-header m::mailbox header value)
   #unspecified)
(define-generic (mailbox-folder-search-keyword m::mailbox keyword)
   #unspecified)
(define-generic (mailbox-folder-uids::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-dates::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-delete-messages! m::mailbox)
   #unspecified)

(define-generic (mailbox-folder-messages::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-headers::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-bodies::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-sizes::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-flags::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-infos::pair-nil m::mailbox)
   '())
(define-generic (mailbox-folder-header-fields::pair-nil m::mailbox s::bstring)
   '())

(define-generic (mailbox-message m::mailbox i::int)
   #unspecified)
(define-generic (mailbox-message-path m::mailbox i::int)
   #f)
(define-generic (mailbox-message-body::bstring m::mailbox i::int . len)
   "")
(define-generic (mailbox-message-header::bstring m::mailbox i::int)
   "")
(define-generic (mailbox-message-header-list::pair-nil m::mailbox i::int)
   '())
(define-generic (mailbox-message-header-field::bstring m::mailbox i::int s::bstring)
   "")
(define-generic (mailbox-message-size::int m::mailbox i::int)
   -1)
(define-generic (mailbox-message-info m::mailbox i::int)
   #unspecified)
(define-generic (mailbox-message-flags::pair-nil m::mailbox i::int)
   '())
(define-generic (mailbox-message-flags-set! m::mailbox i::int o::pair-nil)
   #unspecified)
(define-generic (mailbox-message-delete! m::mailbox i::int)
   #unspecified)
(define-generic (mailbox-message-copy! m::mailbox i::int s::bstring)
   #unspecified)
(define-generic (mailbox-message-move! m::mailbox i::int s::bstring)
   #unspecified)
(define-generic (mailbox-message-create! m::mailbox s1::bstring s2::bstring)
   #unspecified)
