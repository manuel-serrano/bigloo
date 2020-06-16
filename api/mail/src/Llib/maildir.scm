;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mail/src/Llib/maildir.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  4 18:40:47 2007                          */
;*    Last change :  Fri Nov 17 08:47:15 2017 (serrano)                */
;*    Copyright   :  2007-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bigloo maildir implementation.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mail_maildir
   
   (import __mail_mailbox
	   __mail_rfc2822)
   
   (export (class &maildir-error::&mailbox-error)
	   
	   (class maildir::mailbox
	      (%separator::char read-only (default #\.))
	      (%separator-cache::obj (default #f))
	      (%folders read-only (default (make-hashtable)))
	      (%selection::bstring (default ""))
	      (%selection-info (default #unspecified))
	      (message-base::bstring (default (hostname)))
	      (prefix::bstring read-only (default "INBOX"))
	      (path::bstring read-only)))
   
   (static (class folderinfo
	      time::elong
	      (path::bstring read-only)
	      uidvalidity::int
	      (uids read-only)
	      (nextuid::int (default 1))
	      (count::int (default 0))
	      (recent::int (default 0)))))

;*---------------------------------------------------------------------*/
;*    object-print ::maildir ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-print o::maildir p print-slot)
   (with-access::maildir o (path prefix folder-selection)
      (display "#<maildir path=" p)
      (print-slot path p)
      (display " prefix=" p)
      (print-slot prefix p)
      (display " folder-selection=" p)
      (print-slot folder-selection p)
      (display ">" p)))

;*---------------------------------------------------------------------*/
;*    mailbox-close ::maildir ...                                      */
;*---------------------------------------------------------------------*/
(define-method (mailbox-close m::maildir)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    mailbox-separator ::maildir ...                                  */
;*---------------------------------------------------------------------*/
(define-method (mailbox-separator m::maildir)
   (with-access::maildir m (%separator %separator-cache)
      (unless (string? %separator-cache)
	 (set! %separator-cache (string %separator)))
      %separator-cache))

;*---------------------------------------------------------------------*/
;*    mailbox-hostname ::maildir ...                                   */
;*---------------------------------------------------------------------*/
(define-method (mailbox-hostname m::maildir)
   (with-access::maildir m (path)
      path))

;*---------------------------------------------------------------------*/
;*    mailbox-prefix ::maildir ...                                     */
;*---------------------------------------------------------------------*/
(define-method (mailbox-prefix m::maildir)
   (with-access::maildir m (prefix)
      prefix))

;*---------------------------------------------------------------------*/
;*    mailbox-folders ::maildir ...                                    */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folders m::maildir)
   (with-access::maildir m (%separator prefix path)
      (cons prefix
	    (sort string<?
		  (filter-map (lambda (f)
				 (when (and (char=? (string-ref f 0) %separator)
					    (directory? (make-file-name path f)))
				    (string-append prefix f)))
			      (directory->list path))))))

;*---------------------------------------------------------------------*/
;*    folder->directory ...                                            */
;*---------------------------------------------------------------------*/
(define (folder->directory proc m::maildir s::bstring)
   (with-access::maildir m (path prefix %separator)
      (let ((ls (string-length s))
	    (lp (string-length prefix)))
	 (cond
	    ((or (<fx ls lp)
		 (not (substring-at? s prefix 0))
		 (and (>fx ls lp) (not (char=? (string-ref s lp) %separator))))
	     (raise
	      (instantiate::&maildir-error
		 (proc proc)
		 (msg (format "Folder ~s should be prefixed with ~s" s prefix))
		 (obj m))))
	    ((=fx lp ls)
	     path)
	    (else
	     (make-file-name path (substring s lp ls)))))))
   
;*---------------------------------------------------------------------*/
;*    mailbox-folder-select! ::maildir ...                             */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-select! m::maildir s::bstring)
   (with-access::maildir m (%mutex folder-selection %selection %selection-info)
      (synchronize %mutex
	 (if (and (string? folder-selection)
		  (string=? folder-selection s)
		  (isa? %selection-info folderinfo))
	     (with-access::folderinfo %selection-info (count recent)
		`((count . ,count) (recent . ,recent)))
	     (let ((path (folder->directory
			    "mailbox-folder-select ::maildir" m s)))
		(set! %selection path)
		(set! %selection-info (get-folder-info m path))
		(unless (isa? %selection-info folderinfo)
		   (error "mailbox-folder-select!"
		      (format "Illegal folder ~s" s)
		      path))
		(set! folder-selection s)
		(with-access::folderinfo %selection-info (count recent)
		   `((count . ,count) (recent . ,recent))))))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-unselect! ::maildir ...                           */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-unselect! m::maildir)
   (with-access::maildir m (%mutex folder-selection %selection %selection-info)
      (synchronize %mutex
	 (set! %selection "")
	 (set! %selection-info #unspecified)
	 (set! folder-selection #f))))
   
;*---------------------------------------------------------------------*/
;*    mailbox-folder-create! ::maildir ...                             */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-create! m::maildir s::bstring)
   (let ((path (folder->directory "mailbox-folder-create! ::maildir" m s)))
      (cond
	 ((directory? path)
	  (raise
	   (instantiate::&maildir-error
	      (proc "mailbox-folder-create! ::maildir")
	      (msg (format "Folder ~s already exists" s))
	      (obj m))))
	 ((and (make-directory path)
	       (make-directory (make-file-name path "cur"))
	       (make-directory (make-file-name path "tmp"))
	       (make-directory (make-file-name path "new")))
	  #t)
	 (else
	  (raise
	   (instantiate::&maildir-error
	      (proc "mailbox-folder-create! ::maildir")
	      (msg (format "Cannot create folder ~s" s))
	      (obj m)))))))

;*---------------------------------------------------------------------*/
;*    is-subfolder? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-subfolder? m folder parent)
   ;; this predicate returns #t iff folder is a subfolder of parent
   (let ((lenf (string-length folder))
	 (lenp (string-length parent)))
      (and (>fx lenf lenp)
	   (substring-at? folder parent 0)
	   (with-access::maildir m (%separator)
	      (char=? (string-ref folder lenp) %separator)))))

;*---------------------------------------------------------------------*/
;*    is-direct-subfolder? ...                                         */
;*---------------------------------------------------------------------*/
(define (is-direct-subfolder? m folder parent)
   ;; this predicate returns #t iff folder is a direct subfolder of parent
   (let ((lenf (string-length folder))
	 (lenp (string-length parent)))
      (and (>fx lenf lenp)
	   (substring-at? folder parent 0)
	   (with-access::maildir m (%separator)
	      (=fx (string-index-right folder %separator) lenp)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-delete! ::maildir ...                             */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-delete! m::maildir s::bstring)
   (with-access::maildir m (path)
      (let* ((path (folder->directory "mailbox-folder-delete! ::maildir" m s))
	     (cur (make-file-name path "cur"))
	     (new (make-file-name path "new"))
	     (tmp (make-file-name path "tmp")))
	 (cond
	    ((not (directory? path))
	     ;; folder does not exist
	     (raise (instantiate::&maildir-error
		       (proc "mailbox-folder-delete! ::maildir")
		       (msg (format "Folder ~s does not exist" s))
		       (obj m))))
	    ((or (pair? (directory->list cur))
		 (pair? (directory->list new))
		 (pair? (directory->list tmp)))
	     ;; one of cur, new, and tmp is not empty
	     (raise (instantiate::&maildir-error
		       (proc "mailbox-folder-delete! ::maildir")
		       (msg (format "Folder ~s not empty" s))
		       (obj m))))
	    (else
	     ;; delete the subfolders
	     (for-each (lambda (f)
			  (when (is-direct-subfolder? m f s)
			     (mailbox-folder-delete! m f)))
		       (mailbox-folders m))
	     ;; delete the files
	     (let loop ((path path))
		(when (file-exists? path)
		   (if (directory? path)
		       (let ((files (directory->list path)))
			  (for-each (lambda (f)
				       (loop (make-file-name path f)))
				    files)
			  (delete-directory path))
		       (begin
			  (delete-file path)))))
	     #t)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-rename! ::maildir ...                             */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-rename! m::maildir s1::bstring s2::bstring)
   (let ((opath (folder->directory "mailbox-folder-rename! ::maildir"
				   m
				   s1))
	 (npath (folder->directory "mailbox-folder-rename! ::maildir"
				   m
				   s2)))
      (let ((l (string-length s1)))
	 (if (not (rename-file opath npath))
	     (raise
	      (instantiate::&maildir-error
		 (proc "mailbox-folder-rename! ::maildir")
		 (msg (format "Folder ~s cannot be renamed into ~s" s1 s2))
		 (obj m))))
	 (for-each (lambda (f)
		      (when (is-direct-subfolder? m f s1)
			 (let* ((base (substring f l (string-length f)))
				(dest (string-append s2 base)))
			    (mailbox-folder-rename! m f dest))))
		   (mailbox-folders m)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-move! ::maildir ...                               */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-move! m::maildir s1::bstring s2::bstring)
   (with-access::maildir m (%separator)
      (let ((i (string-index-right s1 %separator)))
	 (if i
	     (let* ((len1 (string-length s1))
		    (base (substring s1 i len1))
		    (dest (string-append s2 base)))
		(mailbox-folder-rename! m s1 dest)
		(for-each (lambda (f)
			     (when (is-subfolder? m f s1)
				(let* ((base (substring f i (string-length f)))
				       (dest (string-append s2 base)))
				   (mailbox-folder-rename! m f dest))))
		   (mailbox-folders m)))
	     (raise
		(instantiate::&maildir-error
		   (proc "mailbox-folder-move! ::maildir")
		   (msg (format "Illegal folder name ~s " s1))
		   (obj m)))))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-subscribe! ::maildir ...                          */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-subscribe! m::maildir s::bstring)
   #unspecified)
   
;*---------------------------------------------------------------------*/
;*    mailbox-folder-unsubscribe! ::maildir ...                        */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-unsubscribe! m::maildir s::bstring)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    mailbox-folder-exists? ::maildir ...                             */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-exists? m::maildir s::bstring)
   (let ((path (folder->directory "mailbox-folder-exists? ::maildir" m s)))
      (directory? path)))

;*---------------------------------------------------------------------*/
;*    get-cached-folder-info ...                                       */
;*---------------------------------------------------------------------*/
(define (get-cached-folder-info m::maildir s errmsg)
   (with-access::maildir m (%mutex folder-selection %selection-info)
      (if (and (string? folder-selection) (string=? folder-selection s))
	  (begin
	     (unless (folderinfo-valid? %selection-info)
		(let ((dir (folder->directory errmsg m s)))
		   (set! %selection-info (get-folder-info m dir))))
	     %selection-info)
	  (let ((dir (folder->directory errmsg m s)))
	     (get-folder-info m dir)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-status ::maildir ...                              */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-status m::maildir s::bstring)
   (with-access::maildir m (%mutex)
      (synchronize %mutex
	 (let ((info (get-cached-folder-info
			m s "mailbox-folder-status ::maildir")))
	    (when (isa? info folderinfo)
	       (with-access::folderinfo info (uids uidvalidity nextuid)
		  (let ((messages (hashtable-size uids))
			(recent 0)
			(unseen 0)
			(deleted 0))
		     ;; count the unseen messages
		     (hashtable-for-each
			uids
			(lambda (k f)
			   (let ((s (string-index-right f #\,)))
			      (unless (string-index f #\S s)
				 (set! unseen (+fx 1 unseen)))
			      (when (string-index f #\D s)
				 (set! deleted (+fx 1 deleted))))))
		     `((unseen . ,unseen)
		       (uidvalidity . ,uidvalidity)
		       (uidnext . ,nextuid)
		       (recent . ,recent)
		       (messages . ,messages)
		       (deleted . ,deleted)))))))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-expunge! ...                                      */
;*---------------------------------------------------------------------*/
(define (mailbox-folder-expunge! m::maildir s::bstring) 
   (with-access::maildir m (%mutex)
      (synchronize %mutex
	 (let ((info (get-cached-folder-info
			m s "mailbox-folder-status ::maildir")))
	    (when (isa? info folderinfo)
	       (with-access::folderinfo info (uids)
		  (hashtable-for-each
		     uids
		     (lambda (k f)
			(let ((s (string-index-right f #\,)))
			   (when (string-index f #\D s)
			      (let ((path (get-message-path "mailbox-folder-expunge! ::maildir" m k)))
				 (delete-file path)))))))
	       (invalidate-folderinfo! info)
	       #unspecified)))))

;*---------------------------------------------------------------------*/
;*    folder-info-valid? ...                                           */
;*---------------------------------------------------------------------*/
(define (folderinfo-valid? info)
   (with-access::folderinfo info (time path)
      (=elong (file-modification-time path) time)))

;*---------------------------------------------------------------------*/
;*    make-folder-uidtable ...                                         */
;*---------------------------------------------------------------------*/
(define (make-folder-uidtable folder cur)
   (let ((path (make-file-name folder "bigloomail-uidlist"))
	 (table (make-hashtable 50 10 =fx (lambda (x) x)))
	 (taux (make-hashtable 50 10 string=?))
	 (files (filter! (lambda (f) (string-index f ","))
			 (directory->list cur)))
	 (uid 1))
      ;; mark all the existing files
      (for-each (lambda (f)
		   (let ((i (string-index f ",")))
		      (hashtable-put! taux (substring f 0 i) f)))
		files)
      (when (file-exists? path)
	 (with-handler
	    (lambda (e)
	       ;; the file is corrupted, let's reset it
	       (delete-file path))
	    ;; collect the old values
	    (for-each (lambda (e)
			 (let ((u (car e))
			       (f (cdr e)))
			    (when (>=fx u uid) (set! uid u))
			    (let ((i (string-index f ",")))
			       (when i
				  (let* ((key (substring f 0 i))
					 (cur (hashtable-get taux key)))
				     (when cur
					(hashtable-remove! taux key)
					(hashtable-put! table u cur)))))))
		      (with-input-from-file path read))))
      ;; collect the new ones
      (for-each (lambda (f)
		   (let* ((i (string-index f ","))
			  (key (substring f 0 i)))
		      (when (hashtable-get taux key)
			 (set! uid (+fx 1 uid))
			 (hashtable-put! table uid f))))
		files)
      ;; write the new table
      (with-output-to-file path (lambda () (write (hashtable-map table cons))))
      ;; we are done
      (values table (+fx uid 1))))

;*---------------------------------------------------------------------*/
;*    invalidate-folderinfo! ...                                       */
;*---------------------------------------------------------------------*/
(define (invalidate-folderinfo! finfo::folderinfo)
   (with-access::folderinfo finfo (time uidvalidity)
      (set! time #e-1)
      (set! uidvalidity (+fx 1 uidvalidity))))

;*---------------------------------------------------------------------*/
;*    update-folderinfo! ...                                           */
;*    -------------------------------------------------------------    */
;*    The maildir lock must be acquired before calling this update     */
;*    function.                                                        */
;*---------------------------------------------------------------------*/
(define (update-folderinfo! folder info::folderinfo uid::int npath)
   (with-access::folderinfo info (time path uids)
      ;; update the validity time stamp
      (set! time (file-modification-time path))
      ;; update the hashtable
      (if (string? npath)
	  (hashtable-update! uids uid (lambda (x) npath) npath)
	  (hashtable-remove! uids uid))
      ;; write the new table
      (with-output-to-file (make-file-name folder "bigloomail-uidlist")
	 (lambda () (write (hashtable-map uids cons))))))

;*---------------------------------------------------------------------*/
;*    get-folder-info ...                                              */
;*    -------------------------------------------------------------    */
;*    The maildir lock must be acquired before calling this update     */
;*    function.                                                        */
;*---------------------------------------------------------------------*/
(define (get-folder-info::obj m::maildir folder::bstring)
   (with-access::maildir m (%folders %mutex)
      (let ((oinfo (hashtable-get %folders folder)))
	 (if (and (isa? oinfo folderinfo) (folderinfo-valid? oinfo))
	     oinfo
	     (let ((pcur (make-file-name folder "cur"))
		   (pnew (make-file-name folder "new")))
		(when (directory? pcur)
		   (multiple-value-bind (uids nextuid)
		      (make-folder-uidtable folder pcur)
		      (let* ((newc (length (directory->list pnew)))
			     (curc (length (directory->list pcur)))
			     (uidv (if oinfo
				       (with-access::folderinfo oinfo (uidvalidity)
					  (+fx 1 uidvalidity))
				       (elong->fixnum (current-seconds))))
			     (ninfo (instantiate::folderinfo
				       (time (file-modification-time pcur))
				       (path pcur)
				       (uidvalidity uidv)
				       (uids uids)
				       (nextuid nextuid)
				       (count (+fx curc newc))
				       (recent newc))))
			 (hashtable-update! %folders folder
					    (lambda (i) ninfo)
					    ninfo)
			 ninfo))))))))

;*---------------------------------------------------------------------*/
;*    get-nextuid! ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-nextuid! info)
   (with-access::folderinfo info (nextuid)
      (let ((uid nextuid))
	 (set! nextuid (+fx 1 nextuid))
	 uid)))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-uids ::maildir ...                                */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-uids m::maildir)
   (with-access::maildir m (%selection %selection-info %mutex)
      (synchronize %mutex
	 (unless (isa? %selection-info folderinfo)
	    (raise (instantiate::&maildir-error
		      (proc "mailbox-folder-uids ::maildir")
		      (msg "No folder selected")
		      (obj m))))
	 (unless (folderinfo-valid? %selection-info)
	    (set! %selection-info (get-folder-info m %selection)))
	 (with-access::folderinfo %selection-info (uids)
	    (hashtable-map uids (lambda (k m) k))))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-dates ::maildir ...                               */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-dates m::maildir)
   (map (lambda (d)
	   (if (string? (cdr d))
	       (set-cdr! d (with-handler
			      (lambda (e)
				 (current-date))
			      (rfc2822-date->date (cdr d))))
	       (set-cdr! d (current-date)))
	   d)
	(mailbox-folder-header-fields m "date")))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-delete-messages! ...                              */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-delete-messages! m::maildir)
   (with-access::maildir m (%selection %selection-info %mutex)
      (synchronize %mutex
	 (unless (isa? %selection-info folderinfo)
	    (raise (instantiate::&maildir-error
		      (proc "mailbox-folder-delete-messages ::maildir")
		      (msg "No folder selected")
		      (obj m))))
	 (let ((cur (make-file-name %selection "cur")))
	    (for-each (lambda (f)
			 (delete-file (make-file-name cur f)))
	       (directory->list cur))
	    (invalidate-folderinfo! %selection-info)
	    (set! %selection-info (get-folder-info m %selection))
	    #unspecified))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-map ...                                           */
;*---------------------------------------------------------------------*/
(define (mailbox-folder-map m::maildir proc)
   (with-access::maildir m (%mutex %selection %selection-info)
      (synchronize %mutex
	 (let ((info (get-folder-info m %selection)))
	    (if (isa? info folderinfo)
		(with-access::folderinfo info (uids)
		   (hashtable-map uids proc))
		'())))))
   
;*---------------------------------------------------------------------*/
;*    mailbox-folder-messages ::maildir ...                            */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-messages m::maildir)
   (mailbox-folder-map m (lambda (k uid) (list k (mailbox-message m k)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-headers ::maildir ...                             */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-headers m::maildir)
   (mailbox-folder-map m (lambda (k uid) (cons k (mailbox-message-header m k)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-bodies ::maildir ...                              */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-bodies m::maildir)
   (mailbox-folder-map m (lambda (k uid) (cons k (mailbox-message-body m k)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-sizes ::maildir ...                               */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-sizes m::maildir)
   (mailbox-folder-map m (lambda (k uid) (cons k (mailbox-message-size m k)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-flags ::maildir ...                               */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-flags m::maildir)
   (mailbox-folder-map m (lambda (k uid) (cons k (mailbox-message-flags m k)))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-infos ::maildir ...                               */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-infos m::maildir)
   
   (define (get-info m i)
      (let* ((header (mailbox-message-header-list m i))
	     (hid (assq 'message-id header))
	     (hdate (assq 'date header)))
	 (list i
	       (cons 'message-id (if (pair? hid) (cdr hid) #f))
	       (cons 'date (if (pair? hdate) (cdr hdate) #f))
	       (cons 'size (mailbox-message-size m i))
	       (cons 'flags (mailbox-message-flags m i)))))
   
   (mailbox-folder-map m (lambda (k uid) (get-info m k))))

;*---------------------------------------------------------------------*/
;*    mailbox-folder-header-fields ::maildir ...                       */
;*---------------------------------------------------------------------*/
(define-method (mailbox-folder-header-fields m::maildir s::bstring)
   (with-access::maildir m (%selection %selection-info %mutex)
      (synchronize %mutex
	 (unless (isa? %selection-info folderinfo)
	    (raise (instantiate::&maildir-error
		      (proc "mailbox-folder-header-fields ::maildir")
		      (msg "No folder selected")
		      (obj m))))
	 (with-access::folderinfo %selection-info (uids path)
	    (let ((k (string->symbol s)))
	       (define (message-date uid file)
		  (let* ((path (make-file-name path file))
			 (lst (with-input-from-file path
				 (lambda ()
				    (with-handler
				       (lambda (e)
					  (exception-notify e)
					  '())
				       (mail-header->list
					  (current-input-port))))))
			 (key (assq k lst)))
		     (cons uid (and (pair? key) (cdr key)))))
	       (hashtable-map uids message-date))))))
   
;*---------------------------------------------------------------------*/
;*    get-folder-info-message ...                                      */
;*---------------------------------------------------------------------*/
(define (get-folder-info-message mbox info uid)
   (with-access::folderinfo info (path uids)
      (let ((entry (hashtable-get uids uid)))
	 (if (not entry)
	     (raise (instantiate::&maildir-error
		       (proc 'get-folder-message)
		       (msg (format "No such message ~s" uid))
		       (obj mbox)))
	     (make-file-path path entry)))))

;*---------------------------------------------------------------------*/
;*    get-folder-message ...                                           */
;*---------------------------------------------------------------------*/
(define (get-folder-message mbox folder uid)
   (with-access::maildir mbox (%mutex)
      (let ((info (synchronize %mutex (get-folder-info mbox folder))))
	 (if (not info)
	     (raise (instantiate::&maildir-error
		       (proc 'get-folder-message)
		       (msg (format "Folder ~s doest not exist!" folder))
		       (obj mbox)))
	     (get-folder-info-message mbox info uid)))))
   
;*---------------------------------------------------------------------*/
;*    get-message-path ...                                             */
;*---------------------------------------------------------------------*/
(define (get-message-path proc m uid)
   (with-access::maildir m (%selection %selection-info)
      (if (not (isa? %selection-info folderinfo))
	  (raise (instantiate::&maildir-error
		    (proc 'get-message-path)
		    (msg "No folder selected")
		    (obj m)))
	  (get-folder-info-message m %selection-info uid))))

;*---------------------------------------------------------------------*/
;*    mailbox-message ::maildir ...                                    */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message m::maildir i::int)
   (with-input-from-file (get-message-path "mailbox-message ::maildir" m i)
      (lambda ()
	 (read-string (current-input-port)))))

;*---------------------------------------------------------------------*/
;*    mailbox-message-path ::maildir ...                               */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-path m::maildir i::int)
   (get-message-path "mailbox-message-path ::maildir" m i))

;*---------------------------------------------------------------------*/
;*    mailbox-message-body ::maildir ...                               */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-body m::maildir i::int . len)
   (let* ((path (get-message-path "mailbox-message-body ::maildir" m i))
	  (ip (open-input-file path))
	  (gram (regular-grammar ()
		   ((+ (out "\n\r"))
		    (ignore))
		   ((or "\n" "\r\n")
		    (ignore))
		   ((or "\n\n" "\r\n\r\n")
		    (if (and (pair? len) (integer? (car len)))
			(read-chars (car len) (the-port))
			(read-string (the-port))))
		   (else
		    ""))))
      (if (input-port? ip)
	  (let ((body (read/rp gram ip)))
	     (close-input-port ip)
	     body)
	  (error 'mailbox-message-boxy "mail does not exist" path))))

;*---------------------------------------------------------------------*/
;*    mailbox-message-header ::maildir ...                             */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-header m::maildir i::int)
   (let* ((path (get-message-path "mailbox-message-header ::maildir" m i))
	  (ip (open-input-file path))
	  (gram (regular-grammar ()
		   ((+ (out "\n\r"))
		    (let ((s (the-string)))
		       (cons s (ignore))))
		   ((or "\n" "\r\n")
		    (let ((s (the-string)))
		       (cons s (ignore))))
		   ((or "\n\n" "\r\n\r\n")
		    '())
		   (else
		    '()))))
      (let ((body (apply string-append (read/rp gram ip))))
	 (close-input-port ip)
	 body)))
   
;*---------------------------------------------------------------------*/
;*    mailbox-message-header-list ::maildir ...                        */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-header-list m::maildir i::int)
   (let ((path (get-message-path mailbox-message-header-list m i)))
      (call-with-input-file path
	 (lambda (ip)
	    (mail-header->list ip)))))

;*---------------------------------------------------------------------*/
;*    mailbox-message-header-field ::maildir ...                       */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-header-field m::maildir i::int s::bstring)
   (let* ((list (mailbox-message-header-list m i))
	  (key (string->symbol (string-downcase s)))
	  (cell (assq key list)))
      (if (pair? cell)
	  (cdr cell)
	  "")))

;*---------------------------------------------------------------------*/
;*    mailbox-message-size ::maildir ...                               */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-size m::maildir i::int)
   (let ((path (get-message-path "mailbox-message-size ::maildir" m i)))
      (elong->fixnum (file-size path))))

;*---------------------------------------------------------------------*/
;*    mailbox-message-info ...                                         */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-info m::maildir i::int)
   (let* ((header (mailbox-message-header-list m i))
	  (hid (assq 'message-id header))
	  (hdate (assq 'date header)))
      (list (if (pair? hid) (cdr hid) #f)
	    i
	    (if (pair? hdate) (cdr hdate) #f)
	    (mailbox-message-size m i)
	    (mailbox-message-flags m i))))

;*---------------------------------------------------------------------*/
;*    mailbox-message-flags ::maildir ...                              */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-flags m::maildir i::int)
   (let* ((m (get-message-path 'mailbox-message-flags m i))
	  (i (string-index-right m #\,))
	  (l (string-length m)))
      (let loop ((i (+fx i 1))
		 (flags '()))
	 (if (=fx i l)
	     flags
	     (case (string-ref-ur m i)
		((#\S)
		 (loop (+fx i 1) (cons "\\Seen" flags)))
		((#\A)
		 (loop (+fx i 1) (cons "\\Answered" flags)))
		((#\D)
		 (loop (+fx i 1) (cons "\\Deleted" flags)))
		((#\F)
		 (loop (+fx i 1) (cons "\\Flagged" flags)))
		(else
		 (loop (+fx i 1) flags)))))))

;*---------------------------------------------------------------------*/
;*    mailbox-message-flags-set! ::maildir ...                         */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-flags-set! m::maildir uid::int flags)
   (let* ((msg (get-message-path 'mailbox-message-flags-set! m uid))
	  (i (+fx 1 (string-index-right msg #\,)))
	  (n (+fx (length flags) i))
	  (name (make-string n)))
      (blit-string! msg 0 name 0 i)
      (when (member "\\Seen" flags)
	 (string-set! name i #\S)
	 (set! i (+fx i 1)))
      (when (member "\\Answered" flags)
	 (string-set! name i #\A)
	 (set! i (+fx i 1)))
      (when (member "\\Deleted" flags)
	 (string-set! name i #\D)
	 (set! i (+fx i 1)))
      (when (member "\\Flagged" flags)
	 (string-set! name i #\F)
	 (set! i (+fx i 1)))
      (when (<fx i n)
	 (set! name (string-shrink! name i)))
      (with-access::maildir m (%selection %selection-info %mutex)
	 (synchronize %mutex
	    (cond
	       ((not (isa? %selection-info folderinfo))
		(raise (instantiate::&maildir-error
			  (proc "mailbox-message-flags-set! ::maildir")
			  (msg "No folder selected")
			  (obj m))))
	       ((not (rename-file msg name))
		(raise (instantiate::&maildir-error
			  (proc "mailbox-message-flags-set! ::maildir")
			  (msg (format "Cannot set message flags ~a" uid))
			  (obj m))))
	       (else
		(update-folderinfo! %selection %selection-info
		   uid (basename name))))))))

;*---------------------------------------------------------------------*/
;*    mailbox-message-delete! ::maildir ...                            */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-delete! m::maildir uid::int)
   (let ((path (get-message-path 'mailbox-message-delete! m uid)))
      (with-access::maildir m (%selection %selection-info %mutex)
	 (synchronize %mutex
	    (cond
	       ((not (isa? %selection-info folderinfo))
		(raise (instantiate::&maildir-error
			  (proc "mailbox-message-delete! ::maildir")
			  (msg "No folder selected")
			  (obj m))))
	       ((not (delete-file path))
		(raise (instantiate::&maildir-error
			  (proc "mailbox-message-delete! ::maildir")
			  (msg (format "Cannot delete message ~a, path: ~a"
				  uid path))
			  (obj m))))
	       (else
		(invalidate-folderinfo! %selection-info)
		(update-folderinfo! %selection %selection-info uid #f)))))))
   
;*---------------------------------------------------------------------*/
;*    mailbox-message-move! ::maildir ...                              */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-move! m::maildir uid::int folder::bstring)
   (let* ((mpath (get-message-path 'mailbox-message-move! m uid))
	  (file (basename mpath))
	  (dir (folder->directory "mailbox-message-move! ::maildir" m folder)))
      (with-access::maildir m (%selection %selection-info %mutex path)
	 (synchronize %mutex
	    (let ((dest (make-file-path dir "cur" file)))
	       (cond
		  ((not (isa? %selection-info folderinfo))
		   (raise (instantiate::&maildir-error
			     (proc "mailbox-message-move! ::maildir")
			     (msg "No folder selected")
			     (obj m))))
		  ((not (rename-file mpath dest))
		   (raise (instantiate::&maildir-error
			     (proc "mailbox-message-move! ::maildir")
			     (msg (format "Cannot move message ~a" uid))
			     (obj m))))
		  (else
		   (with-access::maildir m (%folders %mutex)
		      (let ((oinfo (hashtable-get %folders folder)))
			 (when (isa? oinfo folderinfo)
			    (invalidate-folderinfo! oinfo))))
		   (update-folderinfo! %selection %selection-info uid #f)
		   (with-access::folderinfo %selection-info (nextuid)
		      nextuid))))))))

;*---------------------------------------------------------------------*/
;*    mailbox-message-copy! ::maildir ...                              */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-copy! m::maildir i::int s::bstring)
   (let ((uid (mailbox-message-create! m s (mailbox-message m i))))
      (mailbox-message-flags-set! m uid (mailbox-message-flags m i))
      uid))
   
;*---------------------------------------------------------------------*/
;*    genfilename ...                                                  */
;*---------------------------------------------------------------------*/
(define (genfilename uid base)
   (let* ((host base)
	  (i (string-index host #\.))
	  (hostname (if i (substring host 0 i) host)))
      (format "~a.P~aQ1.~a:2," (current-seconds) uid hostname)))

;*---------------------------------------------------------------------*/
;*    mailbox-message-create! ::maildir ...                            */
;*---------------------------------------------------------------------*/
(define-method (mailbox-message-create! m::maildir folder::bstring txt::bstring)
   (let ((dir (folder->directory "mailbox-message-create! ::maildir"
				   m folder)))
      (with-access::maildir m (%mutex path message-base)
	 (synchronize %mutex
	    (let ((dinfo (get-folder-info m dir)))
	       (unless dinfo
		  (raise
		     (instantiate::&maildir-error
			(proc "mailbox-message-create! ::maildir")
			(msg (format "Folder ~s does not exist" dir))
			(obj m))))
	       (let* ((uid (get-nextuid! dinfo))
		      (file (genfilename uid message-base))
		      (tmp (make-file-path dir "tmp" file))
		      (dest (make-file-path dir "cur" file)))
		  (with-output-to-file tmp (lambda () (display txt)))
		  (rename-file tmp dest)
		  (delete-file tmp)
		  (update-folderinfo! dir dinfo uid file)
		  uid))))))
