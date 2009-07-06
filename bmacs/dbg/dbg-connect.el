;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-connect.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  6 11:25:49 1998                          */
;*    Last change :  Wed Jan 23 13:32:02 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The connection between dbg and the source buffers.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-connect)
(require 'dbg-config)
(require 'dbg-autoload)
(require 'dbg)
(require 'bee-autoload)
(require 'ude-autoload)

;*---------------------------------------------------------------------*/
;*    dbg-connected-buffers ...                                        */
;*    -------------------------------------------------------------    */
;*    The list of connected buffers.                                   */
;*---------------------------------------------------------------------*/
(defvar dbg-connected-buffers '())

;*---------------------------------------------------------------------*/
;*    dbg-connected-buffer ...                                         */
;*    -------------------------------------------------------------    */
;*    A buffer local variable that help the implementation of the      */
;*    dbg-connected-buffer-p predicate.                                */
;*---------------------------------------------------------------------*/
(defvar dbg-connected-buffer nil)
(make-variable-buffer-local 'dbg-connected-buffer)

;*---------------------------------------------------------------------*/
;*    dbg-connected-buffer-p ...                                       */
;*    -------------------------------------------------------------    */
;*    Is the current buffer connected?                                 */
;*---------------------------------------------------------------------*/
(defun dbg-connected-buffer-p ()
  dbg-connected-buffer)

;*---------------------------------------------------------------------*/
;*    dbg-connect-hooks ...                                            */
;*---------------------------------------------------------------------*/
(defvar dbg-connect-hooks ()
  "The list of hooks to run when connecting a buffer.")

;*---------------------------------------------------------------------*/
;*    dbg-disconnect-hooks ...                                         */
;*---------------------------------------------------------------------*/
(defvar dbg-disconnect-hooks ()
  "The list of hooks to run when disconnecting a buffer.")

;*---------------------------------------------------------------------*/
;*    dbg-add-connect-hook ...                                         */
;*---------------------------------------------------------------------*/
(defun dbg-add-connect-hook (mode hook)
  (setq dbg-connect-hooks (cons (cons mode hook) dbg-connect-hooks)))

;*---------------------------------------------------------------------*/
;*    dbg-add-disconnect-hook ...                                      */
;*---------------------------------------------------------------------*/
(defun dbg-add-disconnect-hook (mode hook)
  (setq dbg-disconnect-hooks (cons (cons mode hook) dbg-disconnect-hooks)))

;*---------------------------------------------------------------------*/
;*    dbg-connect-buffer ...                                           */
;*    -------------------------------------------------------------    */
;*    This function connect a buffer, that is, some buffer bindings    */
;*    are changed and the margin is setup.                             */
;*---------------------------------------------------------------------*/
(defun dbg-connect-buffer (buffer)
  (interactive "BBuffer: ")
  ;; because it may happens that latency make emacs got confused,
  ;; we check that it is not the comint buffer that we are trying
  ;; to connect. If so, we simply ignore the request
  (cond
;*    ((not (dbg-comint-started-p))                                    */
;*     (ude-error "No Dbg spawned yet"))                               */
   ((not (eq buffer dbg-comint-buffer))
    (if (stringp buffer)
	(setq buffer (get-buffer buffer)))
    (if (buffer-live-p buffer)
	(progn
	  (set-buffer buffer)
	  (if (not dbg-connected-buffer)
	      (progn
		;; we register this buffer as connected to dbg
		(setq dbg-connected-buffers
		      (cons buffer dbg-connected-buffers))
		;; we mark this buffer as connected to dbg
		(setq dbg-connected-buffer t)
		;; we draw the margin
		(dbg-set-buffer-margin buffer)
		;; mode specific initialization
		(let ((cell (assq major-mode dbg-connect-hooks)))
		  (if (consp cell)
		      (funcall (cdr cell))))
		t)))
      nil))
   (t
    nil)))

;*---------------------------------------------------------------------*/
;*    dbg-connect-file ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-connect-file (file)
  (interactive "fConnect to file: ")
  (let ((buffer (find-file-noselect file)))
    (if (bufferp buffer)
	(let ((pop-up-frames t))
	  (pop-to-buffer (buffer-name buffer))
	  (dbg-connect-buffer buffer)))))
  
;*---------------------------------------------------------------------*/
;*    dbg-disconnect-buffer ...                                        */
;*    -------------------------------------------------------------    */
;*    This function connect a buffer, that is, some buffer bindings    */
;*    are changed and the margin is setup.                             */
;*---------------------------------------------------------------------*/
(defun dbg-disconnect-buffer (buffer)
  (interactive "BBuffer: ")
  (if (buffer-live-p buffer)
      (progn
	(set-buffer buffer)
	(if dbg-connected-buffer
	    (progn
	      ;; we remove the dbg region popup entry
	      (let ((cell (assq major-mode dbg-disconnect-hooks)))
		(if (consp cell)
		    (funcall (cdr cell))))
	      ;; we mark this buffer as connected to dbg
	      (setq dbg-connected-buffer nil)
	      ;; we draw the margin
	      (dbg-unset-buffer-margin buffer)
	      ;; we set dbg mouse bindings
	      (local-unset-key ude-mouse-2-binding)
	      ;; we redisplay the buffer
	      (sit-for 0))))))
	    
;*---------------------------------------------------------------------*/
;*    dbg-toggle-connect-buffer ...                                    */
;*---------------------------------------------------------------------*/
(defun dbg-toggle-connect-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (if (dbg-connected-buffer-p)
	(dbg-disconnect-buffer buffer)
      (dbg-connect-buffer buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-disconnect-all-buffers ...                                   */
;*    -------------------------------------------------------------    */
;*    Disconnect all currently connected buffers.                      */
;*---------------------------------------------------------------------*/
(defun dbg-disconnect-all-buffers ()
  (mapcar 'dbg-disconnect-buffer dbg-connected-buffers)
  (setq dbg-connected-buffers '()))
  
