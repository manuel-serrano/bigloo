;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-gdb.el                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 18 08:14:01 1998                          */
;*    Last change :  Tue Feb 19 11:54:34 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Cee Gdb connection                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-gdb)
(require 'ude-autoload)
(require 'dbg-autoload)
(require 'dbg-config)
(require 'dbg)
(require 'cee-keymap)
 
;*---------------------------------------------------------------------*/
;*    gdb-cee-source-menu-entries ...                                  */
;*---------------------------------------------------------------------*/
(defun gdb-cee-source-menu-entries (event)
  (let* ((region (buffer-substring (region-beginning) (region-end)))
	 (buffer (event-buffer event))
	 (file   (file-name-nondirectory (buffer-file-name buffer)))
	 (line   (count-lines 1 (+ (region-beginning) 1))))
    (list (vector "(gdb) print"
		  (list 'dbg-command-region "print " region)
		  t)
	  (vector "(gdb) print *"
		  (list 'dbg-command-region "print *" region)
		  t)
	  (vector "(gdb) print **"
		  (list 'dbg-command-region "print **" region)
		  t)
	  "-"
	  (vector "(gdb) whatis"
		  (list 'dbg-command-region "whatis " region)
		  t)
	  "-"
	  (vector "(gdb) break"
		  (list 'gdb-remote-set-breakpoint
			(concat dbg-break-command " ")
			file
			line)
		  t)
	  (vector "(gdb) temporary break"
		  (list 'gdb-remote-set-breakpoint
			(concat dbg-tbreak-command " ")
			file
			line)
		  t)
	  "--:shadowDoubleEtchedOut")))

;*---------------------------------------------------------------------*/
;*    gdb-cee-balloon-start ...                                        */
;*---------------------------------------------------------------------*/
(defun gdb-cee-balloon-start ()
  ;; we start balloon
  (ude-balloon-start)
  ;; the action on breakpoint (margin balloon)
  (ude-add-balloon-action
   'cee-gdb
   #'(lambda (x win)
       (and (windowp win)
	    (numberp x)
	    (> (window-left-margin-pixel-width win) x)))
   #'(lambda ()
       (let* ((buffer (ude-balloon-get-buffer))
	      (pos    (ude-balloon-get-point)))
	 (if (and (bufferp buffer) (numberp pos))
	     (let* ((ex  (extent-at pos buffer 'dbg-breakpoint))
		    (msg (dbg-breakpoint-balloon-message ex)))
	       (if (stringp msg)
		   (display-message 'no-log msg))))))))

;*---------------------------------------------------------------------*/
;*    gdb-cee-balloon-stop ...                                         */
;*---------------------------------------------------------------------*/
(defun gdb-cee-balloon-stop ()
  (ude-remove-balloon-action 'cee-gdb))

;*---------------------------------------------------------------------*/
;*    cee-connect-buffer-hook ...                                      */
;*---------------------------------------------------------------------*/
(defun cee-connect-buffer-hook ()
  ;; we set dbg mouse bindings
  (ude-add-menu #'(lambda (event)
		    (and (> (window-left-margin-pixel-width
			     (event-window event))
			    (event-x-pixel event))))
		'dbg-breakpoint-menu)
  ;; we actived Scheme source balloon help
  (gdb-cee-balloon-start)
  ;; the s-expression menu when clicking on the active region
  (cee-add-region-popup-entry 'gdb-cee-source-menu-entries)
  (if (bufferp dbg-comint-buffer)
      ;; we have to refresh the breakpoints if we are running gdb by the
      ;; means of comint
      (dbg-breakpoint-command)))

;*---------------------------------------------------------------------*/
;*    cee-disconnect-buffer-hook ...                                   */
;*---------------------------------------------------------------------*/
(defun cee-disconnect-buffer-hook ()
  ;; we first remove the region popup
  (cee-remove-region-popup-entry 'gdb-cee-source-menu-entries)
  ;; then, we remove the balloon help
  (gdb-cee-balloon-stop))
  
;*---------------------------------------------------------------------*/
;*    cee-dbg-mode-hook ...                                            */
;*---------------------------------------------------------------------*/
(defun cee-dbg-mode-hook ()
  (dbg-verbose-remote-call "set confirm off"))

;*---------------------------------------------------------------------*/
;*    cee-gdb-init ...                                                 */
;*---------------------------------------------------------------------*/
(defun cee-gdb-init ()
  ;; cee gdb connexion
  (dbg-add-connect-hook 'c-mode 'cee-connect-buffer-hook)
  (dbg-add-disconnect-hook 'c-mode 'cee-disconnect-buffer-hook)
  ;; standard hook
  (add-hook 'dbg-spawn-hook 'cee-dbg-mode-hook))
  
;*---------------------------------------------------------------------*/
;*    cee-gdb-other-frame ...                                          */
;*---------------------------------------------------------------------*/
(defun cee-gdb-other-frame ()
  (interactive)
  (cee-gdb-init)
  (dbg-other-frame))


