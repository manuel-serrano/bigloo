;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Cc/cc.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr 29 09:51:32 1995                          */
;*    Last change :  Mon May  7 09:47:11 2018 (serrano)                */
;*    Copyright   :  1995-2018 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C compilation                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cc_cc
   (export  (cc name oname ::bool))
   (import  tools_speek
	    tools_error
	    cc_exec
	    engine_param
	    tools_misc
	    module_module))

;*---------------------------------------------------------------------*/
;*    cc ...                                                           */
;*---------------------------------------------------------------------*/
(define (cc name oname need-to-return)
   (cond
      ((string=? (os-class) "unix")
       (unix-cc name oname need-to-return))
      ((string=? (os-class) "win32")
       (win32-cc name oname))
      ((string=? (os-class) "mingw")
       (mingw-cc name oname need-to-return))
      (else
       (user-error "cc" "Unknown os" (os-class)))))

;*---------------------------------------------------------------------*/
;*    command-line->string ...                                         */
;*---------------------------------------------------------------------*/
(define (command-line->string l)
   (md5sum (format "~a" l)))

;*---------------------------------------------------------------------*/
;*    unix-cc ...                                                      */
;*---------------------------------------------------------------------*/
(define (unix-cc name oname need-to-return)
   (verbose 1 "   . cc (" *cc* ")" #\Newline)
   (cond
      ((not (string? name))
       (error "cc" "can't process cc on stdout" name))
      (else
       (let* ((objname (if (string? oname) oname name))
	      (basename (basename objname))
	      (need-o (and (not (string=? basename objname))
			   (not (string=? (pwd) (dirname objname)))
			   (not (string=? "." (dirname objname)))))
	      (needmv (and need-o (or *cc-move* (string=? *cc-o-option* ""))))
	      (obj (cond
		      ((not need-o)
		       "")
		      (needmv
		       (unix-filename
			  (string-append
			     (bigloo-mangle (symbol->string *module*))
			     (command-line->string *bigloo-args*)
			     "."
			     *c-object-file-extension*)))
		      (else
		       (unix-filename
			  (string-append objname
			     "."
			     *c-object-file-extension*)))))
	      (cc (string-append *cc*
		     " "
		     (format "~( )" *cc-options*)
		     " "
		     *cflags*
		     " "
		     (string-append " " (bigloo-config 'c-pic-flag))
		     (string-append " " (bigloo-config 'c-nan-flag))
		     " "
		     " -c "
		     (if need-o *cc-o-option* "")
		     " "
		     obj
		     " -I. "
		     (let loop ((path *lib-dir*))
			(cond
			   ((null? path)
			    "")
			   ((directory? (car path))
			    (string-append "-I"
			       (car path)
			       " "
			       (loop (cdr path))))
			   (else
			    (loop (cdr path)))))
		     (if (or *c-debug* (>fx *bdb-debug* 0))
			 (string-append " " *c-debug-option*)
			 "")
		     " " (unix-filename name ".c") " "))
	      (rm-csrc  (if *rm-tmp-files*
			    (string-append "&& "
			       (bigloo-config 'shell-rm)
			       " "
			       (unix-filename name ".c") " ")
			    ""))
	      (mv-obj   (if needmv
			    (string-append "&& "
			       (bigloo-config 'shell-mv)
			       " "
			       obj
			       " "
			       (unix-filename
				  objname "."
				  *c-object-file-extension*)
			       " 2>&1 >/dev/null ")
			    ""))
	      (cmd      (string-append cc mv-obj rm-csrc)))
	  (verbose 2 "      [" cmd #\] #\Newline)
	  (exec cmd need-to-return "cc")))))

;*---------------------------------------------------------------------*/
;*    mingw-cc ...                                                     */
;*---------------------------------------------------------------------*/
(define (mingw-cc name oname need-to-return)
   (verbose 1 "   . cc (" *cc* ")" #\Newline)
   (cond
      ((not (string? name))
       (error "cc" "can't process cc on stdout" name))
      (else
       (let* ((oname (if (string? oname) oname name))
	      (dest-obj (if *cc-move*
			    ""
			    (unix-filename
			     (string-append *cc-o-option* oname "."
					    *c-object-file-extension*))))
	      (cc (string-append *cc*
				 " "
				 (format "~( )" *cc-options*)
				 " "
				 *cflags*
				 " -c "
				 dest-obj
				 " -I. "
				 (let loop ((path *lib-dir*))
				    (if (null? path)
					""
					(string-append "-I"
						       (car path)
						       " "
						       (loop (cdr path)))))
				 (if (or *c-debug* (>fx *bdb-debug* 0))
				     (string-append " " *c-debug-option*)
				     "")
				 " " (unix-filename name ".c") " "))
	      (basename (basename oname))
	      (rm-csrc  "")
	      (mv-obj "")
	      (cmd (string-append cc mv-obj rm-csrc)))
	  (print cmd)
	  (verbose 2 "      [" cmd #\] #\Newline)
	  (exec cmd need-to-return "cc")
	  (if (and *cc-move*
		   (not (string=? basename oname))
		   (not (string=? (pwd) (dirname oname)))
		   (not (string=? "." (dirname oname))))
	      (rename-file (string-append basename "." *c-object-file-extension*) 
			   (string-append oname "." *c-object-file-extension*)))
	  (if *rm-tmp-files* 
	      (delete-file (string-append name ".c ")))))))

;*---------------------------------------------------------------------*/
;*    win32-cc ...                                                     */
;*---------------------------------------------------------------------*/
(define (win32-cc name oname)
   (verbose 1 "   . cc (" *cc* ")" #\Newline)
   (cond
      ((not (string? name))
       (error "cc" "can't process cc on stdout" name))
      (else
       (let* ((oname (if (string? oname) oname name))
              (cc-args (append (append-map (lambda (o)
					      (string-split-char o #\space))
				  *cc-options*)
			  (string-split-char *cflags* #\space)
			  '("-c")
			  '("-I.")
			  (let loop ((path *lib-dir*))
			     (if (null? path)
				 '()
				 (cons (string-append "-I" (car path))
                                    (loop (cdr path)))))
			  (if (or *c-debug* (>fx *bdb-debug* 0))
			      (string-split-char *c-debug-option* #\space)
			      '())
			  (list (string-append name ".c"))
			  (if (char=? (string-ref *cc-o-option*
					 (- (string-length *cc-o-option*) 1))
				 #\space)
			      (list *cc-o-option*
				 (string-append oname "." *c-object-file-extension*))
			      (list (string-append *cc-o-option* oname "." *c-object-file-extension*))))))
          (verbose 2 "      " (cons *cc* cc-args) #\Newline)
          (apply run-process *cc* (append cc-args '(wait: #t)))
          (if *rm-tmp-files*
              (delete-file (string-append name ".c ")))))))
