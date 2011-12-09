;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/parse.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 10:10:53 1999                          */
;*    Last change :  Fri Dec  9 11:12:39 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The command parser                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_parse
   (import engine_param
	   tools_error
	   tools_speek
	   tools_io
	   command_command
	   gdb_invoke
	   gdb_annotate)
   (export (command-parse ::pair ::bstring)
	   (generic-parse ::pair-nil ::bstring ::int ::pair-nil)
	   (make-stop-parse ::procedure)
	   (parse-error ::bstring ::bstring ::bstring ::int)))

;*---------------------------------------------------------------------*/
;*    command-parse ...                                                */
;*    -------------------------------------------------------------    */
;*    This function parse a token list in order to figure out what     */
;*    to do with that list. In particular, this function finds out if  */
;*    the command is a Bdb command or a Gdb command. This require a    */
;*    completion on-the-fly of this command.                           */
;*    -------------------------------------------------------------    */
;*    The argument SOURCE is only used for reporting command errors.   */
;*---------------------------------------------------------------------*/
(define (command-parse cmd-list::pair source::bstring)
   (let* ((env        (toplevel-command-env))
	  (sub-cmd-id (car cmd-list))
	  (sub-cmd::command    (find-command sub-cmd-id env)))
      (if (not (isa? sub-cmd command))
	  ;; unknown command are simply pass to gdb without filtering
	  (begin 
	     (gdb-enable-print!)
	     (console-echo (gdb-call->string source)))
	  ((-> sub-cmd parser ) (cdr cmd-list)
				source
				1
				(-> sub-cmd env)))))

;*---------------------------------------------------------------------*/
;*    generic-parse ...                                                */
;*---------------------------------------------------------------------*/
(define (generic-parse cmd-list source level env)
   (let loop ((cmd-list cmd-list)
	      (env      (toplevel-command-env))
	      (level    level))
      (if (null? cmd-list)
	  ;; if we have reached null command it means that we have
	  ;; nothing to do otherwise the end of list would have be
	  ;; intercepted by a sub command parser
	  'nop
	  (let* ((sub-cmd-id (car cmd-list))
		 (sub-cmd::command    (find-command sub-cmd-id env)))
	     (if (not (isa? sub-cmd command))
		 (parse-error "Unknown command" sub-cmd-id source level)
		 ((-> sub-cmd parser ) (cdr cmd-list)
				       source
				       (+fx level 1)
				       (-> sub-cmd env)))))))

;*---------------------------------------------------------------------*/
;*    make-stop-parse ...                                              */
;*---------------------------------------------------------------------*/
(define (make-stop-parse action::procedure)
   (lambda (cmd-list::pair-nil source::bstring level::int env)
      (if (null? cmd-list)
	  (action source)
	  (parse-error "Unknown command" (car cmd-list) source level))))

;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function does not raise an error. It makes its own printing */
;*    and then it returns the nop thunk (denoted by the NOP symbol).   */
;*---------------------------------------------------------------------*/
(define (parse-error msg pattern source err-level)
   (bdb-notify-error "parse" msg pattern)
   (let* ((len  (find-pattern pattern source err-level))
	  (sstr (if (>fx len 0)
		    (fix-tabulation! len
		       source
		       (make-string len #\space))
		    "")))
      (console-error (string-append "#" source #"\n#" sstr #"^\n"))))

;*---------------------------------------------------------------------*/
;*    find-pattern ...                                                 */
;*---------------------------------------------------------------------*/
(define (find-pattern pat source err-level)
   ;; this function skip a number of separator in a string and return
   ;; the index of the char following the n-th separator
   (define (skip str index num)
      (if (=fx num 0)
	  0
	  (let loop ((index index)
		     (num   num))
	     (cond
		((=fx num 0)
		 index)
		((or (char=? (string-ref str index) #\Space)
		     (char=? (string-ref str index) #\Tab))
		 (loop (+fx index 1) (-fx num 1)))
		(else
		 (loop (+fx index 1) num))))))
   (define (substring-at=? r2 pat src lpat)
      (if (<fx (string-length src) (+fx r2 lpat))
	  (error "substring-at=?" "Illegal search" (cons pat src))
	  (let loop ((r1 0)
		     (r2 r2))
	     (cond
		((=fx r1 lpat)
		 #t)
		((char=? (string-ref src r2) (string-ref pat r1))
		 (loop (+fx r1 1) (+fx r2 1)))
		(else
		 #f)))))
   (let* ((start (skip source 0 err-level))
	  (len   (string-length source))
	  (lpat  (string-length pat))
	  (stop  (-fx len lpat)))
      (let loop ((i start))
	 (cond
	    ((=fx i stop)
	     i)
	    ((substring-at=? i pat source lpat)
	     i)
	    (else
	     (loop (+fx i 1)))))))
   
;*---------------------------------------------------------------------*/
;*    fix-tabulation! ...                                              */
;*---------------------------------------------------------------------*/
(define (fix-tabulation! marker src dst)
   (let loop ((read (-fx marker 1)))
      (cond
	 ((=fx read -1)
	  dst)
	 ((char=? (string-ref src read) #\tab)
	  (string-set! dst read #\tab)
	  (loop (-fx read 1)))
	 (else
	  (loop (-fx read 1))))))
    

	  
