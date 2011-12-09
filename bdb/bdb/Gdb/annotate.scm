;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Gdb/annotate.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 17:27:11 1999                          */
;*    Last change :  Sun Jul 30 09:01:16 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The GDB annotate output command handling.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gdb_annotate
   (import gdb_invoke
	   gdb_proc
	   gdb_read
	   tools_tools
	   tools_speek
	   tools_io
	   engine_param
	   command_backtrace
	   command_print
	   command_display
	   command_explore
	   command_expr
	   patient_mangling)
   (export (find-annotate-parser line)
	   (gdb-annotate-reset-registers!)
	   (gdb-annotate-user-reset-registers!)
	   (gdb-annotate-busy?)
	   (gdb-annotate-error?)
	   (gdb-annotate-hook)
	   (gdb-annotate-run-hook)
	   (gdb-annotate-running?)
	   (gdb-annotate-frame-invalid?)
	   (gdb-annotate-current-function)
	   (gdb-annotate-current-function-set! ::obj)
	   (gdb-annotate-current-frame)
	   (gdb-annotate-reset-stack-frames!)
	   (gdb-annotate-stack-frames)
	   (gdb-annotate-current-frame-set! ::obj)
	   (gdb-annotate-breakpoint-table)
	   (gdb-annotate-registers-protect ::procedure)
	   (gdb-enable-print!)
	   (gdb-enable-bigloo-print!)
	   (gdb-enable-explore-print!)
	   (gdb-enable-store-print!)
	   (gdb-disable-print!)
	   (get-gdb-print-store)
	   (console-frame ::obj ::symbol))
   
   (export (class stack-frame
	      (num::bstring read-only)
	      (name::bstring read-only)
	      (args::pair-nil read-only)
	      (file-name read-only)
	      (line-num read-only)))
   
   (static (class arg
	      (name::bstring read-only)
	      (separator::bstring read-only)
	      (value::bstring read-only))))

;*---------------------------------------------------------------------*/
;*    *gdb-annotate-reg-starting?* ..                                  */
;*    *gdb-annotate-reg-stopped?* ...                                  */
;*    *gdb-annotate-reg-signal?* ...                                   */
;*    *gdb-annotate-reg-frames-invalid?* ...                           */
;*    *gdb-annotate-reg-breakpoints-invalid?* ...                      */
;*    *gdb-annotate-reg-breakpoint?* ...                               */
;*    *gdb-annotate-reg-error?* ...                                    */
;*    *gdb-annotate-reg-stack-frame-number* ...                        */
;*    *gdb-annotate-reg-stack-function-name* ...                       */
;*    *gdb-annotate-reg-stack-args* ...                                */
;*    *gdb-annotate-reg-stack-frame-source* ...                        */
;*    *gdb-annotate-reg-stack-file* ...                                */
;*    *gdb-annotate-reg-stack-line* ...                                */
;*    *gdb-annotate-reg-stack-frame* ...                               */
;*    *gdb-annotate-reg-stack-frame-previous* ...                      */
;*    *gdb-annotate-reg-stack-frames* ...                              */
;*    *gdb-annotate-reg-stack-frame-invalid?* ...                      */
;*    *gdb-annotate-reg-source* ...                                    */
;*    *gdb-annotate-reg-signal?* ...                                   */
;*    *gdb-annotate-reg-breakpoint-table* ...                          */
;*    *gdb-annotate-reg-display-table* ...                             */
;*    -------------------------------------------------------------    */
;*    Annotate register. These global variables contains relevant      */
;*    status informations. There are reset before every GDB            */
;*    command invokation.                                              */
;*---------------------------------------------------------------------*/
(define *gdb-annotate-reg-starting?* #f)
(define *gdb-annotate-reg-stopped?* #f)
(define *gdb-annotate-reg-signal?* #f)
(define *gdb-annotate-reg-frames-invalid?* #f)
(define *gdb-annotate-reg-breakpoints-invalid?* #f)
(define *gdb-annotate-reg-breakpoint?* #f)
(define *gdb-annotate-reg-busy?* #f)
(define *gdb-annotate-reg-error?* #f)
(define *gdb-annotate-reg-stack-frame-number* 0)
(define *gdb-annotate-reg-stack-function-name* #unspecified)
(define *gdb-annotate-reg-stack-args* '())
(define *gdb-annotate-reg-stack-frame-source* '())
(define *gdb-annotate-reg-stack-file* #unspecified)
(define *gdb-annotate-reg-stack-line* -1)
(define *gdb-annotate-reg-stack-frame* #unspecified)
(define *gdb-annotate-reg-stack-frame-previous* #unspecified)
(define *gdb-annotate-reg-stack-frames* '())
(define *gdb-annotate-reg-stack-frame-invalid?* #f)
(define *gdb-annotate-reg-source* #unspecified)
(define *gdb-annotate-reg-breakpoint-table* #f)
(define *gdb-annotate-reg-display-table* '())

;*---------------------------------------------------------------------*/
;*    *gdb-annotate-print?* ...                                        */
;*    *gdb-annotate-print-store* ...                                   */
;*    -------------------------------------------------------------    */
;*    Value display machinery.                                         */
;*---------------------------------------------------------------------*/
(define *gdb-annotate-print?* #f)
(define *gdb-annotate-print-store* '())

;*---------------------------------------------------------------------*/
;*    gdb-annotate-reset-registers! ...                                */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-reset-registers!)
   (set! *gdb-annotate-reg-display-table* '())
   (set! *gdb-annotate-reg-starting?* #f)
   (set! *gdb-annotate-reg-stopped?* #f)
   (set! *gdb-annotate-reg-signal?* #f)
   (set! *gdb-annotate-reg-stack-frame-invalid?* #f)
   (set! *gdb-annotate-reg-breakpoints-invalid?* #f)
   (set! *gdb-annotate-reg-breakpoint?* #f)
   (set! *gdb-annotate-reg-busy?* #f)
   (set! *gdb-annotate-reg-error?* #f))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-registers-protect ...                               */
;*    -------------------------------------------------------------    */
;*    Invoke a thunk, protecting the annotate registers.               */
;*    -------------------------------------------------------------    */
;*    The register *GDB-ANNOTATE-REG-STACK-FRAMES* can't be            */
;*    protected otherwise the Kbdb backtrace hook can't work anymore   */
;*    because it must run in server (i.e., protect) mode.              */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-registers-protect thunk)
   (let* ((reg-starting? *gdb-annotate-reg-starting?*)
	  (reg-stopped? *gdb-annotate-reg-stopped?*)
	  (reg-signal? *gdb-annotate-reg-signal?*)
	  (reg-stack-frame-invalid? *gdb-annotate-reg-stack-frame-invalid?*)
	  (reg-breakpoints-invalid? *gdb-annotate-reg-breakpoints-invalid?*)
	  (reg-breakpoint? *gdb-annotate-reg-breakpoint?*)
	  (reg-busy? *gdb-annotate-reg-busy?*)
	  (reg-error? *gdb-annotate-reg-error?*)
	  (reg-frame-number *gdb-annotate-reg-stack-frame-number*)
	  (reg-function-name *gdb-annotate-reg-stack-function-name*)
	  (reg-stack-frame *gdb-annotate-reg-stack-frame*)
	  (reg-stack-frame-previous *gdb-annotate-reg-stack-frame-previous*)
	  (res (thunk)))
      (set! *gdb-annotate-reg-stack-frame-number* reg-frame-number)
      (set! *gdb-annotate-reg-stack-frame* reg-stack-frame)
      (set! *gdb-annotate-reg-stack-frame-previous* reg-stack-frame-previous)
      (set! *gdb-annotate-reg-stack-function-name* reg-function-name)
      (set! *gdb-annotate-reg-starting?* reg-starting?)
      (set! *gdb-annotate-reg-stopped?* reg-stopped?)
      (set! *gdb-annotate-reg-signal?* reg-signal?)
      (set! *gdb-annotate-reg-stack-frame-invalid?* reg-stack-frame-invalid?)
      (set! *gdb-annotate-reg-breakpoints-invalid?* reg-breakpoints-invalid?)
      (set! *gdb-annotate-reg-breakpoint?* reg-breakpoint?)
      (set! *gdb-annotate-reg-busy?* reg-busy?)
      (set! *gdb-annotate-reg-error?* reg-error?)
      res))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-frame-invalid? ...                                  */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-frame-invalid?)
   *gdb-annotate-reg-frames-invalid?*)

;*---------------------------------------------------------------------*/
;*    gdb-annotate-user-reset-registers! ...                           */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-user-reset-registers!)
   (set! *gdb-annotate-reg-source* #unspecified))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-busy? ...                                           */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-busy?)
   *gdb-annotate-reg-busy?*)

;*---------------------------------------------------------------------*/
;*    gdb-annotate-error? ...                                          */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-error?)
   *gdb-annotate-reg-error?*)

;*---------------------------------------------------------------------*/
;*    gdb-annotate-breakpoint-table ...                                */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-breakpoint-table)
   *gdb-annotate-reg-breakpoint-table*)

;*---------------------------------------------------------------------*/
;*    console-frame ...                                                */
;*---------------------------------------------------------------------*/
(define (console-frame frame format)
   (with-access::stack-frame frame (name args file-name line-num num)
      ;; the stack frame number
      (if (not (eq? format 'breakpoint))
	  (if (string? num)
	      (begin
		 (console-echo "#")
		 (console-echo num)
		 (console-echo "   "))))
      ;; we can't demangle inside signal handler because GDB CALL
      ;; command is not available.
      (let ((str (and (not *gdb-annotate-reg-signal?*)
		      (not (eq? *bdb-mode* 'c))
		      (bdb-demangle name))))
	 (if (string? str)
	     (console-echo-edit str str)
	     (console-echo name)))
      (if (pair? args)
	  (console-echo "(...)")
	  (console-echo "()"))
      (console-echo " at ")
      (cond
	 ((and (string? file-name) (string? line-num))
	  (let ((str (string-append file-name ":" line-num)))
	     (console-echo-edit str str)))
	 ((string? file-name)
	  (console-echo-edit file-name file-name))
	 (else
	  (console-echo "??? <no source info available>")))
      (if (memq format '(short breakpoint))
	  (console-newline))))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-run-hook ...                                        */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-run-hook)
   (set! *gdb-annotate-reg-stack-frame* #unspecified)
   (set! *gdb-annotate-reg-stack-frame-previous* #unspecified))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-hook ...                                            */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-hook)
   ;; we display current stack frame only if we are running and
   ;; if the stack frame has changed.
;*    (tprint #"\n==========> frame: "                                 */
;* 	   (if (stack-frame? *gdb-annotate-reg-stack-frame*)           */
;* 	       (stack-frame-name *gdb-annotate-reg-stack-frame*)       */
;* 	       "___")                                                  */
;* 	   " previous: "                                               */
;* 	   (if (stack-frame? *gdb-annotate-reg-stack-frame-previous*)  */
;* 	       (stack-frame-name *gdb-annotate-reg-stack-frame-previous*) */
;* 	       "___"))                                                 */
   (if (or *gdb-annotate-reg-breakpoint?*
	   (and (not *bdb-gui-host?*)
		(isa? *gdb-annotate-reg-stack-frame* stack-frame)
		(and (not (equal? *gdb-annotate-reg-stack-frame-previous*
				  *gdb-annotate-reg-stack-frame*))
		     *gdb-annotate-reg-stack-frame-invalid?*
		     *gdb-annotate-reg-frames-invalid?*)))
       (begin
	  ;; we display the stack frame only if it has changed since the
	  ;; last time we have displayed it.
	  (set! *gdb-annotate-reg-stack-frame-previous*
		*gdb-annotate-reg-stack-frame*)
	  (console-frame *gdb-annotate-reg-stack-frame*
			 (if *gdb-annotate-reg-breakpoint?*
			     'breakpoint
			     'short))))
   ;; the source line position
   (if (string? *gdb-annotate-reg-source*)
       (console-source *gdb-annotate-reg-source*)
       ;; the newline
       (console-newline))
   ;; the display table
   (if (pair? *gdb-annotate-reg-display-table*)
       (display-display-table! *gdb-annotate-reg-display-table*)))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-running? ...                                        */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-running?)
   (isa?  *gdb-annotate-reg-stack-frame* stack-frame))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-current-function ...                                */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-current-function)
   (if (gdb-annotate-running?)
       *gdb-annotate-reg-stack-function-name*))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-current-function-set! ...                           */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-current-function-set! fun)
   (set! *gdb-annotate-reg-stack-function-name* fun))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-current-frame ...                                   */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-current-frame)
   (if (gdb-annotate-running?)
       *gdb-annotate-reg-stack-frame*))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-current-frame-set! ...                              */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-current-frame-set! frame)
   (set! *gdb-annotate-reg-stack-frame* frame))

;*---------------------------------------------------------------------*/
;*    gdb-annotate-stack-frames ...                                    */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-stack-frames)
   *gdb-annotate-reg-stack-frames*)

;*---------------------------------------------------------------------*/
;*    gdb-annotate-reset-stack-frames! ...                             */
;*---------------------------------------------------------------------*/
(define (gdb-annotate-reset-stack-frames!)
   (set! *gdb-annotate-reg-stack-frames* '()))

;*---------------------------------------------------------------------*/
;*    gdb-enable-print! ...                                            */
;*---------------------------------------------------------------------*/
(define (gdb-enable-print!)
   (set! *gdb-annotate-print?* #t))

;*---------------------------------------------------------------------*/
;*    gdb-enable-bigloo-print! ...                                     */
;*---------------------------------------------------------------------*/
(define (gdb-enable-bigloo-print!)
   (set! *gdb-annotate-print?* 'bigloo))

;*---------------------------------------------------------------------*/
;*    gdb-enable-explore-print! ...                                    */
;*---------------------------------------------------------------------*/
(define (gdb-enable-explore-print!)
   (set! *gdb-annotate-print?* 'explore))

;*---------------------------------------------------------------------*/
;*    gdb-enable-store-print! ...                                      */
;*---------------------------------------------------------------------*/
(define (gdb-enable-store-print!)
   (set! *gdb-annotate-print-store* '())
   (set! *gdb-annotate-print?* 'store))

;*---------------------------------------------------------------------*/
;*    gdb-disable-print! ...                                           */
;*---------------------------------------------------------------------*/
(define (gdb-disable-print!)
   (set! *gdb-annotate-print-store* '())
   (set! *gdb-annotate-print?* #f))

;*---------------------------------------------------------------------*/
;*    get-gdb-print-store ...                                          */
;*---------------------------------------------------------------------*/
(define (get-gdb-print-store)
   *gdb-annotate-print-store*)

;*---------------------------------------------------------------------*/
;*    *annotate-parser* ...                                            */
;*---------------------------------------------------------------------*/
(define *annotate-parser* '())

;*---------------------------------------------------------------------*/
;*    add-annotate-parser! ...                                         */
;*---------------------------------------------------------------------*/
(define (add-annotate-parser! start-markup parser)
   (set! *annotate-parser*
	 (cons (cons start-markup parser) *annotate-parser*)))

;*---------------------------------------------------------------------*/
;*    find-annotate-parser ...                                         */
;*---------------------------------------------------------------------*/
(define (find-annotate-parser line)
   (define (substring-assoc obj alist)
      (let loop ((alist alist))
	 (if (pair? alist)
	     (if (string-prefix? (caar alist) obj)
		 (car alist)
		 (loop (cdr alist)))
	     #f)))
   (let ((cell (substring-assoc line *annotate-parser*)))
      (if (pair? cell)
	  (cdr cell)
	  #f)))

;*---------------------------------------------------------------------*/
;*    All the annotate parsers.                                        */
;*---------------------------------------------------------------------*/
;; program start and completion
(add-annotate-parser! #"quit" ignore-parser)
(add-annotate-parser! #"exited" exited-parser)
(add-annotate-parser! #"signal" signal-parser)
(add-annotate-parser! #"signalled" signalled-parser)
(add-annotate-parser! #"stopped\n" stopped-parser)
(add-annotate-parser! #"starting\n" starting-parser)
;; misc
(add-annotate-parser! #"display-begin" display-begin-parser)
(add-annotate-parser! #"frames-invalid\n" frames-invalid-parser)
(add-annotate-parser! #"breakpoints-invalid\n" breakpoints-invalid-parser)
(add-annotate-parser! #"breakpoint " breakpoint-parser)
(add-annotate-parser! #"error-begin\n" error-begin-parser)
(add-annotate-parser! #"error\n" error-parser)
;; frame annotations
(add-annotate-parser! #"frame-begin" frame-begin-parser)
(add-annotate-parser! #"frame-end\n" frame-end-parser)
(add-annotate-parser! #"frame-address\n" frame-address-parser)
(add-annotate-parser! #"frame-function-name\n" frame-function-name-parser)
(add-annotate-parser! #"function-call\n" function-call-parser)
(add-annotate-parser! #"signal-handler-caller\n" signal-handler-caller-parser)
(add-annotate-parser! #"source " source-parser)
;; breakpoints
(add-annotate-parser! #"breakpoints-headers\n" breakpoint-headers-parser)
(add-annotate-parser! #"breakpoints-table-end\n" breakpoint-table-parser)
;; argument parsing
(add-annotate-parser! #"arg-begin" arg-begin-parser)
;; actually gdb 4.18 nor gdb 5.0 emit these annotations while they should.
(add-annotate-parser! #"post-prompt\n" ignore-parser)
(add-annotate-parser! #"pre-prompt\n" ignore-parser)
(add-annotate-parser! #"prompt\n" ignore-parser)
;; we are in non-interactive mode so query should not been raised
(add-annotate-parser! #"query\n" ignore-parser)
(add-annotate-parser! #"overload-choice\n" ignore-parser)
(add-annotate-parser! #"prompt-for-continue\n" ignore-parser)
;; values
(add-annotate-parser! #"value-history-begin" value-history-begin-parser)
(add-annotate-parser! #"value-begin" value-begin-parser)
(add-annotate-parser! #"field-begin" field-begin-parser)
(add-annotate-parser! #"array-section-begin" array-section-begin-parser)
;; commands
(add-annotate-parser! #"pre-commands" pre-commands-parser)
(add-annotate-parser! #"post-commands" ignore-parser)
(add-annotate-parser! #"commands" ignore-parser)

;*---------------------------------------------------------------------*/
;*    check-line ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function parse one display entry entry.                     */
;*---------------------------------------------------------------------*/
(define (check-line who markup line)
   (bdb-log 6 "check-line: " who " {" markup "}<->{" line "}" #\Newline)
   (if (not (substring=? markup line (string-length markup)))
       (error "check-line"
	      (string-append "Illegal line (" markup " expected)")
	      (string-until line #\Newline 0))))

;*---------------------------------------------------------------------*/
;*    ignore-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (ignore-parser _)
   #f)

;*---------------------------------------------------------------------*/
;*    frames-invalid-parser ...                                        */
;*---------------------------------------------------------------------*/
(define (frames-invalid-parser line)
   (set! *gdb-annotate-reg-frames-invalid?* #t)
   #f)

;*---------------------------------------------------------------------*/
;*    breakpoints-invalid-parser ...                                   */
;*---------------------------------------------------------------------*/
(define (breakpoints-invalid-parser line)
   (set! *gdb-annotate-reg-breakpoints-invalid?* #t)
   #f)

;*---------------------------------------------------------------------*/
;*    breakpoint-parser ...                                            */
;*---------------------------------------------------------------------*/
(define (breakpoint-parser line)
   (set! *gdb-annotate-reg-breakpoint?* #t)
   #f)

;*---------------------------------------------------------------------*/
;*    starting-parser ...                                              */
;*---------------------------------------------------------------------*/
(define (starting-parser line)
   ;; register the running state
   (set! *gdb-annotate-reg-busy?* #t)
   (set! *gdb-annotate-reg-starting?* #t)
   ;; call the busy hooks
   (for-each (lambda (h) (h #t)) *gdb-busy-hook*)
   #f)

;*---------------------------------------------------------------------*/
;*    stopped-parser ...                                               */
;*---------------------------------------------------------------------*/
(define (stopped-parser line)
   ;; register the running state
   (set! *gdb-annotate-reg-busy?* #f)
   (set! *gdb-annotate-reg-stopped?* #t)
   ;; call the busy hooks
   (for-each (lambda (h) (h #f)) *gdb-busy-hook*)
   #f)

;*---------------------------------------------------------------------*/
;*    error-parser ...                                                 */
;*---------------------------------------------------------------------*/
(define (error-parser _)
   (set! *gdb-annotate-reg-error?* #t)
   #f)

;*---------------------------------------------------------------------*/
;*    error-begin-parser ...                                           */
;*    -------------------------------------------------------------    */
;*    Warning this markup is emitted on stderr, not on stdout. Thus,   */
;*    we have to implement a special function to display error.        */
;*    Errors have been detected by the means of the ^z^zerror markup   */
;*    on stdout, afterward, we have to read the error.                 */
;*---------------------------------------------------------------------*/
(define (error-begin-parser l)
   (error "error-begin-parser" "Illegal markup" (string-until l #\Newline 0)))

;*---------------------------------------------------------------------*/
;*    frame-begin-parser ...                                           */
;*    -------------------------------------------------------------    */
;*    we have to fetch the frame number here, it is important          */
;*    because when we will be parsing a function number or an argument */
;*    list that number will tell us the depth in the stack.            */
;*---------------------------------------------------------------------*/
(define (frame-begin-parser line)
;*    (tprint "FRAME-BEGIN-PARSER: " (string-until line #\Newline 0))  */
   ;; we mark that we have seen a new stack frame
   (set! *gdb-annotate-reg-stack-frame-invalid?* #t)
   ;; because we don't know if we will have function name information
   ;; before continuing parsing, we have to reset that register
   (set! *gdb-annotate-reg-stack-function-name* #unspecified)
   (set! *gdb-annotate-reg-stack-file* #unspecified)
   (set! *gdb-annotate-reg-stack-line* #unspecified)
   ;; then, we store the frame number
   (set! *gdb-annotate-reg-stack-frame-number*
	 (string-until-at line
			  (+ 1 (string-length #"frame-begin"))
			  #\space
			  0))
   (let ((level-string (gdb-read-line 1)))
      #t))

;*---------------------------------------------------------------------*/
;*    frame-end-parser ...                                             */
;*---------------------------------------------------------------------*/
(define (frame-end-parser line)
   (verbose 5 "frame-end-parser: FRAME: "
	    *gdb-annotate-reg-stack-frame-number* " "
	    *gdb-annotate-reg-stack-function-name*
	    #\Newline)
   (if *gdb-annotate-reg-stack-frame-invalid?*
       ;; we store the new stack frame description
       (let ((f (instantiate::stack-frame
		   (num *gdb-annotate-reg-stack-frame-number*)
		   (name *gdb-annotate-reg-stack-function-name*)
		   (args *gdb-annotate-reg-stack-args*)
		   (file-name (if (string? *gdb-annotate-reg-stack-file*)
				  *gdb-annotate-reg-stack-file*
				  #unspecified))
		   (line-num (if (string? *gdb-annotate-reg-stack-line*)
				 *gdb-annotate-reg-stack-line*
				 #unspecified)))))
	  (set! *gdb-annotate-reg-stack-frame* f)
	  (set! *gdb-annotate-reg-stack-frames*
		(cons f *gdb-annotate-reg-stack-frames*))))
   #f)
      
;*---------------------------------------------------------------------*/
;*    frame-address-parser ...                                         */
;*---------------------------------------------------------------------*/
(define (frame-address-parser _)
   (let ((address (gdb-read-line 0)))
      (check-line frame-address-parser:
		  #"frame-address-end\n" (gdb-read-line 1))
      (let* ((separator (gdb-read-line 0))
	     (fline (gdb-read-line 1)))
	 (check-line frame-address-parser:
		     #"frame-function-name\n" fline)
	 (frame-function-name-parser fline))))
   
;*---------------------------------------------------------------------*/
;*    frame-function-name-parser ...                                   */
;*---------------------------------------------------------------------*/
(define (frame-function-name-parser _)
   (set! *gdb-annotate-reg-stack-function-name* (gdb-read-line 0))
   (let ((frame-args (gdb-read-line 1)))
      (check-line frame-function-name-parser:
		  #"frame-args\n" frame-args)
      (let ((open-par (gdb-read-line 0)))
	 (check-line frame-function-name-parser: " (" open-par)
	 (let loop ((line (if (string=? open-par " ()")
			      #")\n"
			      (gdb-read-line 1)))
		    (args '()))
	    (cond
	       ((string=? line #")\n")
		(set! *gdb-annotate-reg-stack-args* args)
		(let loop ((line (gdb-read-line 1)))
		   (cond
		      ((string-prefix? #"frame-source-begin\n" line)
		       (frame-source-begin-parser line))
		      ((string-prefix? #"frame-where\n" line)
		       (frame-where-parser line))
		      ((newline? line)
		       #unspecified))))
	       ((string=? line #", \n")
		(loop (gdb-read-line 1) args))
	       (else
		(check-line frame-function-name-parser:
			    #"arg-begin\n" line)
		(let ((arg (arg-begin-parser line)))
		   (loop (gdb-read-line 1) (cons arg args)))))))))
	 
;*---------------------------------------------------------------------*/
;*    arg-begin-parser ...                                             */
;*    -------------------------------------------------------------    */
;*    The name is printed here un demangled. I guess this is want      */
;*    one want because this parse is used on command such as           */
;*    "info frame" which prints rather low level informations (such    */
;*    as register values).					       */
;*---------------------------------------------------------------------*/
(define (arg-begin-parser _)
   (let ((name (gdb-read-line 0)))
      (check-line arg-begin-parser: #"arg-name-end\n" (gdb-read-line 1))
      (let* ((separator-string (gdb-read-line 0))
	     (arg-value (gdb-read-line 0)))
	 (check-line arg-begin-parser: "arg-value" arg-value)
	 (multiple-value-bind (value next-line)
	    (value-parser (gdb-read-line 0)
			  (string-char-after arg-value #\space))
	    (begin
	       (check-line 'arg-begin-parser: #"arg-end" next-line)
	       (instantiate::arg
		  (name name)
		  (separator separator-string)
		  (value value)))))))

;*---------------------------------------------------------------------*/
;*    function-call-parser ...                                         */
;*---------------------------------------------------------------------*/
(define (function-call-parser _)
   (gdb-read-line 0))

;*---------------------------------------------------------------------*/
;*    signal-handler-caller-parser ...                                 */
;*---------------------------------------------------------------------*/
(define (signal-handler-caller-parser _)
   (gdb-read-line 0))

;*---------------------------------------------------------------------*/
;*    frame-source-begin-parser ...                                    */
;*---------------------------------------------------------------------*/
(define (frame-source-begin-parser _)
   (let ((intro (gdb-read-line 0)))
      (check-line frame-source-begin-parser:
		  #"frame-source-file\n" (gdb-read-line 1))
      (let ((file-name (gdb-read-line 0)))
	 (set! *gdb-annotate-reg-stack-file* file-name)
	 (check-line frame-source-begin-parser:
		     #"frame-source-file-end\n" (gdb-read-line 1))
	 (check-line frame-source-begin-parser:
		     ":" (gdb-read-line 0))
	 (check-line frame-source-begin-parser:
		     #"frame-source-line\n" (gdb-read-line 1))
	 (let ((line (gdb-read-line 0)))
	    (set! *gdb-annotate-reg-stack-line* line)
	    (check-line frame-source-begin-parser:
			#"frame-source-end\n" (gdb-read-line 1))
	    #f))))

;*---------------------------------------------------------------------*/
;*    frame-where-parser ...                                           */
;*---------------------------------------------------------------------*/
(define (frame-where-parser _)
   (gdb-read-line 0))
	     
;*---------------------------------------------------------------------*/
;*    source-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (source-parser line)
   (define (bee-client-source-representation)
      (string-append #"" (string-until-at line
					    (string-length #"source ")  
					    #\Newline
					    0)))
   (define (user-source-representation)
      (let* ((start (string-length #"source "))
	     (file  (string-until-at line start #\: 0))
	     (lnum  (string-until-at line (+ start (string-length file) 1)
				     #\: 0))
	     (pos   (string-until-at line (+ start
					     (string-length file)
					     (string-length lnum)
					     2)
				     #\: 0))
	     (beg   (string-until-at line (+ start
					     (string-length file)
					     (string-length lnum)
					     (string-length pos)
					     3)
				     #\: 0))
	     (addr  (string-until-at line (+ start
					     (string-length file)
					     (string-length lnum)
					     (string-length pos)
					     (string-length beg)
					     4)
				     #\: 0))) 
	 (fetch-source-line file lnum)))
   (set! *gdb-annotate-reg-source*
	 (if (or *bee-client?* *emacs-client?*)
	     (bee-client-source-representation)
	     (user-source-representation))))

;*---------------------------------------------------------------------*/
;*    exited-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (exited-parser _)
   ;; we mark that we don't have stack frame anymore
   (set! *gdb-annotate-reg-stack-frame* #unspecified)
   #f)

;*---------------------------------------------------------------------*/
;*    signal-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (signal-parser _)
   ;; we mark that a signal has been raised
   (set! *gdb-annotate-reg-signal?* #t)
   #f)

;*---------------------------------------------------------------------*/
;*    signalled-parser ...                                             */
;*---------------------------------------------------------------------*/
(define (signalled-parser _)
   (signal-parser _)
   ;; the dummy line
   (let loop ((intro-text (gdb-read-line 1)))
      (if (not (string-prefix? #"signal-name\n" intro-text))
	  (loop (gdb-read-line 1))))
   (let ((sig (string-until (gdb-read-line 1) #\Newline 0)))
      (check-line signalled-parser:
		  #"signal-name-end" (gdb-read-line 1))
      ;; the command line
      (check-line signalled-parser: "," (gdb-read-line 1))
      ;; the signal string 
      (check-line signalled-parser:
		  #"signal-string" (gdb-read-line 1))
      (let ((msg (string-until (gdb-read-line 1) #\Newline 0)))
	 (console-error "Signal received: ")
	 (console-echo sig)
	 (console-echo " ")
	 (console-echo msg)
	 (console-newline)
	 (check-line signalled-parser:
		     #"signal-string-end" (gdb-read-line 1))
	 (gdb-read-line 1)))
   #f)

;*---------------------------------------------------------------------*/
;*    value-begin-parser ...                                           */
;*---------------------------------------------------------------------*/
(define (value-begin-parser line)
   (multiple-value-bind (the-value next-line)
      (value-parser (gdb-read-line 0) (string-char-after line #\space))
      (begin
	 (check-line value-begin-parser: #"value-end" next-line)
	 (if *gdb-annotate-print?*
	     (begin
		;; we only display the output if we are executing a
		;; print command (that is gdb-enable-print! has been
		;; invoked before the execution of the gdb command that
		;; as operated the print).
		(case *gdb-annotate-print?*
		   ((bigloo)
		    (console-echo (gdb-value->bdb-value the-value))
		    (console-newline))
		   ((explore)
		    (will-explore-value! the-value))
		   ((store)
		    (set! *gdb-annotate-print-store*
			  (cons the-value *gdb-annotate-print-store*)))
		   (else
		    (console-echo the-value)
		    (console-newline)))))))
   #f)

;*---------------------------------------------------------------------*/
;*    value-history-begin-parser ...                                   */
;*---------------------------------------------------------------------*/
(define (value-history-begin-parser line)
   (let* ((flag (string-char-after line #\space))
	  (hist-num (gdb-read-line 0)))
      (check-line 'value-history-begin-parser:
		  #"value-history-value\n" (gdb-read-line 1))
      (multiple-value-bind (value next-line)
	 (value-parser (gdb-read-line 0) flag)
	 (begin
	    (check-line 'value-history-begin-parser:
			#"value-history-end" next-line)
	    (case *gdb-annotate-print?*
	       ((bigloo)
		(console-echo (gdb-value->bdb-value value))
		(console-newline)
		#f)
	       ((explore)
		(will-explore-value! value)
		#f)
	       ((store)
		(set! *gdb-annotate-print-store*
		      (cons value *gdb-annotate-print-store*))
		#f)
	       (else
		value))))))

;*---------------------------------------------------------------------*/
;*    value-parser ...                                                 */
;*    -------------------------------------------------------------    */
;*    A value is either:                                               */
;*      - an plain string line                                         */
;*      - a struct ^Z^Zfield-begin                                     */
;*      - an array ^Z^Zarray-section-begin                             */
;*    All other markups are illegal.                                   */
;*---------------------------------------------------------------------*/
(define (value-parser line flag)
   (let loop ((line line)
	      (lst '()))
      (cond
	 ((string-prefix? #"" line)
	  (cond
	     ((string-prefix? #"field-begin" line)
	      (let ((field-value (field-begin-parser line)))
		 (loop (gdb-read-line 0)
		       (cons field-value lst))))
	     ((string-prefix? #"array-section-begin" line)
	      (let ((array-value (array-section-begin-parser line)))
		 (loop (gdb-read-line 0)
		       (cons array-value lst))))
	     (else
	      (let loop ((lst lst)
			 (res ""))
		 (if (null? lst)
		     (values res line)
		     (loop (cdr lst)
			   (string-append (car lst) res)))))))
	 ((string-prefix? #"\n" line)
	  (loop (gdb-read-line 0) lst))
	 (else
	  (loop (gdb-read-line 0) (cons line lst))))))

;*---------------------------------------------------------------------*/
;*    field-begin-parser ...                                           */
;*---------------------------------------------------------------------*/
(define (field-begin-parser line)
   (let* ((flag (string-char-after line #\space))
	  (name (gdb-read-line 0)))
      (check-line 'field-begin-parser:
		  #"field-name-end\n" (gdb-read-line 1))
      (let ((separator-string (gdb-read-line 1)))
	 (check-line 'field-begin-parser:
		     #"field-value\n" (gdb-read-line 1))
	 (multiple-value-bind (value next-line)
	    (value-parser (gdb-read-line 0) flag)
	    (begin
	       (check-line 'field-begin-parser: #"field-end" next-line)
	       (string-append "  " name " = " value #"\n  "))))))
	    
;*---------------------------------------------------------------------*/
;*    array-section-begin-parser ...                                   */
;*---------------------------------------------------------------------*/
(define (array-section-begin-parser line)
   (let ((flag (string-char-after line #\space))
	 (first (gdb-read-line 0)))
      (cond
	 ((string-prefix? #"array-section-end" first)
	  "")
	 (else
	  (multiple-value-bind (value next-line)
	     (value-parser first flag)
	     (let loop ((line next-line)
			(lst (list value)))
		(cond
		   ((string-prefix? #"array-section-end" line)
		    (let loop ((lst lst)
			       (res ""))
		       (if (null? lst)
			   res
			   (loop (cdr lst)
				 (string-append (car lst) res)))))
		   ((string=? line ", ")
		    (multiple-value-bind (value next-line)
		       (value-parser (gdb-read-line 0) flag)
		       (begin
			  (check-line array-section-begin-parser:
				      #"elt" next-line)
			  (loop (gdb-read-line 0)
				(cons* line value lst)))))
		   ((substring=? line ", " 2)
		    (multiple-value-bind (value next-line)
		       (value-parser (substring line 2 (string-length line))
				     flag)
		       (begin
			  (check-line array-section-begin-parser:
				      #"elt" next-line)
			  (loop (gdb-read-line 0)
				(cons* line value lst)))))
		   ((string-prefix? #"elt" line)
		    (loop (gdb-read-line 0)
			  lst))
		   ((string=? "" line)
		    (loop (gdb-read-line 0)
			  lst))
		   ((string-prefix? #"elt-rep" line)
		    (let* ((rep-num (string-until-at
				     line
				     (+fx (string-length #"elt-rep") 1)
				     #\Newline
				     0))
			   (rep-string (gdb-read-line 0)))
		       (check-line 'array-section-begin-parser:
				   #"elt-rep-end"
				   (gdb-read-line 0))
		       (loop (gdb-read-line 0)
			     (cons* rep-num rep-string lst))))
		   (else
		    (warning "bdb" "Illegal array -- [" line "]")
		    ""))))))))
	  
;*---------------------------------------------------------------------*/
;*    breakpoint-headers-parser ...                                    */
;*    -------------------------------------------------------------    */
;*    This function is the main function used to parser breakpoint     */
;*    table (e.g. the output of the "info break" command).             */
;*    -------------------------------------------------------------    */
;*    This function does not produce any output. It stores the         */
;*    breakpoint table in a variable that can be used by the command   */
;*    that invoked "info break".                                       */
;*---------------------------------------------------------------------*/
(define (breakpoint-headers-parser _)
   (let* ((table-markup         #"breakpoints-table\n")
	  (table-markup-len     (string-length table-markup))
	  (table-end-markup     #"breakpoints-table-end\n")
	  (table-end-markup-len (string-length table-end-markup))
	  (record-markup        #"record\n")
	  (record-markup-len    (string-length record-markup))
	  (field-markup         #"field")
	  (field-markup-len     (string-length field-markup)))
      (define (read-headers)
	 ;; read all the table headers
	 (let loop ((line     (gdb-read-line 1))
		    (headers '()))
	    (cond
	       ((substring=? field-markup line field-markup-len)
		(let ((new-header (string-until (gdb-read-line 1)
						#\Newline
						0)))
		   (loop (gdb-read-line 1)
			 (cons new-header headers))))
	       ((substring=? table-markup line table-markup-len)
		(gdb-read-line 1)
		(reverse! headers))
	       ((or (string=? line "") (char=? (string-ref line 0) #\Newline))
		(loop (gdb-read-line 1)
		      headers))
	       (else
		(error "breakpoint-headers-parser"
		       "Illegal header line"
		       (string-until line #\Newline 0))))))
      (define (read-record field-len)
	 ;; read one breakpoint record
	 (let loop ((i    0)
		    (vals '()))
	    (cond
	       ((=fx i field-len)
		;; we have to read addition informations
		(let loop ((line (gdb-read-line 1))
			   (vals vals))
		   (cond
		      ((or (substring=? record-markup line
					record-markup-len)
			   (substring=? table-end-markup line
					table-end-markup-len))
		       ;; we are done
		       (values (reverse! vals) line))
		      ((or (= (string-length line) 0)
			   (char=? (string-ref line 0) #\Newline)
			   (substring=? field-markup line
					field-markup-len))
		       (loop (gdb-read-line 1)
			     vals))
		      (else
		       (let ((value (string-until line #\Newline 0)))
			  (loop (gdb-read-line 1)
				(cons value vals)))))))
 	       (else
		(gdb-read-line 1)
		(loop (+fx i 1)
		      (cons (string-until (gdb-read-line 1) #\Newline 0)
			    vals))))))
      (define (read-records field-len)
	 ;; read all breakpoint records
	 (let loop ((line (gdb-read-line 1))
		    (res  '()))
	    (cond
	       ((substring=? record-markup line record-markup-len)
		(gdb-read-line 1)
		(multiple-value-bind (record next-line)
		   (read-record field-len)
		   (loop next-line
			 (cons record res))))
	       ((substring=? table-end-markup line table-end-markup-len)
		(reverse! res))
	       ((char=? (string-ref line 0) #\Newline)
		(loop (gdb-read-line 1)
		      res))
	       (else
		(error "breakpoint-records-parser"
		       "Illegal records line"
		       (string-until line #\Newline 0))))))
      (let* ((headers (read-headers))
	     (records (read-records (length headers))))
	 (set! *gdb-annotate-reg-breakpoint-table*
	       (cons headers records))))
   #f)

;*---------------------------------------------------------------------*/
;*    breakpoint-table-parser ...                                      */
;*    -------------------------------------------------------------    */
;*    If we find this markup it means that the user has requested      */
;*    an table breakpoint inspection before any breakpoint has been    */
;*    set.                                                             */
;*---------------------------------------------------------------------*/
(define (breakpoint-table-parser _)
   (set! *gdb-annotate-reg-breakpoint-table* 'no-breakpoint)
   #f)

;*---------------------------------------------------------------------*/
;*    pre-commands-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (pre-commands-parser _)
   (gdb-read-line 0))

;*---------------------------------------------------------------------*/
;*    display-begin-parser ...                                         */
;*    -------------------------------------------------------------    */
;*    This function accumulates all the table display. When the        */
;*    end markup is found, the table is then displayed.                */
;*---------------------------------------------------------------------*/
(define (display-begin-parser line)
   (let ((number (gdb-read-line 0)))
      (check-line 'display-begin-parser:
		  #"display-number-end\n" (gdb-read-line 1))
      (let ((number-separator (gdb-read-line 0)))
	 (check-line 'display-begin-parser:
		     #"display-format\n" (gdb-read-line 1))
	 (let ((format (gdb-read-line 0)))
	    (check-line 'display-begin-parser:
			#"display-expression\n" (gdb-read-line 1))
	    (let ((expression (gdb-read-line 0)))
	       (check-line 'display-begin-parser:
			   #"display-expression-end\n" (gdb-read-line 1))
	       (let ((expression-separator (gdb-read-line 0)))
		  ;; It seems that there is an error in GDB here. The
		  ;; documentation of 5.0 says that GDB emits a
		  ;; ^Z^Zdisplay-value but GDB 4.18 emits
		  ;; ^Z^Zdisplay-expression
		  ;; I don't care who's wrong, I simplify the check to
		  ;; ^Z^Zdisplay
		  (check-line 'display-begin-parser:
			      #"display" (gdb-read-line 1))
		  (let loop ((value (gdb-read-line 0)))
		     (let ((line (gdb-read-line 0)))
			(cond
			   ((string-prefix? #"display-end" line)
			    (set! *gdb-annotate-reg-display-table*
				  (cons (list number
					      format
					      expression
					      value)
					*gdb-annotate-reg-display-table*)))
			   ((string-prefix? #"\n" line)
			    (loop value))
			   (else
			    (loop (string-append value #"\n" line))))))))))))
	 
