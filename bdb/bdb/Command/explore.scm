;*=====================================================================*/
;*    serrano/prgm/project/bdk/kbdb/src/Command/explore.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  5 15:42:04 1999                          */
;*    Last change :  Thu Aug 10 11:38:05 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The "explore" command implementation.                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_explore
   (import  command_expr
	    tools_error
	    tools_tools
	    tools_io
	    patient_mangling
	    patient_invoke
	    patient_value
	    gdb_tools
	    gdb_invoke
	    gdb_annotate
	    engine_param
	    command_command)
   (export  (will-explore-value! ::bstring)
	    *bdb-explore-hook*
	    (holder->string addr::bstring)))

;*---------------------------------------------------------------------*/
;*    *bdb-explore-hook* ...                                           */
;*---------------------------------------------------------------------*/
(define *bdb-explore-hook* (lambda (s) s))
   
;*---------------------------------------------------------------------*/
;*    console-print ...                                                */
;*---------------------------------------------------------------------*/
(define (console-print . obj)
   (for-each console-echo obj)
   (console-newline))

;*---------------------------------------------------------------------*/
;*    get-frame-addresses ...                                          */
;*    -------------------------------------------------------------    */
;*    In order to avoid int max length problem the stack frame         */
;*    addresses are strings (instead of fixnums).                      */
;*---------------------------------------------------------------------*/
(define (get-frame-addresses i)
   (define (find-fp line)
      (if (substring=? line "Stack frame at " 15)
	  (substring line 15 (-fx (string-length line) 1))
	  #f))
   (define (find-sp line)
      (if (substring=? line " called by frame at " 19)
	  (substring line 19 (-fx (string-length line) 1))
	  #f))
   (define (find-fun line)
      (if (substring=? line " eip = " 7)
	  (string-until (string-from-at line 8 #\space 4) #\space 0)
	  #f))
   (let* ((cmd (string-append "info frame " (integer->string i)))
	  (res (let ((old-console-echo (get-console-echo-function)))
		  (set-console-echo-function! (lambda (x) x))
		  (let ((res (gdb-server->string cmd)))
		     (set-console-echo-function! old-console-echo)
		     res))))
      (with-input-from-string
       res
       (lambda ()
	  (let ((fp (find-fp (read-line))))
	     (if (not (string? fp))
		 (values #f #f #f)
		 (let ((fun (find-fun (read-line))))
		    (let ((sp (find-sp (read-line))))
		       (if (string? sp)
			   (values fp sp fun)
			   (values #f #f #f))))))))))

;*---------------------------------------------------------------------*/
;*    find-local-in-stack-frame ...                                    */
;*---------------------------------------------------------------------*/
(define (find-local-in-stack-frame i c-fun rbase)
   (define (find-var-name cmd fun)
      (let ((string (gdb-server->string cmd)))
	 (with-input-from-string
	  string
	  (lambda ()
	     (let loop ((line (read-line)))
		(if (string? line)
		    (begin
		       (string-case line
			  ((bol (: (submatch (: (in ("azAZ") #\_)
						(* (in ("09azAZ") "_"))))
				   " = "
				   (+ all)))
			   ;; this is a variable
			   (let* ((id (the-submatch 1))
				  (cmd (string-append "print &" id))
				  (addr (begin
					   (gdb-enable-store-print!)
					   (gdb-server->string cmd)
					   (let* ((store (get-gdb-print-store))
						  (str (string-from-at
							(car store)
							2
							#\)
							2)))
					      (gdb-disable-print!)
					      str))))
			      (if (string=? addr rbase)
				  (let ((s-id (bdb-demangle2 c-fun id)))
				     (if (string? s-id)
					 s-id
					 id))
				  (loop (read-line)))))))
		    #f))))))
   (console-echo "Holder        : ")
   ;; the variable name
   (let* ((s-fun (bdb-demangle c-fun))
	  (fun (if (string? s-fun) s-fun c-fun)))
      ;; the function name
      (console-echo-edit (string-append "function " fun) fun)
      (console-echo ", ")
      ;; the stack frame
      (let* ((snum (integer->string i))
	     (str (string-append "frame " snum)))
	 (console-echo-command str
			       "Unwind to frame"
			       (*bdb-explore-hook*
				(string-append "frame " snum)))
	 (let ((var-name (find-var-name "info locals" s-fun)))
	    (if (string? var-name)
		(begin
		   (console-echo " (")
		   (console-echo var-name)
		   (console-echo ")"))
		(let ((var-name (find-var-name "info args" s-fun)))
		   (if (string? var-name)
		       (begin
			  (console-echo " (")
			  (console-echo var-name)
			  (console-echo ")"))))))
	 (console-newline)
	 #t)))

;*---------------------------------------------------------------------*/
;*    holder->string ...                                               */
;*    -------------------------------------------------------------    */
;*    Deploy some refined heuristics in order to set a readable name   */
;*    associated to a GC root address.                                 */
;*---------------------------------------------------------------------*/
(define (holder->string rbase::bstring)
   (define (local-holder->string i c-fun rbase)
      (define (find-var-name cmd fun)
	 (let ((string (gdb-server->string cmd)))
	    (with-input-from-string
	     string
	     (lambda ()
		(let loop ((line (read-line)))
		   (if (string? line)
		       (begin
			  (string-case line
			     ((bol (: (submatch (: (in ("azAZ") #\_)
						   (* (in ("09azAZ") "_"))))
				      " = "
				      (+ all)))
			      ;; this is a variable
			      (let* ((id (the-submatch 1))
				     (cmd (string-append "print &" id))
				     (addr (begin
					      (gdb-enable-store-print!)
					      (gdb-server->string cmd)
					      (let* ((store (get-gdb-print-store))
						     (str (string-from-at
							   (car store)
							   2
							   #\)
							   2)))
						 (gdb-disable-print!)
						 str))))
				 (if (string=? addr rbase)
				     (let ((s-id (bdb-demangle2 c-fun id)))
					(if (string? s-id)
					    s-id
					    id))
				     (loop (read-line)))))))
		       #f))))))
      ;; the variable name
      (let* ((s-fun (bdb-demangle c-fun))
	     (fun (if (string? s-fun) s-fun c-fun))
	     (res (string-append fun " "))
	     (var-name (find-var-name "info locals" s-fun)))
	 (if (string? var-name)
	     (set! res (string-append res " (" var-name ")"))
	     (let ((var-name (find-var-name "info args" s-fun)))
		(if (string? var-name)
		    (set! res (string-append res " (" var-name ")")))))
	 res))
   (define (global-holder->string rbase)
      (if (substring=? rbase "0x" 2)
	  (let* ((cmd (string-append "info symbol " rbase))
		 (str (gdb-server->string cmd)))
	     (string-case str
		((: (submatch (+ (out #\space))) " in " (+ all))
		 ;; great we just have to demangle the identifier
		 (let* ((c-id (the-submatch 1))
			(s-id (bdb-demangle c-id)))
 		    (if (string? s-id)
			s-id
			c-id)))
		(else
		 #f)))
	  #f))
   (define (stack-holder->string rbase)
      ;; hum, the value is held by a local variable. First we have
      ;; to find the correct stack frame. Then, inside that frame,
      ;; the variable.
      (let loop ((i 0))
	 (multiple-value-bind (fp sp fun)
	    (get-frame-addresses i)
	    (cond
	       ((not (and (string? fp) (string? sp)))
		;; we have scan all the stack without finding the stack frame
		#f)
	       ((or (and (string>=? fp rbase) (string<=? sp rbase))
		    (and (string<=? fp rbase) (string>=? sp rbase)))
		;; we get it
		(local-holder->string i fun rbase))
	       (else       
		;; not in that frame
		(loop (+fx i 1)))))))
   (cond
      ((string=? rbase "_")
       ;; there is no string associated to an unknown data
       #f)
      ((or (string=? rbase "reg") (string=? rbase "0"))
       ;; this is a register the holder is the current stack frame
       "frame 0")
      (else
       (let ((global (global-holder->string rbase)))
	  (if (string? global)
	      global
	      (let ((stack (stack-holder->string rbase)))
		 (if (string? stack)
		     stack
		     "frame 0")))))))

;*---------------------------------------------------------------------*/
;*    explore-tty-echo ...                                             */
;*---------------------------------------------------------------------*/
(define (explore-tty-echo explore)
   (define (explore-root-global-variable rbase)
      (if (substring=? rbase "0x" 2)
	  (let* ((cmd (string-append "info symbol " rbase))
		 (str (gdb-server->string cmd)))
	     (string-case str
		((: (submatch (+ (out #\space))) " in " (+ all))
		 ;; great we just have to demangle the identifier
		 (console-echo "Holder        : ")
		 (let* ((c-id (the-submatch 1))
			(s-id (bdb-demangle c-id)))
 		    (if (string? s-id)
			(console-echo-edit s-id s-id)
			(console-echo-edit c-id c-id))
		    (console-newline)
		    #t))
		(else
		 #f)))
	  #f))
   (define (explore-root-stack rbase)
      ;; hum, the value is held by a local variable. First we have
      ;; to find the correct stack frame. Then, inside that frame,
      ;; the variable.
      (let loop ((i 0))
	 (multiple-value-bind (fp sp fun)
	    (get-frame-addresses i)
	    (cond
	       ((not (and (string? fp) (string? sp)))
		;; we have scan all the stack without finding the stack frame
		(console-print "Holder        : ???")
		#t)
	       ((or (and (string>=? fp rbase) (string<=? sp rbase))
		    (and (string<=? fp rbase) (string>=? sp rbase)))
		;; we get it
		(find-local-in-stack-frame i fun rbase))
	       (else       
		;; not in that frame
		(loop (+fx i 1)))))))
   (define (explore-root-register rbase)
      ;; when the value is held by a register there is not much to be done
      (console-print "Holder        : frame 0")
      #f)
   (define (explore-root rbase)
      (cond
	 ((string=? rbase "_")
	  'nothing)
	 ((string=? rbase "reg")
	  ;; the root is a register in the first stack frame
	  (explore-root-register rbase))
	 (else
	  ;; rbase is an adress, for which we try to associate an
	  ;; identifier. With use two succesive strategy.
	  ;;   1- we check if the root is a global variable
	  ;;   2- we check if it is a stack address
	  ;;   3- it is a register
	  (if (not (explore-root-global-variable rbase))
	      (if (not (explore-root-stack rbase))
		  (explore-root-register rbase)))
	  #f)))
   (define (explore-link-chain/base link base)
      (for-each (lambda (x)
		   (console-echo "   ")
		   (if (substring=? "0x" x 2)
		       (let ((base (string-until x #\space 0)))
			  (console-echo-explore base base)
			  (console-echo (string-from x #\space 0)))
		       (console-echo x))
		   (console-newline))
		link))
   (define (explore-link-chain/w-base link base)
      (for-each (lambda (x)
		   (console-echo "   ")
		   (let ((str (string-from x #\[ 0)))
		      (if (substring=? "0x" x 2)
			  (let ((base (string-until x #\space 0)))
			     (console-echo-explore str base))
			  (console-echo str)))
		   (console-newline))
		link))
   (define (explore-link-chain link base)
      (if *base-address?*
	  (explore-link-chain/base link base)
	  (explore-link-chain/w-base link base)))
   (define (id->lambda id)
      ;; if the identifier is a lambda identifier, we nice it
      (if (substring=? id "<ANONYMOUS:" 11)
	  ;; yes, that's one
	  (string-append "<LAMBDA:" (string-from-at id 11 #\: 1))
	  id))
   (match-case explore
      ((?kind ?gen ?size ?fun ?lnum ?type ?link ?base (?file . ?line) ?rbase)
       (case kind
	  ((0)
	   (console-print "Unreferenced object! (0x"
			  (integer->string base 16) ")"))
	  ((1)
	   (console-print "Undebugged object! (0x"
			  (integer->string base 16) ")"))
	  (else
	   (explore-root rbase)
	   (console-echo "Type          : ")
	   (if (string=? type "<STRING BUFFER>")
	       (console-print "<STRING BUFFER> (0x"
			      (integer->string base 16)
			      ")")
	       (begin
		  (console-echo-dprint type
				       (string-append
					"0x"
					(integer->string base 16)))
		  (console-newline)))
	   (console-echo "Producer      : ")
	   (cond
	      ((substring=? fun "BDB:" 4)
	       (console-print fun))
	      ((=fx line 0)
	       (console-echo-edit (id->lambda fun) fun)
	       (console-newline))
	      (else
	       (let ((edit (string-append file ":" (integer->string line))))
		  (console-echo-edit (id->lambda fun) edit)
		  (console-newline))))
	   (console-print "GC birth date : " (integer->string gen))
	   (console-print "Size in bytes : " (integer->string size))
	   (if *base-address?*
	       (begin
		  (console-echo "base address  : " )
		  (if (string=? type "<STRING BUFFER>")
		      (console-print "0x" (integer->string base 16))
		      (let ((num (string-append "0x"
						(integer->string base 16))))
			 (console-echo-dprint num num)
			 (console-newline)))))
	   (explore-link-chain link base))))
      (else
       (error "explore"
	      "Internal error (illegal explore format)"
	      explore))))

;*---------------------------------------------------------------------*/
;*    *bigloo-type-shape* ...                                          */
;*---------------------------------------------------------------------*/
(define *bigloo-type-shape* #unspecified)

;*---------------------------------------------------------------------*/
;*    *will-explore-value* ...                                         */
;*---------------------------------------------------------------------*/
(define *will-explore-value* #unspecified)

;*---------------------------------------------------------------------*/
;*    will-explore-value! ...                                          */
;*---------------------------------------------------------------------*/
(define (will-explore-value! value::bstring)
   (set! *will-explore-value* value))

;*---------------------------------------------------------------------*/
;*    explore-value ...                                                */
;*---------------------------------------------------------------------*/
(define (explore-value value::bstring)
   (define (one-bigloo-type? shapes exp)
      (let loop ((shapes shapes))
	 (cond
	    ((null? shapes)
	     #f)
	    ((substring=? (car shapes)
			  value
			  (string-length (car shapes)))
	     (loop (cdr shapes)))
	    (else
	     #t))))
   (if (not (pair? *bigloo-type-shape*))
       (set! *bigloo-type-shape*
	     (map (lambda (t) (string-append "(" t ")")) *bigloo-type*)))
   (if (not (one-bigloo-type? *bigloo-type-shape* value))
       (begin
	  (console-echo "No heap value")
	  (console-newline))
       (let ((len (string-length value)))
	  (let loop ((i (-fx len 1)))
	     (let ((c (string-ref value i)))
		(if (or (char=? c #\space) (char=? c #\)))
		    (let* ((addr (substring value (+fx i 1) len))
			   (explore (patient-call "bdb_heap_explore"
						  addr
						  (if (>=fx *verbose* 4)
						      "1"
						      "0"))))
		       (explore-tty-echo explore))
 		    (loop (-fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    explore-parser ...                                               */
;*---------------------------------------------------------------------*/
(define (explore-parser cmd-list source level env)
   (set! *will-explore-value* #f)
   (if (null? cmd-list)
       (bdb-notify-error "explore" "Missing arguments" source)
       (let* ((bdb-expr (string-from-at source 1 #\space 1))
	      (gdb-expr (bdb-expr->gdb-expr bdb-expr
					    (gdb-annotate-current-function))))
	  ;; if bdb-expr and gdb-expr are the same it is a real exploring...
	  (gdb-enable-explore-print!)
	  (let ((cmd (string-append "print " gdb-expr)))
	     (gdb-call->string cmd)
	     (if (string? *will-explore-value*)
		 (begin
		    (explore-value *will-explore-value*)
		    (set! *will-explore-value* #f))))
	  (gdb-disable-print!))))

;*---------------------------------------------------------------------*/
;*    explore-value-offset ...                                         */
;*---------------------------------------------------------------------*/
(define (explore-value-offset addr::bstring)
   (let ((explore (patient-call "bdb_heap_explore_offset"
				addr
				(if (>=fx *verbose* 4) "1" "0"))))
      (explore-tty-echo explore)))
   
;*---------------------------------------------------------------------*/
;*    explore-offset-parser ...                                        */
;*---------------------------------------------------------------------*/
(define (explore-offset-parser cmd-list source level env)
   (set! *will-explore-value* #f)
   (if (null? cmd-list)
       (bdb-notify-error "explore_offset" "Missing arguments" source)
       (let ((gdb-expr (string-from-at source 1 #\space 1)))
	  (gdb-enable-explore-print!)
	  (let ((cmd (string-append "print " gdb-expr)))
	     (gdb-call->string cmd)
	     (if (string? *will-explore-value*)
		 (begin
		    (explore-value-offset *will-explore-value*)
		    (set! *will-explore-value* #f))))
	  (gdb-disable-print!))))

;*---------------------------------------------------------------------*/
;*    *explore-help* ...                                               */
;*---------------------------------------------------------------------*/
(define *explore-help*
   "Explore in the heap the value of expression EXP.
Variables accessible are those of the lexical environment of the selected
stack frame, plus all those whose scope is global or an entire file.")

;*---------------------------------------------------------------------*/
;*    *explore-offset-help* ...                                        */
;*---------------------------------------------------------------------*/
(define *explore-offset-help*
   "Private command.")

;*---------------------------------------------------------------------*/
;*    The print command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "explore"
			3
			explore-parser
			*explore-help*)

;*---------------------------------------------------------------------*/
;*    The print command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "explore_offset"
			9
			explore-offset-parser
			*explore-offset-help*)

