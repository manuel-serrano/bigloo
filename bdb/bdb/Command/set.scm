;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/set.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec  3 09:10:58 1999                          */
;*    Last change :  Fri Dec  9 11:12:52 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The set command                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_set
   (include "Engine/hooks.sch")
   (import  command_command
	    command_parse
	    engine_param
	    engine_engine
	    tools_io
	    patient_server
	    gdb_invoke
	    (bdb-error tools_error))
   (export  (set-mixte-mode!)
	    (set-scheme-mode!)
	    (set-c-mode!)))

;*---------------------------------------------------------------------*/
;*    set-mixte-mode! ...                                              */
;*---------------------------------------------------------------------*/
(define (set-mixte-mode!)
   (if *bdb-scheme-mode-enabled?*
       (begin
	  (set! *bdb-mode* 'mixte)
	  (set! *heap-explorer?* #t)
	  #t)
       (begin
	  (warning "Bdb facilities unavailable"
		   "Can't set `mixte' mode -- "
		   #"See -gbdb Bigloo option\n")
	  #f)))

;*---------------------------------------------------------------------*/
;*    set-scheme-mode! ...                                             */
;*---------------------------------------------------------------------*/
(define (set-scheme-mode!)
   (if *bdb-scheme-mode-enabled?*
       (begin
	  (set! *bdb-mode* 'scheme)
	  (set! *heap-explorer?* #t)
	  #t)
       (begin
	  (warning "Bdb facilities unavailable"
		   "Can't set `scheme' mode -- "
		   #"See -gbdb Bigloo option\n")
	  #f)))

;*---------------------------------------------------------------------*/
;*    set-c-mode! ...                                                  */
;*---------------------------------------------------------------------*/
(define (set-c-mode!)
   (set! *bdb-mode* 'c)
   (set! *heap-explorer?* #f))
   
;*---------------------------------------------------------------------*/
;*    default-parser ...                                               */
;*---------------------------------------------------------------------*/
(define (default-parser cmd-list source level env)
   (match-case cmd-list
      (()
       (console-echo (gdb-call->string source))
       (console-echo (string-append "mode: The Bdb mode is "
			(symbol->string *bdb-mode*)))
       (console-newline))
      ((?id . ?rest)
       (let ((cmd (find-command id env)))
	  (if (not (isa? cmd command))
	      ;; if we do not override the command, simply calls gdb
	      (console-echo (gdb-call->string source))
	      (let ((cmd::command cmd))
		 ((-> cmd parser) rest
				  source
				  (+fx level 1)
				  (-> cmd env))))))))

;*---------------------------------------------------------------------*/
;*    set-parser ...                                                   */
;*---------------------------------------------------------------------*/
(define (set-parser cmd-list source level env)
   (define (err mode)
      (bdb-error "set" "\"mixte\" or \"scheme\" or \"c\" expected." mode))
   (if (pair? cmd-list)
       (if (string? (car cmd-list))
	   (let ((mode (string->symbol (string-upcase (car cmd-list)))))
	      (case mode
		 ((mixte)
		  (set-mixte-mode!))
		 ((scheme)
		  (set-scheme-mode!))
		 ((c)
		  (set-c-mode!))
		 (else
		  (err mode))))
	   (err (car cmd-list)))))

;*---------------------------------------------------------------------*/
;*    show-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (show-parser _)
   (console-echo "Mode is ")
   (console-echo (symbol->string *bdb-mode*))
   (console-echo #".\n"))

;*---------------------------------------------------------------------*/
;*    set-trace-parser ...                                             */
;*---------------------------------------------------------------------*/
(define (set-trace-parser cmd-list source level env)
   (define (err mode)
      (bdb-error "set" "\"mixte\" or \"scheme\" or \"c\" expected." mode))
   (if (pair? cmd-list)
       (if (string? (car cmd-list))
	   (let ((value (string->integer (car cmd-list))))
	      (set! *verbose* value))
	   (err (car cmd-list)))))

;*---------------------------------------------------------------------*/
;*    show-trace-parser ...                                            */
;*---------------------------------------------------------------------*/
(define (show-trace-parser _)
   (console-echo "Mode is ")
   (console-echo (number->string *verbose*))
   (console-echo #".\n"))

;*---------------------------------------------------------------------*/
;*    *help-set* ...                                                   */
;*---------------------------------------------------------------------*/
(define *help-set*
   "help set
Evaluate expression EXP and assign result to variable VAR, using assignment
syntax appropriate for the current language (VAR = EXP or VAR := EXP for
example).  VAR may be a debugger \"convenience\" variable (names starting
with $), a register (a few standard names starting with $), or an actual
variable in the program being debugged.  EXP is any valid expression.
Use \"set variable\" for variables with names identical to set subcommands.

With a subcommand, this command modifies parts of the gdb environment.
You can see these environment settings with the \"show\" command.

List of set subcommands:

set annotate -- Set annotation_level
set architecture -- Set architecture of target
set args -- Set argument list to give program being debugged when it is started
set auto-solib-add -- Set autoloading of shared library symbols
set check -- Set the status of the type/range checker
set complaints -- Set max number of complaints about incorrect symbols
set confirm -- Set whether to confirm potentially dangerous operations
set demangle-style -- Set the current C++ demangling style
set editing -- Set editing of command lines as they are typed
set endian -- Set endianness of target
set environment -- Set environment variable value to give the program
set gnutarget -- Set the current BFD target
set height -- Set number of lines gdb thinks are in a page
set history -- Generic command for setting command history parameters
set input-radix -- Set default input radix for entering numbers
set language -- Set the current source language
set listsize -- Set number of source lines gdb will list by default
set output-radix -- Set default output radix for printing of values
set print -- Generic command for setting how things print
set prompt -- Set gdb's prompt
set radix -- Set default input and output number radices
set remotebaud -- Set baud rate for remote serial I/O
set remotebreak -- Set whether to send break if interrupted
set remotecache -- Set cache use for remote targets
set remotedebug -- Set debugging of remote protocol
set remotedevice -- Set device for remote serial I/O
set remotelogbase -- Set 
set remotelogfile -- Set filename for remote session recording
set remotetimeout -- Set timeout limit to wait for target to respond
set remotewritesize -- Set the maximum number of bytes in each memory write packet
set solib-absolute-prefix -- Set prefix for loading absolute shared library symbol files
set solib-search-path -- Set the search path for loading non-absolute shared library symbol files
set stop-on-solib-events -- Set stopping for shared library events
set symbol-reloading -- Set dynamic symbol table reloading multiple times in one run
set targetdebug -- Set target debugging
set variable -- Evaluate expression EXP and assign result to variable VAR
set verbose -- Set verbosity
set watchdog -- Set watchdog timer
set width -- Set number of characters gdb thinks are in a line
set write -- Set writing into executable and core files
set mode -- Set the Bdb execution mode
set btrace -- Set the Bdb tracing mode")

;*---------------------------------------------------------------------*/
;*    *help-show*                                                      */
;*---------------------------------------------------------------------*/
(define *help-show*
   "Generic command for showing things about the debugger.

List of show subcommands:

show annotate -- Show annotation_level
show architecture -- Show architecture of target
show args -- Show argument list to give program being debugged when it is started
show auto-solib-add -- Show autoloading of shared library symbols
show check -- Show the status of the type/range checker
show commands -- Show the history of commands you typed
show complaints -- Show max number of complaints about incorrect symbols
show confirm -- Show whether to confirm potentially dangerous operations
show convenience -- Debugger convenience (\"$foo\") variables
show copying -- Conditions for redistributing copies of GDB
show demangle-style -- Show the current C++ demangling style
show directories -- Current search path for finding source files
show editing -- Show editing of command lines as they are typed
show endian -- Show endianness of target
show environment -- The environment to give the program
show gnutarget -- Show the current BFD target
show height -- Show number of lines gdb thinks are in a page
show history -- Generic command for showing command history parameters
show input-radix -- Show default input radix for entering numbers
show language -- Show the current source language
show listsize -- Show number of source lines gdb will list by default
show output-radix -- Show default output radix for printing of values
show paths -- Current search path for finding object files
show print -- Generic command for showing print settings
show prompt -- Show gdb's prompt
show radix -- Show the default input and output number radices
show remotebaud -- Show baud rate for remote serial I/O
show remotebreak -- Show whether to send break if interrupted
show remotecache -- Show cache use for remote targets
show remotedebug -- Show debugging of remote protocol
show remotedevice -- Show device for remote serial I/O
show remotelogbase -- Show 
show remotelogfile -- Show filename for remote session recording
show remotetimeout -- Show timeout limit to wait for target to respond
show remotewritesize -- Show the maximum number of bytes in each memory write packet
show solib-absolute-prefix -- Show prefix for loading absolute shared library symbol files
show solib-search-path -- Show the search path for loading non-absolute shared library symbol files
show stop-on-solib-events -- Show stopping for shared library events
show symbol-reloading -- Show dynamic symbol table reloading multiple times in one run
show targetdebug -- Show target debugging
show user -- Show definitions of user defined commands
show values -- Elements of value history around item number IDX (or last ten)
show verbose -- Show verbosity
show version -- Show what version of GDB this is
show warranty -- Various kinds of warranty you do not have
show watchdog -- Show watchdog timer
show width -- Show number of characters gdb thinks are in a line
show write -- Show writing into executable and core files
show mode -- Show the Bdb execution mode
show btrace -- Show the Bdb tracing mode.")
   

;*---------------------------------------------------------------------*/
;*    the set command                                                  */
;*---------------------------------------------------------------------*/
(let ((set (bind-toplevel-command! "set"
				   3
				   default-parser
				   *help-set*)))
   (bind-sub-command! set "mode" 3 set-parser "Set Bdb mode.")
   (bind-sub-command! set "btrace" 3 set-trace-parser "Set Bdb trace mode."))

;*---------------------------------------------------------------------*/
;*    the show command                                                 */
;*---------------------------------------------------------------------*/
(let ((show (bind-toplevel-command! "show"
				    3
				    default-parser
				    *help-show*)))
   (bind-sub-command! show "mode" 3 (make-stop-parse show-parser) "Bdb mode.")
   (bind-sub-command! show "btrace" 3 (make-stop-parse show-trace-parser) "Bdb trace mode."))


