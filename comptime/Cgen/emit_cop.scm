;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Cgen/emit_cop.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  2 14:39:37 1996                          */
;*    Last change :  Thu Apr 19 09:05:06 2018 (serrano)                */
;*    Copyright   :  1996-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The emission of cop code.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_emit-cop
   (include "Tools/location.sch"
	    "Tools/fprint.sch"
	    "Tools/trace.sch")
   (import  type_type
	    type_tools
	    type_cache
	    type_typeof
	    tools_shape
	    engine_param
	    ast_var
	    ast_node
	    ast_env
	    backend_c_emit
	    cgen_cop
	    (*module-location* module_module))
   (export  (generic emit-cop::bool ::cop)
	    (reset-bdb-loc!)
	    (emit-bdb-loc ::obj)
	    (get-current-bdb-loc)))

;*---------------------------------------------------------------------*/
;*    emit-cop ...                                                     */
;*    -------------------------------------------------------------    */
;*    If emit-cop emit an expression with a `;' it returns #f,         */
;*    otherwise it returns #t.                                         */
;*    -------------------------------------------------------------    */
;*    The general idea of that printer is that no specific printer     */
;*    ever emit \n because they are emitted by the location printer.   */
;*---------------------------------------------------------------------*/
(define-generic (emit-cop::bool cop::cop))
 
;*---------------------------------------------------------------------*/
;*    emit-cop ::clabel ...                                            */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::clabel)
   (with-access::clabel cop (used? name body loc)
      (if used?
	  (begin
	     (emit-bdb-loc loc)
	     (display name *c-port*)
	     (write-char #\: *c-port*)))
      (emit-cop body)))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cgoto ...                                             */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cgoto)
   (with-access::cgoto cop (label loc)
      (emit-bdb-loc loc)
      (fprin *c-port* "goto " (clabel-name label) #\;)
      #f))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cblock ...                                            */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cblock)
   (with-access::cblock cop (body loc)
      (emit-bdb-loc loc)
      (if (isa? body cblock)
	  (emit-cop body)
	  (begin
	     (display #"{ " *c-port*)
	     (trace cgen (display "/* cop-block */" *c-port*))
	     (emit-bdb-loc-comment loc)
	     (if (emit-cop body)
		 (begin
		    (emit-bdb-loc (get-current-bdb-loc))
		    (display "; " *c-port*)
		    (trace cgen (display "/* cop-block */" *c-port*))))
	     (display "} " *c-port*)
	     (trace cgen (display "/* cop-block */" *c-port*))
	     #f))))

;*---------------------------------------------------------------------*/
;*    emit-cop ::creturn ...                                           */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::creturn)
   (with-access::creturn cop (value loc)
      (emit-bdb-loc loc)
      (display "return " *c-port*)
      (if (emit-cop value)
	  (write-char #\; *c-port*))
      #f))

;*---------------------------------------------------------------------*/
;*    emit-cop ::catom ...                                             */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::catom)
   (with-access::catom cop (value)
      (emit-atom-value value)
      #t))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cvoid ...                                             */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cvoid)
   (with-access::cvoid cop (value)
      (emit-cop value)))

;*---------------------------------------------------------------------*/
;*    emit-cop ::varc ...                                              */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::varc)
   (with-access::varc cop (variable)
      (if (when (isa? variable global)
	     (with-access::global  variable (value)
		(when (isa? value scnst)
		   (with-access::scnst value (class)
		      (eq? class 'sreal)))))
	  (begin
	     (display "BGL_REAL_CNST( " *c-port*)
	     (display (variable-name variable) *c-port*)
	     (display ")" *c-port*))
	  (display (variable-name variable) *c-port*))
      #t))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cpragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cpragma)
   (with-access::cpragma cop (args format loc)
      (emit-bdb-loc loc)
      (if (null? args)
	  (display format *c-port*)
	  (let* ((sport  (open-input-string format))
		 (args   (list->vector args))
		 (parser (regular-grammar ()
			    ((: #\$ (+ (in (#\0 #\9))))
			     (let* ((str   (the-string))
				    (len   (the-length))
				    (index (string->number
					    (substring str 1 len))))
				(emit-cop (vector-ref args (-fx index 1)))
				(ignore)))
			    ("$$"
			     (display "$" *c-port*)
			     (ignore))
			    ((+ (out #\$))
			     (display (the-string) *c-port*)
			     (ignore))
			    (else
			     (the-failure)))))
	     (read/rp parser sport)
	     (close-input-port sport)
	     #t))))

;*---------------------------------------------------------------------*/
;*    emit-cop ::ccast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::ccast)
   (with-access::ccast cop (arg type loc)
      (emit-bdb-loc loc)
      (display "((" *c-port*)
      (display (type-name type) *c-port*)
      (write-char #\) *c-port*)
      (emit-cop arg)
      (write-char #\) *c-port*)
      #t))

;*---------------------------------------------------------------------*/
;*    emit-cop ::csequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::csequence)
   (with-access::csequence cop (c-exp? cops loc)
      (if c-exp?
	  (begin
	     (if (null? cops)
		 (emit-atom-value #unspecified)
		 (begin
		    (display "( " *c-port*)
		    (trace cgen (display "/* cop-csequence */" *c-port*))
		    (let liip ((exp cops))
		       (if (null? (cdr exp))
			   (begin
			      (emit-cop (car exp))
			      (display ") " *c-port*)
			      (trace cgen "/* cop-csequence */" *c-port*)
			      #t)
			   (begin
			      (emit-cop (car exp))
			      (if (cfail? (car exp))
				  (begin
				     (display ") " *c-port*)
				     (trace cgen  "/* cop-csequence */" *c-port*)
				     #t)
				  (begin
				     (display ", " *c-port*)
				     (trace cgen "/* cop-csequence */" *c-port*)
				     (liip (cdr exp))))))))))
	  (let liip ((exp cops))
	     (if (null? exp)
		 #f
		 (let ((e (car exp)))
		    (if (emit-cop e)
			(begin
			   (display "; " *c-port*)
			   (trace cgen
				  (display "/* cop-csequence */" *c-port*))))
		    (if (cfail? e)
			(liip '())
			(liip (cdr exp)))))))))

;*---------------------------------------------------------------------*/
;*    emit-cop ::nop ...                                               */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::nop)
   (with-access::nop cop (loc)
      (display "; " *c-port*)
      (trace cgen (display "/* cop-nop */" *c-port*))
      #f))

;*---------------------------------------------------------------------*/
;*    emit-cop ::stop ...                                              */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::stop)
   (with-access::stop cop (value loc)
      (if (emit-cop value)
	  ;; we don't have to emit a location here because the
	  ;; location is held by the printed value
	  (begin
	     (display "; " *c-port*)
	     (trace cgen (display "/* cop-stop */" *c-port*))))
      #f))

;*---------------------------------------------------------------------*/
;*    emit-cop ::csetq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::csetq)
   (with-access::csetq cop (var value loc)
      ;; we first emit a location for this node
      (emit-bdb-loc loc)
      (emit-cop var)
      ;; don't omit to put space sourrounding `=' otherwise
      ;; it could become an ambiguous assignement (e.g. x=-1).
      (display " = " *c-port*)
      (emit-cop value)
      #t))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cif ...                                               */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cif)
   (with-access::cif cop (test true false loc)
      (emit-bdb-loc loc)
      (display "if(" *c-port*)
      (emit-cop test)
      (write-char #\) *c-port*)
      (emit-cop true)
      (display " else " *c-port*)
      (emit-cop false)))

;*---------------------------------------------------------------------*/
;*    emit-cop ::local ...                                             */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::local-var)
   (with-access::local-var cop (vars loc)
      (emit-bdb-loc loc)
      (for-each (lambda (local)
		   (fprin *c-port*
			  (make-typed-declaration (local-type local)
						  (local-name local))
			  (if (and (>fx *bdb-debug* 0)
				   (eq? (type-class (local-type local))
					'bigloo))
			      (string-append " = (("
					     (make-typed-declaration
					      (local-type local)
					      "")
					     ")BUNSPEC)")
			      "")
			  #\;))
		vars)
      #f))

;*---------------------------------------------------------------------*/
;*    emit-cop ::bdb-block ...                                         */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::bdb-block)
   (with-access::bdb-block cop (loc body)
      (emit-bdb-loc loc)
      (fprin *c-port* "int bigloo_dummy_bdb; bigloo_dummy_bdb = 0; { ")
      (emit-cop body)
      (display "} " *c-port*)
      (trace cgen (display " /* cop-bdb-block */" *c-port*))))
   
;*---------------------------------------------------------------------*/
;*    emit-cop ::cfuncall ...                                          */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cfuncall)
   
   (define (out-call op cop)
      (if (eq? (cfuncall-type cop) *obj*)
	  (begin
	     (display op *c-port*)
	     (display "(" *c-port*)
	     (emit-cop (cfuncall-fun cop))
	     (display ")(" *c-port*))
	  (begin
	     (display "((" *c-port*)
	     (display (type-name (cfuncall-type cop)) *c-port*)
	     (display "(*)())" *c-port*)
	     (display op *c-port*)
	     (display "(" *c-port*)
	     (emit-cop (cfuncall-fun cop))
	     (display "))(" *c-port*))))

   (labels ((emit-extra-light-cfuncall (cop)
               (let ((actuals (cfuncall-args cop)))
		  (emit-cop (cfuncall-fun cop))
		  (write-char #\( *c-port*)
		  (let loop ((actuals actuals))
		     ;; actuals are never empty because their are always
		     ;; the EOA.
		     (if (null? (cddr actuals))
			 (begin
			    (emit-cop (car actuals))
			    (write-char #\) *c-port*)
			    #t)
			 (begin
			    (emit-cop (car actuals))
			    (display ", " *c-port*)
			    (loop (cdr actuals)))))))
	    (emit-light-cfuncall (cop)
               (let ((actuals (cfuncall-args cop)))
		  (out-call "PROCEDURE_L_ENTRY" cop)
		  (let loop ((actuals actuals))
		     ;; actuals are never empty because their are always
		     ;; the function and EOA.
		     (if (null? (cddr actuals))
			 (begin
			    (emit-cop (car actuals))
			    (display ")" *c-port*)
			    #t)
			 (begin
			    (emit-cop (car actuals))
			    (display ", " *c-port*)
			    (loop (cdr actuals)))))))
	    (emit-regular-cfuncall/eoa (cop)
	       (let ((actuals (cfuncall-args cop)))
		  (out-call "PROCEDURE_ENTRY" cop)
		  (let loop ((actuals actuals))
		     ;; actuals are never empty because their are always
		     ;; the function and EOA.
		     (if (null? (cdr actuals))
			 (begin
			    (emit-cop (car actuals))
			    (display ")" *c-port*)
			    #t)
			 (begin
			    (emit-cop (car actuals))
			    (display ", " *c-port*)
			    (loop (cdr actuals)))))))
	    (emit-regular-cfuncall/oeoa (cop)
	       (let ((actuals (cfuncall-args cop)))
		  (out-call "PROCEDURE_ENTRY" cop)
		  (let loop ((actuals actuals))
		     ;; actuals are never empty because their are always
		     ;; the function and EOA.
		     (if (null? (cddr actuals))
			 (begin
			    (emit-cop (car actuals))
			    (display ")" *c-port*)
			    #t)
			 (begin
			    (emit-cop (car actuals))
			    (display ", " *c-port*)
			    (loop (cdr actuals)))))))
	    (emit-stdc-regular-cfuncall (cop)
	       (begin
		  (display "(VA_PROCEDUREP( " *c-port*)
		  (emit-cop (cfuncall-fun cop))
		  (display " ) ? " *c-port*)
		  (emit-regular-cfuncall/eoa cop)
		  (display " : " *c-port*)
		  (emit-regular-cfuncall/oeoa cop)
		  (display " )" *c-port*)
		  #t)))
      (emit-bdb-loc (cop-loc cop))
      (case (cfuncall-strength cop)
	 ((elight)
	  (emit-extra-light-cfuncall cop))
	 ((light)
	  (emit-light-cfuncall cop))
	 (else
	  (if *stdc*
	      (emit-stdc-regular-cfuncall cop)
	      (emit-regular-cfuncall/eoa cop))))))

;*---------------------------------------------------------------------*/
;*    emit-cop ::capply ...                                            */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::capply)
   (with-access::capply cop (fun arg loc)
      (emit-bdb-loc loc)
      (display "apply(" *c-port*)
      (emit-cop fun)
      (display ", " *c-port*)
      (emit-cop arg)
      (write-char #\) *c-port*)
      #t))
	     
;*---------------------------------------------------------------------*/
;*    emit-cop ::capp ...                                              */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::capp)
   
   (define (emit-infix-capp)
      (let ((actuals (capp-args cop)))
	 (write-char #\( *c-port*)
	 (cond
	    ((null? actuals)
	     (emit-cop (capp-fun cop)))
	    ((null? (cdr actuals))
	     (emit-cop (car actuals))
	     (emit-cop (capp-fun cop)))
	    ((null? (cddr actuals))
	     (emit-cop (car actuals))
	     (emit-cop (capp-fun cop))
	     (emit-cop (cadr actuals)))
	    (else
	     (error "emit-cop" "Illegal infix macro"
		(shape (varc-variable (capp-fun cop))))))
	 (write-char #\) *c-port*)
	 #t))
   
   (define (emit-prefix-capp)
      (let ((actuals (capp-args cop)))
	 (emit-cop (capp-fun cop))
	 (write-char #\( *c-port*)
	 (if (null? actuals)
	     (begin
		(write-char #\) *c-port*)
		#t)
	     (let loop ((actuals actuals))
		(if (null? (cdr actuals))
		    (begin
		       (emit-cop (car actuals))
		       (write-char #\) *c-port*)
		       #t)
		    (begin
		       (emit-cop (car actuals))
		       (display ", " *c-port*)
		       (loop (cdr actuals))))))))
   
   (define (emit-prefix-capp-sans-bdb-loc)
      (let ((o *bdb-debug-no-line-directives?*))
	 (set! *bdb-debug-no-line-directives?* #t)
	 (emit-prefix-capp)
	 (set! *bdb-debug-no-line-directives?* o)))
   (let ((fun (varc-variable (capp-fun cop)))
	 (loc (capp-loc cop)))
      (emit-bdb-loc loc)
      (cond
	 ((and (cfun? (global-value fun)) (cfun-infix? (global-value fun)))
	  (emit-infix-capp))
	 ((and (cfun? (global-value fun)) (cfun-macro? (global-value fun)))
	  (emit-prefix-capp-sans-bdb-loc))
	 (else
	  (emit-prefix-capp)))))

;*---------------------------------------------------------------------*/
;*    *bfalse*                                                         */
;*    -------------------------------------------------------------    */
;*    A local cache for the C false macro.                             */
;*---------------------------------------------------------------------*/
(define *bfalse* #f)

;*---------------------------------------------------------------------*/
;*    emit-cop ::cfail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cfail)
   (with-access::cfail cop (proc msg obj loc)
      (emit-bdb-loc loc)
      (if (not *bfalse*)
	  (set! *bfalse* (get-global/module 'bfalse 'foreign)))
      (cond
	 ((and (varc? proc) (eq? (varc-variable proc) *bfalse*)
	       (varc? msg) (eq? (varc-variable msg) *bfalse*)
	       (varc? obj) (eq? (varc-variable obj) *bfalse*))
	  (display "exit( -1 );" *c-port*))
	 ((<=fx *bdb-debug* 0)
	  (display "FAILURE(" *c-port*)
	  (emit-cop proc)
	  (write-char #\, *c-port*)
	  (emit-cop msg)
	  (write-char #\, *c-port*)
	  (emit-cop obj)
	  (display ");" *c-port*))
	 (else
	  (display "the_failure(" *c-port*)
	  (emit-cop proc)
	  (write-char #\, *c-port*)
	  (emit-cop msg)
	  (write-char #\, *c-port*)
	  (emit-cop obj)
	  (display "), exit( -1 );" *c-port*)))
      #f))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cswitch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cswitch)
   (with-access::cswitch cop (test clauses loc)
      (emit-bdb-loc loc)
      (display "switch( " *c-port*)
      (emit-cop test)
      (display ") { " *c-port*)
      (trace cgen (display "/* cop-cswitch */" *c-port*))
      (let loop ((clauses clauses)
		 (seen '()))
	 (let ((clause (car clauses)))
	    (cond
	       ((eq? (car clause) 'else)
		(let ((loc (cop-loc (cdr clause))))
		   (emit-bdb-loc loc)
		   (display "default: " *c-port*)
		   (if (emit-cop (cdr clause))
		       (begin
			  (display "; " *c-port*)
			  (trace cgen
			     (display "/* cswitch default */" *c-port*))))
		   (display "} " *c-port*)
		   (trace cgen (display "/* cswitch */" *c-port*))
		   #f))
	       ((every (lambda (n) (memq n seen)) (car clause))
		(loop (cdr clauses) seen))
	       (else
		(for-each (lambda (t)
			     (unless (memq t seen)
				(display "case " *c-port*)
				(emit-atom-value t)
				(display " : " *c-port*)
				(newline *c-port*)))
		   (car clause))
		(if (emit-cop (cdr clause))
		    (begin
		       (display "; " *c-port*)
		       (trace cgen
			  (display "/* cswitch clause */" *c-port*))))
		(display "break;" *c-port*)
		(loop (cdr clauses) (append (car clause) seen))))))))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cmake-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cmake-box)
   (with-access::cmake-box cop (value loc stackable)
      (emit-bdb-loc loc)
      (if (local? stackable)
	  (begin
	     (display "MAKE_CELL_STACK(" *c-port*)
	     (emit-cop value)
	     (write-char #\, *c-port*)
	     (display (variable-name stackable) *c-port*)
	     (write-char #\) *c-port*))
	  (begin
	     (display "MAKE_CELL(" *c-port*)
	     (emit-cop value)
	     (write-char #\) *c-port*)))
      #t))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cbox-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cbox-ref)
   (with-access::cbox-ref cop (var loc)
      (emit-bdb-loc loc)
      (display "CELL_REF(" *c-port*)
      (emit-cop var)
      (write-char #\) *c-port*)
      #t))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cbox-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cbox-set!)
   (with-access::cbox-set! cop (var value loc)
      (emit-bdb-loc loc)
      (display "CELL_SET(" *c-port*)
      (emit-cop var)
      (display ", " *c-port*)
      (emit-cop value)
      (write-char #\) *c-port*)
      #t))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cset-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cset-ex-it)
   (with-access::cset-ex-it cop (exit jump-value body loc)
      (emit-bdb-loc loc)
      (display "if( SET_EXIT(" *c-port*)
      (emit-cop exit)
      (display ") ) { " *c-port*)
      (trace cgen (display "/* cop-cset-ex-it */" *c-port*))
      (when (emit-cop jump-value) (display ";" *c-port*))
      (emit-bdb-loc loc)
      (display "} else {\n" *c-port*)
      (display "#if( SIGSETJMP_SAVESIGS == 0 )\n" *c-port*)
      (display "  bgl_restore_signal_handlers();\n" *c-port*)
      (display "#endif\n" *c-port*)
      (emit-cop body)
      (emit-bdb-loc loc)
      (display "} " *c-port*)
      (trace cgen (display "/* cop-cset-ex-it */" *c-port*))
      #f))

;*---------------------------------------------------------------------*/
;*    emit-cop ::cjump-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (emit-cop cop::cjump-ex-it)
   (with-access::cjump-ex-it cop (exit value loc)
      (emit-bdb-loc loc)
      (display "JUMP_EXIT( " *c-port*)
      (emit-cop exit)
      (write-char #\, *c-port*)
      (emit-cop value)
      (write-char #\) *c-port*)
      #t))

;*---------------------------------------------------------------------*/
;*    *bdb-loc* ...                                                    */
;*    -------------------------------------------------------------    */
;*    The current bdb source location information.                     */
;*---------------------------------------------------------------------*/
(define *bdb-loc* #unspecified)

;*---------------------------------------------------------------------*/
;*    reset-bdb-loc! ...                                               */
;*---------------------------------------------------------------------*/
(define (reset-bdb-loc!)
   (set! *bdb-loc* #unspecified))

;*---------------------------------------------------------------------*/
;*    get-current-bdb-loc ...                                          */
;*---------------------------------------------------------------------*/
(define (get-current-bdb-loc)
   *bdb-loc*)

;*---------------------------------------------------------------------*/
;*    emit-bdb-loc ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function emits a bdb location information (that is a        */
;*    C # line information.) This function emits this information      */
;*    only if we are dumping the C code for a different Scheme source  */
;*    code line that the previous line dump.                           */
;*---------------------------------------------------------------------*/
(define (emit-bdb-loc cur-loc)
   (cond
      ((or (not *c-debug-lines-info*) *bdb-debug-no-line-directives?*)
       (newline *c-port*))
      ((not (location? cur-loc))
       (cond
	  ((location? *bdb-loc*)
	   (emit-bdb-loc *bdb-loc*))
	  ((location? *module-location*)
	   ;; when no location is found, we use the location of
	   ;; the module clause
	   (emit-bdb-loc *module-location*))
	  ((pair? *src-files*)
	   ;; when no location at all is found, emit a dummy
	   ;; "first file line" location
	   (fprint *c-port* #"\n#line " 1 " \"" (car *src-files*) #\"))))
      (else
       (let ((cur-fname (location-fname cur-loc))
	     (cur-line  (location-lnum cur-loc)))
	  (when (and (integer? cur-line) (string? cur-fname))
	     (fprint *c-port* #"\n#line " cur-line " \"" cur-fname #\"))
	  (set! *bdb-loc* cur-loc)))))

;*---------------------------------------------------------------------*/
;*    emit-bdb-loc-comment ...                                         */
;*    -------------------------------------------------------------    */
;*    For debug purposes this function write a location is a C         */
;*    comment (iff CUR-LOC is a location).                             */
;*---------------------------------------------------------------------*/
(define (emit-bdb-loc-comment cur-loc)
   (if (location? cur-loc)
       (begin
	  (display "/* " *c-port*)
	  (display (location-fname cur-loc) *c-port*)
	  (display " " *c-port*)
	  (display (location-lnum cur-loc) *c-port*)
	  (display " */" *c-port*))))
