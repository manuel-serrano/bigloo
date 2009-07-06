;*=====================================================================*/
;*    serrano/prgm/project/bdk/kbdb/src/Patient/mangling.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 21:08:59 1999                          */
;*    Last change :  Fri Feb 16 15:31:55 2001 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The name Bdb mangling/demangling.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module patient_mangling
   (import tools_tools
	   engine_param
	   patient_invoke
	   bee_invoke
	   command_expr
	   gdb_annotate
	   tools_speek)
   (export (bdb-demangle ::bstring)
	   (bdb-demangle2 ::bstring ::bstring)
	   (bdb-mangle ::bstring)
	   (bdb-mangle2 ::bstring ::bstring)
	   (bdb-mangle-for-funcall ::bstring)
	   (reset-demangling-cache!)
	   (bigloo-function? ::bstring)))

;*---------------------------------------------------------------------*/
;*    global-info                                                      */
;*---------------------------------------------------------------------*/
(define-struct global-info
   ;; the Bigloo identifier for the variable
   scm-name
   ;; the c identifier for the variable
   c-name
   ;; the list of locals contained in the global
   locals)

;*---------------------------------------------------------------------*/
;*    local-info                                                       */
;*---------------------------------------------------------------------*/
(define-struct local-info
   ;; the Bigloo identifier for the variable
   scm-name
   ;; the c identifier for the variable
   c-name)

;*---------------------------------------------------------------------*/
;*    bdb-demangle ...                                                 */
;*    -------------------------------------------------------------    */
;*    To demangle a name, we check if we have already demangle         */
;*    that name and thus if the name is in the local cache.            */
;*    Otherwise we have to request the patient for a demangling.       */
;*---------------------------------------------------------------------*/
(define (bdb-demangle c-ident::bstring)
   (if (memq *bdb-mode* '(scheme mixte))
       (let ((ld (local-demangle c-ident)))
	  (if (global-info? ld)
	      (global-info-scm-name ld)
	      (let ((rd (if (bigloo-mangled? c-ident)
			    (bigloo-demangle c-ident)
			    (remote-demangle c-ident))))
		 (if (string? rd)
		     (let ((glo (global-info rd c-ident '())))
			(put-local-demangle! glo)
			rd)
		     ;; if we have found no name demangling for that
		     ;; function and if the execution is started it
		     ;; means that it is a real C function.
		     (begin
			(if (gdb-annotate-running?)
			    (mark-unmangled! c-ident))
			#f)))))
       #f))

;*---------------------------------------------------------------------*/
;*    bdb-demangle2 ...                                                */
;*    -------------------------------------------------------------    */
;*    To demangle a name, we check if we have already demangle         */
;*    that name and thus if the name is in the local cache.            */
;*    Otherwise we have to request the patient for a demangling.       */
;*---------------------------------------------------------------------*/
(define (bdb-demangle2 c-ident::bstring loc::bstring)
   (define (demangle2/glo glo)
      (let ((ld (local-demangle-local glo loc)))
	 (if (local-info? ld)
	     (local-info-scm-name ld)
	     (let ((rd (remote-demangle2 c-ident loc)))
		(if (string? rd)
		    (let ((loc (local-info rd loc)))
		       (put-local-demangle-local! glo loc)
		       rd)
		    #f)))))
   (if (memq *bdb-mode* '(scheme mixte))
       (let ((glo (local-demangle c-ident)))
	  (if (global-info? glo)
	      (demangle2/glo glo)
	      (let ((glo (remote-demangle c-ident)))
		 (if (global-info? glo)
		     (demangle2/glo glo)
		     #f))))
       #f))

;*---------------------------------------------------------------------*/
;*    bdb-mangle ...                                                   */
;*    -------------------------------------------------------------    */
;*    See DEMANGLE                                                     */
;*---------------------------------------------------------------------*/
(define (bdb-mangle bgl-ident::bstring)
   (if (memq *bdb-mode* '(scheme mixte))
       (let ((ld (local-mangle bgl-ident)))
	  (if (global-info? ld)
	      (global-info-c-name ld)
	      (let ((rd (remote-mangle bgl-ident)))
		 (if (string? rd)
		     (let ((glo (global-info bgl-ident rd '())))
			(put-local-demangle! glo)
			rd)
		     #f))))
       #f))

;*---------------------------------------------------------------------*/
;*    bdb-mangle2 ...                                                  */
;*    -------------------------------------------------------------    */
;*    We don't maintain a cache here. This function is only called     */
;*    when evaluating expression.                                      */
;*---------------------------------------------------------------------*/
(define (bdb-mangle2 c-ident::bstring bgl-ident::bstring)
   (if (memq *bdb-mode* '(scheme mixte))
       (remote-mangle2 c-ident bgl-ident)
       #f))

;*---------------------------------------------------------------------*/
;*    bdb-mangle-for-funcall ...                                       */
;*    -------------------------------------------------------------    */
;*    We don't maintain a cache here. This function is only called     */
;*    when evaluating expression.                                      */
;*---------------------------------------------------------------------*/
(define (bdb-mangle-for-funcall bgl-ident::bstring)
   (if (memq *bdb-mode* '(scheme mixte))
       (remote-mangle-for-funcall bgl-ident)
       #f))

;*---------------------------------------------------------------------*/
;*    *mangling-cache* ...                                             */
;*---------------------------------------------------------------------*/
(define *demangling-cache* #unspecified)
(define *mangling-cache* #unspecified)
(define *unmangled-cache* #unspecified)

;*---------------------------------------------------------------------*/
;*    reset-demangling-cache! ...                                      */
;*---------------------------------------------------------------------*/
(define (reset-demangling-cache!)
   (set! *demangling-cache* (make-hashtable))
   (set! *mangling-cache* (make-hashtable))
   (set! *unmangled-cache* (make-hashtable))
   ;; we mark some standard C functions as unmangled.
   ;; this configuration is mandatory because if the user
   ;; sets a breakpoint in C main, there is no mean to talk to
   ;; the patient yet. Thus, we have to know that main is a C function.
   (for-each mark-unmangled! *c-std-unmangled*))

;*---------------------------------------------------------------------*/
;*    local-demangle ...                                               */
;*---------------------------------------------------------------------*/
(define (local-demangle c::bstring)
   (and (hashtable? *demangling-cache*)
	(hashtable-get *demangling-cache* c)))

;*---------------------------------------------------------------------*/
;*    local-mangle ...                                                 */
;*---------------------------------------------------------------------*/
(define (local-mangle bgl::bstring)
   (and (hashtable? *mangling-cache*)
	(hashtable-get *mangling-cache* bgl)))

;*---------------------------------------------------------------------*/
;*    put-local-demangle! ...                                          */
;*---------------------------------------------------------------------*/
(define (put-local-demangle! glo::struct)
   (if (or (not (hashtable? *demangling-cache*))
	   (not (hashtable? *mangling-cache*)))
       (reset-demangling-cache!))
   (hashtable-put! *demangling-cache* (global-info-c-name glo) glo)
   (hashtable-put! *mangling-cache* (global-info-scm-name glo) glo))

;*---------------------------------------------------------------------*/
;*    remote-demangle ...                                              */
;*    -------------------------------------------------------------    */
;*    The mangling for that identifier is not in the local cache       */
;*    yet, we have to request the patient.                             */
;*---------------------------------------------------------------------*/
(define (remote-demangle c::bstring)
   ;; in bee client mode, we do request the patient, the ask the bee
   (if (not (mark-unmangled? c))
       (patient-call "bdb_demangle" (string-append "\"" c "\""))
       #f))
   
;*---------------------------------------------------------------------*/
;*    remote-demangle2 ...                                             */
;*    -------------------------------------------------------------    */
;*    The mangling for that identifier is not in the local cache       */
;*    yet, we have to request the patient.                             */
;*---------------------------------------------------------------------*/
(define (remote-demangle2 c loc)
   ;; in bee client mode, we do request the patient, the ask the bee
   (patient-call "bdb_demangle2"
		 (string-append "\"" c "\"")
		 (string-append "\"" loc "\"")))
   
;*---------------------------------------------------------------------*/
;*    remote-mangle ...                                                */
;*    -------------------------------------------------------------    */
;*    The mangling for that identifier is not in the local cache       */
;*    yet, we have to request the patient.                             */
;*---------------------------------------------------------------------*/
(define (remote-mangle bgl::bstring)
   (patient-call "bdb_mangle" (string-append "\"" bgl "\"")))
   
;*---------------------------------------------------------------------*/
;*    remote-mangle2 ...                                               */
;*    -------------------------------------------------------------    */
;*    The mangling for that identifier is not in the local cache       */
;*    yet, we have to request the patient.                             */
;*---------------------------------------------------------------------*/
(define (remote-mangle2 c::bstring bgl::bstring)
   (patient-call "bdb_mangle2"
		 (string-append "\"" c "\"")
		 (string-append "\"" bgl "\"")))
   
;*---------------------------------------------------------------------*/
;*    remote-mangle-for-funcall ...                                    */
;*    -------------------------------------------------------------    */
;*    The mangling for that identifier is not in the local cache       */
;*    yet, we have to request the patient.                             */
;*---------------------------------------------------------------------*/
(define (remote-mangle-for-funcall bgl::bstring)
   (patient-call "bdb_mangle_for_funcall" (string-append "\"" bgl "\"")))
   
;*---------------------------------------------------------------------*/
;*    bigloo-function? ...                                             */
;*    -------------------------------------------------------------    */
;*    A function is a Bigloo function iff we are able to demangle it.  */
;*---------------------------------------------------------------------*/
(define (bigloo-function? name::bstring)
   (string? (bdb-demangle name)))

;*---------------------------------------------------------------------*/
;*    local-demangle-local ...                                         */
;*---------------------------------------------------------------------*/
(define (local-demangle-local glo::struct c-ident::bstring)
   (let loop ((locals (global-info-locals glo)))
      (cond
	 ((null? locals)
	  #f)
	 ((string=? (local-info-c-name (car locals)) c-ident)
	  (car locals))
	 (else
	  (loop (cdr locals))))))

;*---------------------------------------------------------------------*/
;*    put-local-demangle-local! ...                                    */
;*---------------------------------------------------------------------*/
(define (put-local-demangle-local! glo::struct loc::struct)
   (global-info-locals-set! glo (cons loc (global-info-locals glo))))

;*---------------------------------------------------------------------*/
;*    local-mangle-local ...                                           */
;*---------------------------------------------------------------------*/
(define (local-mangle-local glo::struct bgl-ident::bstring)
   (let loop ((locals (global-info-locals glo)))
      (cond
	 ((null? locals)
	  #f)
	 ((string=? (local-info-scm-name (car locals)) bgl-ident)
	  (car locals))
	 (else
	  (loop (cdr locals))))))

;*---------------------------------------------------------------------*/
;*    put-local-mangle-local! ...                                      */
;*---------------------------------------------------------------------*/
(define (put-local-mangle-local! glo::struct loc::struct)
   (global-info-locals-set! glo (cons loc (global-info-locals glo))))

;*---------------------------------------------------------------------*/
;*    mark-unmangled! ...                                              */
;*---------------------------------------------------------------------*/
(define (mark-unmangled! c::bstring)
   (if (not (hashtable? *unmangled-cache*))
       (reset-demangling-cache!))
   (hashtable-put! *unmangled-cache* c #t))

;*---------------------------------------------------------------------*/
;*    mark-unmangled? ...                                              */
;*---------------------------------------------------------------------*/
(define (mark-unmangled? c::bstring)
   (and (hashtable? *unmangled-cache*)
	(hashtable-get *unmangled-cache* c)))
