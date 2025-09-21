;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Eval/macro.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 08:59:04 1994                          */
;*    Last change :  Sun Sep 21 01:07:34 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    La manipulation des macros (de l'interprete et du compilateur).  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __macro
   
   (export  *module5-env*
	    (install-module5-expander ::obj ::symbol ::pair ::procedure)
	    (install-module4-expander ::symbol ::procedure)
	    (install-eval-expander ::symbol ::procedure)
	    (install-compiler-expander ::symbol ::procedure)
	    (install-expander ::symbol ::pair ::procedure)
	    (get-module5-expander ::symbol)
	    (get-eval-expander ::symbol)
	    (get-compiler-expander ::symbol)
	    (macro->expander::pair ::pair))

   (import  __error
	    __hash
	    __everror
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __param
	    __bit

	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __evenv
	    __evmodule
	    __progn))

;*---------------------------------------------------------------------*/
;*    *macro-mutex* ...                                                */
;*---------------------------------------------------------------------*/
(define *eval-macro-mutex* (make-spinlock "eval-macros"))
(define *compiler-macro-mutex* (make-spinlock "compiler-macros"))

;*---------------------------------------------------------------------*/
;*    macro                                                            */
;*---------------------------------------------------------------------*/
(define-struct macros key eval-expander compiler-expander)

;*---------------------------------------------------------------------*/
;*    Global hash tables                                               */
;*---------------------------------------------------------------------*/
(define *module5-env* #f)
(define *eval-macro-table* (make-hashtable))
(define *compiler-macro-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    module-macro-table ...                                           */
;*---------------------------------------------------------------------*/
(define (module-macro-table)
   (let ((m (eval-module)))
      (when (evmodule? m)
	 (evmodule-macro-table m))))

;*---------------------------------------------------------------------*/
;*    put-macro! ...                                                   */
;*---------------------------------------------------------------------*/
(define (put-macro! table key expander where)
   (hashtable-update! table key
      (lambda (x)
	 (evwarning #f "install-expander"
	    (format "Redefinition of ~a" " expander -- " where)
	    key)
	 expander)
      expander))

;*---------------------------------------------------------------------*/
;*    install-eval-expander ...                                        */
;*    -------------------------------------------------------------    */
;*    On installe une macro pour l'interprete seulement.               */
;*---------------------------------------------------------------------*/
(define (install-eval-expander id expander)
   (cond
      ((not (symbol? id))
       (error "install-eval-expander" "Illegal expander identifier" id))
      ((not (procedure? expander))
       (error "install-eval-expander" "Illegal expander expander" expander))
      (else
       (synchronize *eval-macro-mutex*
	  (put-macro! (or (module-macro-table) *eval-macro-table*)
	     id expander "eval")))))

;*---------------------------------------------------------------------*/
;*    install-compiler-expander ...                                    */
;*    -------------------------------------------------------------    */
;*    On installe une macro pour le compilateur seulement.             */
;*---------------------------------------------------------------------*/
(define (install-compiler-expander id expander)
   (cond
      ((not (symbol? id))
       (error "install-eval-expander" "Illegal expander identifier" id))
      ((not (procedure? expander))
       (error "install-eval-expander" "Illegal expander expander" expander))
      (else
       (synchronize *compiler-macro-mutex*
	  (put-macro! *compiler-macro-table* id expander "compiler")))))

;*---------------------------------------------------------------------*/
;*    install-module5-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (install-module5-expander env id x expander)
   ;; the module5 compilation (MODULE5-EXPAND-AND-RESOLVE), will need
   ;; the actual source expressions that defined the module macros
   ;; (see runtime/module5.scm), so the module5
   ;; environment stores the definition of the expander itself and its sources
   (hashtable-put! env (symbol->string! id) (cons x expander))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    install-module4-expander ...                                     */
;*    -------------------------------------------------------------    */
;*    On installe une macro pour le compilateur *et* l'interprete.     */
;*---------------------------------------------------------------------*/
(define (install-module4-expander id expander)
   (install-eval-expander id expander)
   (install-compiler-expander id expander)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    install-expander ...                                             */
;*    -------------------------------------------------------------    */
;*    On installe une macro pour le compilateur *et* l'interprete.     */
;*---------------------------------------------------------------------*/
(define (install-expander id x expander)
   (if *module5-env*
       (install-module5-expander *module5-env* id x expander)
       (install-module4-expander id expander)))

;*---------------------------------------------------------------------*/
;*    get-module5-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (get-module5-expander id)
   (when *module5-env*
      (let ((e (hashtable-get *module5-env* (symbol->string! id))))
	 ;; see INSTALL-MODULE5-EXPANDER for the structure of E
	 (when (pair? e)
	    (cdr e)))))

;*---------------------------------------------------------------------*/
;*    get-eval-expander ...                                            */
;*    -------------------------------------------------------------    */
;*    On recupere une macro pour l'interprete.                         */
;*---------------------------------------------------------------------*/
(define (get-eval-expander id)
   (synchronize *eval-macro-mutex*
      (let ((mtable (module-macro-table)))
	 (or (and mtable (hashtable-get mtable id))
	     (hashtable-get *eval-macro-table* id)))))

;*---------------------------------------------------------------------*/
;*    get-compiler-expander ...                                        */
;*    -------------------------------------------------------------    */
;*    On recupere une macro pour le compilateur.                       */
;*    -------------------------------------------------------------    */
;*    Macro compilers are always global so we don't look into          */
;*    the eval module macro.                                           */
;*---------------------------------------------------------------------*/
(define (get-compiler-expander id)
   (synchronize *compiler-macro-mutex*
      (hashtable-get *compiler-macro-table* id)))

;*---------------------------------------------------------------------*/
;*    macro->expander ...                                              */
;*---------------------------------------------------------------------*/
(define (macro->expander x)
   (match-case x
      ((?- (?name . ?args) . ?body)
       (let* ((fname (gensym))
	      (loc (gensym))
	      (nx `(lambda (x1 e)
		      (let ((,fname #f) ,loc)
			 (when (epair? x1)
			    (match-case (cer x1)
			       ((at ?f ?l)
				(set! ,fname f)
				(set! ,loc l))))
			 (let* ((n (let* ,(destructure
					     name fname loc
					     args '(cdr x1) '())
				      ,(expand-progn body)))
				(ne (e n e)))
			    (evepairify* ne x1))))))
	  (evepairify nx x)))
      (else
       (error "macro->expander" "Illegal form syntax" x))))

;*---------------------------------------------------------------------*/
;*    destructure ...                                                  */
;*---------------------------------------------------------------------*/
(define (destructure id fname loc pat arg bindings)
   
   (define (err msg obj)
      `(if (string? ,fname)
	  (error/location ',id ,msg ',obj ,fname ,loc)
	  (error ',id ,msg ',obj)))
   
   (let loop ((pat pat)
	      (arg arg)
	      (bindings bindings))
      (cond
	 ((null? pat)
	  (cons `(,(gensym)
		  (if (not (null? ,arg))
		      ,(err "Too many arguments provided" arg)
		      '()))
		bindings))
	 ((symbol? pat)
	  (cons `(,pat ,arg) bindings))
	 ((pair? pat)
	  (loop (car pat)
		`(if (pair? ,arg)
		     (car ,arg)
		     ,(err "Missing value for argument" (car pat)))
		(loop (cdr pat) `(cdr ,arg) bindings)))
	 (else
	  (error/location id "Illegal macro parameter" pat fname loc)))))
