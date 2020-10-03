;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/bigloo.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 08:24:40 1995                          */
;*    Last change :  Sat Jun 15 08:43:30 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The bigloo runtime utility functions                             */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bigloo

   (import  __error
	    __configure
	    __param
	    __object
	    __thread
	    __bexit)
   
   (use     __type
	    __tvector
	    __bit
	    __bignum

	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r5_control_features_6_4
	    __r4_output_6_10_3
	    __r4_ports_6_10_1
	    
	    __evenv)

   (extern  (macro c-procedure-light?::bool (::obj)
		   "PROCEDURE_LIGHTP")
	    (macro va-procedure?::bool (::procedure)
		   "VA_PROCEDUREP")
	    
	    (macro $procedure-entry->string::bstring (::procedure)
		   "bgl_procedure_entry_to_string")
	    (macro $string->procedure-entry::obj (::bstring)
		   "bgl_string_to_procedure_entry")
	    
	    (macro $procedure-arity::int (::procedure)
		   "PROCEDURE_ARITY")
	    (macro correct-arity?::bool (::procedure ::int)
		   "PROCEDURE_CORRECT_ARITYP")

	    (macro $procedure-length::int (::procedure)
		   "PROCEDURE_LENGTH")

	    (macro $make-procedure::procedure (::obj ::int ::int)
		   "bgl_make_procedure")
	    (macro $dup-procedure::procedure (::procedure)
		   "bgl_dup_procedure")
	    
	    (macro make-fx-procedure::procedure (::obj ::int ::int)
		   "MAKE_FX_PROCEDURE")		 
	    (macro make-va-procedure::procedure (::obj ::int ::int)
		   "MAKE_VA_PROCEDURE")
	    
	    (macro make-stack-fx-procedure::procedure (::obj ::int ::int)
		   "BGL_MAKE_STACK_FX_PROCEDURE")		 
	    
	    (macro make-el-procedure::procedure-el (::int)
		   "MAKE_EL_PROCEDURE")	 
	    
	    (macro make-l-procedure::procedure (::obj ::int)
		   "MAKE_L_PROCEDURE")		 
	    					 
	    (macro procedure-set!::obj (::procedure ::int ::obj)
		   "PROCEDURE_SET")			 
	    (macro procedure-ref::obj (::procedure ::int)
		   "PROCEDURE_REF")			 

	    (macro $procedure-attr-set!::obj (::procedure ::obj)
		   "PROCEDURE_ATTR_SET")			 
	    (macro $procedure-attr::obj (::procedure)
		   "PROCEDURE_ATTR")			 

	    (macro procedure-l-set!::obj (::obj ::int ::obj)
		   "PROCEDURE_L_SET")		  
	    (macro procedure-l-ref::obj (::obj ::int)
		   "PROCEDURE_L_REF")
	    
	    (macro procedure-el-set!::obj (::procedure-el ::int ::obj)
		   "PROCEDURE_EL_SET")		 
	    (macro procedure-el-ref::obj (::procedure-el ::int)
		   "PROCEDURE_EL_REF")
	    
	    (macro $make-cell::cell (::obj)
		   "MAKE_YOUNG_CELL")
	    (macro $cell-set!::obj (::cell ::obj)
		   "CELL_SET")
	    (macro $cell-ref::obj (::cell)
		   "CELL_REF")
	    (macro $cell?::bool (::obj)
		   "CELLP")
	    
	    (macro c-cnst?::bool (::obj)
		   "CNSTP")
	    
	    (macro c-opaque?::bool (::obj)
		   "OPAQUEP")
	    (macro c-opaque-nil::obj ()
		   "BGL_OPAQUE_NIL")
	    
	    (macro __unspec__::obj "BUNSPEC")
	    (macro __eoa__::obj "BEOA")
	    
	    (macro declare-cnst-table::obj (::obj)
		   "DECLARE_CNST_TABLE")
	    (macro cnst-table-set!::obj (::int ::obj)
		   "CNST_TABLE_SET")
	    (macro cnst-table-ref::obj (::int)
		   "CNST_TABLE_REF")

	    (macro close-init-string::obj (::obj)
		   "close_init_string")
	    
	    (macro var->root::obj (::obj)
		   "(obj_t)&")
	    (macro GC-add-globv!::obj (::obj)
		   "GC_ADD_GLOBV")
	    (macro GC-add-roots!::obj (::obj ::obj)
		   "GC_ADD_ROOTS")

	    (macro %exit::obj (::obj)
		   "BIGLOO_EXIT")

	    (macro $time::obj (::procedure)
		   "bgl_time")

	    (macro $null-or-unspecified?::bool (::obj)
		   "BGL_NULL_OR_UNSPECIFIEDP")

	    ($bgl-bmem-reset::obj () "bgl_bmem_reset")
	    ($bgl-gc-verbose-set!::void (::bool) "bgl_gc_verbose_set")

	    (export bigloo-mangle "bigloo_mangle")
	    (export bigloo-module-mangle "bigloo_module_mangle")
	    (export bigloo-mangled? "bigloo_mangledp")
	    (export bigloo-class-mangled? "bigloo_class_mangledp")
	    (export bigloo-demangle "bigloo_demangle")
	    (export bigloo-class-demangle "bigloo_class_demangle")
	    (export bigloo-exit-apply "bigloo_exit_apply")
	    (export bigloo-exit-mutex "bgl_exit_mutex"))

   (java    (class foreign
	       (field static __unspec__::obj "BUNSPEC")
	       (field static __eoa__::obj "BEOA")
	       
	       (method static c-procedure-light?::bool (::obj)
		       "PROCEDURE_LIGHTP")
	       (method static va-procedure?::bool (::procedure)
		       "VA_PROCEDUREP")
	       
	       (method static $procedure-entry->string::bstring (::procedure)
		       "bgl_procedure_entry_to_string")
	       (method static $string->procedure-entry::obj (::bstring)
		       "bgl_string_to_procedure_entry")
	    
	       (method static $procedure-arity::int (::procedure)
		       "PROCEDURE_ARITY")
	       (method static correct-arity?::bool (::procedure ::int)
		       "PROCEDURE_CORRECT_ARITYP")

	       (method static $procedure-length::int (::procedure)
		       "PROCEDURE_LENGTH")

	       (method static $make-procedure::procedure (::obj ::int ::int)
		       "bgl_make_procedure")		 
	       (method static make-fx-procedure::procedure (::obj ::int ::int)
		       "make_fx_procedure")		 
	       (method static make-va-procedure::procedure (::obj ::int ::int)
		       "make_va_procedure")
	       
	       (method static make-el-procedure::procedure-el (::int)
		       "MAKE_EL_PROCEDURE")	 
	       
	       (method static make-l-procedure::procedure (::obj ::int)
		       "MAKE_L_PROCEDURE")		 
	       
	       (method static procedure-set!::obj (::procedure ::int ::obj)
		       "PROCEDURE_SET")			 
	       (method static procedure-ref::obj (::procedure ::int)
		       "PROCEDURE_REF")			 
	       
	       (method static $procedure-attr-set!::obj (::procedure ::obj)
		       "PROCEDURE_ATTR_SET")			 
	       (method static $procedure-attr::obj (::procedure)
		       "PROCEDURE_ATTR")
	       
	       (method static procedure-l-set!::obj (::procedure ::int ::obj)
		       "PROCEDURE_L_SET")		  
	       (method static procedure-l-ref::obj (::procedure ::int)
		       "PROCEDURE_L_REF")
	       
	       (method static procedure-el-set!::obj (::procedure-el ::int ::obj)
		       "PROCEDURE_EL_SET")		 
	       (method static procedure-el-ref::obj (::procedure-el ::int)
		       "PROCEDURE_EL_REF")
	       
	       (method static $make-cell::cell (::obj)
		       "MAKE_CELL")
	       (method static $cell-set!::obj (::cell ::obj)
		       "CELL_SET")
	       (method static $cell-ref::obj (::cell)
		       "CELL_REF")
	       (method static $cell?::bool (::obj)
		       "CELLP")
	       
	       (method static c-cnst?::bool (::obj)
		       "CNSTP")
	       (method static c-opaque?::bool (::obj)
		       "OPAQUEP")
	       (method static c-opaque-nil::obj ()
		       "BGL_OPAQUE_NIL")
	       
	       (method static declare-cnst-table::obj (::obj)
		       "DECLARE_CNST_TABLE")
	       (method static cnst-table-set!::obj (::int ::obj)
		       "CNST_TABLE_SET")
	       (method static cnst-table-ref::obj (::int)
		       "CNST_TABLE_REF")
	       
	       (method static close-init-string::obj (::obj)
		       "close_init_string")
	       
	       (method static var->root::obj (::obj)
		       "VAR_ROOT")
	       (method static GC-add-globv!::obj (::obj)
		       "GC_ADD_GLOBV")
	       (method static GC-add-roots!::obj (::obj ::obj)
		       "GC_ADD_ROOTS")
	       
	       (method static %exit::obj (::obj)
		       "BIGLOO_EXIT")

	       (method static $time::obj (::procedure)
		       "bgl_time"))
	    
	    (export bigloo-mangle "bigloo_mangle")
	    (export bigloo-module-mangle "bigloo_module_mangle")
	    (export bigloo-mangled? "bigloo_mangledp")
	    (export bigloo-demangle "bigloo_demangle")
	    (export bigloo-class-mangled? "bigloo_class_mangledp")
	    (export bigloo-class-demangle "bigloo_class_demangle")
	    (export bigloo-exit-apply "bigloo_exit_apply"))

   (export  (check-version! ::obj ::string ::obj)
	    (inline cnst?::bool ::obj)
	    (inline opaque?::bool ::obj)
	    (inline opaque-nil::obj)
	    (inline procedure-arity::int ::procedure)
	    (inline procedure-length::int ::procedure)
	    (inline procedure-attr::obj ::procedure)
	    (inline procedure-attr-set!::obj ::procedure ::obj)
	    (inline unspecified::unspecified)
	    (inline null-or-unspecified?::bool ::obj)
	    (bigloo-mangled?::bool ::bstring)
	    (bigloo-need-mangling?::bool ::bstring)
	    (bigloo-class-mangled?::bool ::bstring)
	    (bigloo-mangle::bstring ::bstring)
	    (bigloo-module-mangle::bstring ::bstring ::bstring)
	    (bigloo-demangle ::bstring)
	    (bigloo-class-demangle::bstring ::bstring)
	    (register-exit-function! ::procedure)
	    (unregister-exit-function! ::procedure)
	    (bigloo-exit-apply::obj ::obj)
 	    (bigloo-exit-mutex::obj)

	    (bmem-reset!)
	    
	    (time::obj ::procedure)
	    (bigloo-gc-verbose-set! ::bool)

	    (inline make-cell::cell ::obj)
	    (inline cell? ::obj)
	    (inline cell-ref ::cell)
	    (inline cell-set! ::cell ::obj))

   (pragma  (cnst-table-ref no-alloc fail-safe)
	    (c-procedure-light? nesting fail-safe)
	    (va-procedure? nesting fail-safe)
	    (procedure-arity nesting args-safe fail-safe)
	    ($procedure-arity nesting args-safe fail-safe)
	    (procedure-length nesting args-safe fail-safe)
	    ($procedure-length nesting args-safe fail-safe)
	    (correct-arity? nesting args-safe fail-safe)
	    (make-fx-procedure no-cfa-top nesting args-safe fail-safe (stack-allocator "char ~a[ BGL_PROCEDURE_BYTE_SIZE( $3 ) ]" "BGL_MAKE_FX_PROCEDURE_STACK"))
	    (make-va-procedure no-cfa-top nesting args-safe fail-safe)
	    (make-stack-fx-procedure no-cfa-top nesting args-safe fail-safe)
	    (procedure-set! no-cfa-top nesting args-safe fail-safe)
	    (procedure-ref no-cfa-top side-effect-free nesting args-safe fail-safe)
	    (procedure-l-set! nesting args-safe fail-safe)
	    (procedure-l-ref nesting args-safe fail-safe)
	    (procedure-el-set! nesting args-safe fail-safe)
	    (procedure-el-ref nesting args-safe fail-safe)
	    ($cell? (predicate-of cell) nesting fail-safe)
	    ($cell-set! nesting args-safe fail-safe)
	    ($cell-ref nesting  args-safe fail-safe)
	    (cell? (predicate-of cell) nesting fail-safe)
	    (cell-set! nesting args-safe fail-safe)
	    (cell-ref nesting  args-safe fail-safe)
	    (c-cnst? (predicate-of cnst) nesting fail-safe)
	    (c-opaque? (predicate-of opaque) nesting fail-safe))

   (option  (set! *unsafe-version* #t)))

;*---------------------------------------------------------------------*/
;*    check-version! ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is in charge of the coherence of all module        */
;*    of an executable (i.e. does all module compiled by the           */
;*    same Bigloo's version).                                          */
;*---------------------------------------------------------------------*/
(define (check-version! module release level)
   (cond
      ((not (string? *release*))
       (set! *modules* (list module))
       (set! *release* release)
       (set! *level*   level))
      ((or (let ((min (-fx (minfx (string-length release)
				  (string-length *release*))
			   1)))
	      (not (string=? (substring release 0 min)
			     (substring *release* 0 min))))
	   (and (char? level) (char? *level*) (not (char=? *level* level))))
       (let ((release-name (lambda (release level)
			      (if (char? level)
				  (let ((s (string-copy " (level 0)")))
				     (string-set! s 8 level)
				     (string-append release s))
				  release))))
	  (error (string-append "Some modules have been compiled by: "
				(release-name *release* *level*))
		 (string-append "and other by: "
				(release-name release level))
		 (cons module *modules*))))
      (else
       (set! *modules* (cons module *modules*)))))

;*---------------------------------------------------------------------*/
;*    Some variables for check-version!                                */
;*---------------------------------------------------------------------*/
(define *release* #f)
(define *level*   #f)
(define *modules* '())
   
;*---------------------------------------------------------------------*/
;*    procedure-arity ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (procedure-arity proc)
   ($procedure-arity proc))

;*---------------------------------------------------------------------*/
;*    procedure-length ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (procedure-length proc)
   ($procedure-length proc))

;*---------------------------------------------------------------------*/
;*    procedure-attr ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (procedure-attr proc)
   ($procedure-attr proc))

;*---------------------------------------------------------------------*/
;*    procedure-attr-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (procedure-attr-set! proc obj)
   ($procedure-attr-set! proc obj)
   obj)

;*---------------------------------------------------------------------*/
;*    unspecified ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (unspecified)
   __unspec__)

;*---------------------------------------------------------------------*/
;*    null-or-unspecified? ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (null-or-unspecified? obj)
   (cond-expand
      (bigloo-c
       ;; this stupid trick is only used to improve JavaScript performance
       ;; don't use that in real code!
       ($null-or-unspecified? obj))
      (else
       (or (null? obj) (eq? obj #unspecified)))))

;*---------------------------------------------------------------------*/
;*    cnst? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cnst? obj)
   (c-cnst? obj))

;*---------------------------------------------------------------------*/
;*    opaque? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (opaque? obj)
   (c-opaque? obj))

;*---------------------------------------------------------------------*/
;*    opaque-nil ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (opaque-nil)
   (c-opaque-nil))

;*---------------------------------------------------------------------*/
;*    4bits->char ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (4bits->char num)
   (let ((hexa "0123456789abcdef"))
      (string-ref hexa num)))

;*---------------------------------------------------------------------*/
;*    mangle-at! ...                                                   */
;*---------------------------------------------------------------------*/
(define (mangle-at! new old len offset)
   (let loop ((r 0)
	      (w offset)
	      (new-len offset)
	      (checksum 0))
      (if (=fx r len)
	  (begin
	     (string-set! new w #\z)
	     (string-set! new
			  (+fx w 1)
			  (4bits->char (bit-and checksum 15)))
	     (string-set! new
			  (+fx w 2)
			  (4bits->char (bit-and (bit-rsh checksum 4) 15)))
	     (+fx w 3))
	  (let ((c (string-ref old r)))
	     (if (or (and (char-alphabetic? c) (not (char=? c #\z)))
		     (char-numeric? c)
		     (char=? c #\_))
		 (begin 
		    (string-set! new w c)
		    (loop (+fx r 1)
			  (+fx w 1)
			  (+fx new-len 1)
			  checksum))
		 (let ((ic (char->integer c)))
		    (string-set! new w #\z)
		    (string-set! new
				 (+fx w 1)
				 (4bits->char (bit-and ic 15)))
		    (string-set! new
				 (+fx w 2)
				 (4bits->char (bit-and (bit-rsh ic 4) 15)))
		    (loop (+fx r 1)
			  (+fx w 3)
			  (+fx new-len 3)
			  (bit-xor checksum (char->integer c)))))))))

;*---------------------------------------------------------------------*/
;*    bigloo-mangle ...                                                */
;*---------------------------------------------------------------------*/
(define (bigloo-mangle string)
   (let* ((len (string-length string))
	  (new (make-string (+fx (*fx len 3) 7))))
      (if (=fx len 0)
	  (error "bigloo-mangle-string" "Can't mangle empty string" string)
	  (let ((stop (mangle-at! new string len 4)))
	     (blit-string! "BgL_" 0 new 0 4)
	     (substring new 0 stop)))))

;*---------------------------------------------------------------------*/
;*    bigloo-module-mangle ...                                         */
;*---------------------------------------------------------------------*/
(define (bigloo-module-mangle id module)
   (let* ((len (+fx (string-length id) (string-length module)))
	  (new (make-string (+fx (*fx len 3) 12))))
      (if (=fx len 0)
	  (error "bigloo-mangle-string" "Can't mangle empty string" string)
	  (let ((mod-start (mangle-at! new id (string-length id) 4)))
	     (string-set! new mod-start #\z)
	     (string-set! new (+fx 1 mod-start) #\z)
	     (let ((stop (mangle-at! new module
				     (string-length module)
				     (+fx mod-start 2))))
		(blit-string! "BGl_" 0 new 0 4)
		(substring new 0 stop))))))

;*---------------------------------------------------------------------*/
;*    bigloo-mangled? ...                                              */
;*---------------------------------------------------------------------*/
(define (bigloo-mangled? string)
   (let ((len (string-length string)))
      (and (>fx len 7)
	   (or (substring=? string "BgL_" 4)
	       (substring=? string "BGl_" 4))
	   (char=? (string-ref string (-fx len 3)) #\z)
	   (or (char-alphabetic? (string-ref string (-fx len 2)))
	       (char-numeric? (string-ref string (-fx len 2))))
	   (or (char-alphabetic? (string-ref string (-fx len 1)))
	       (char-numeric? (string-ref string (-fx len 1)))))))

;*---------------------------------------------------------------------*/
;*    bigloo-need-mangling? ...                                        */
;*---------------------------------------------------------------------*/
(define (bigloo-need-mangling? string)
   (let ((len (string-length string)))
      (and (>fx len 0)
	   (or (not (or (char-alphabetic? (string-ref string 0))
			(char=? (string-ref string 0) #\_)))
	       (let loop ((i 1))
		  (if (>=fx i len)
		      #f
		      (let ((c (string-ref string i)))
			 (if (or (char-alphabetic? c)
				 (char-numeric? c)
				 (char=? c #\_))
			     (loop (+fx i 1))
			     #t))))))))
		     
;*---------------------------------------------------------------------*/
;*    bigloo-demangle ...                                              */
;*---------------------------------------------------------------------*/
(define (bigloo-demangle string)
   (let* ((len (string-length string))
	  (clen (-fx len 3)))
      (define (err)
	 (error "bigloo-demangle" "Illegal mangling on" string))
      (define (char->digit c)
	 (if (char-numeric? c)
	     (-fx (char->integer c) (char->integer #\0))
	     (+fx 10 (-fx (char->integer c) (char->integer #\a)))))
      (define (get-8bits-integer r)
	 (let* ((c1 (string-ref string (+fx r 1)))
		(c2 (string-ref string (+fx r 2)))
		(i1 (char->digit c1))
		(i2 (char->digit c2)))
	    (+fx i1 (bit-lsh i2 4))))
      (define (bigloo-demangle-at offset)
	 (let ((new (make-string clen)))
	    (let loop ((r offset)
		       (w 0)
		       (checksum 0))
	       (if (=fx r clen)
		   ;; we still have to check the checksum
		   (if (=fx checksum (get-8bits-integer r))
		       (values (substring new 0 w) (+fx r 3))
		       (err))
		   (let ((c (string-ref string r)))
		      (if (char=? c #\z)
			  (if (char=? (string-ref string (+fx r 1)) #\z)
			      (values (substring new 0 (-fx w 1)) (+fx r 2))
			      (let* ((i (get-8bits-integer r))
				     (nc (integer->char i)))
				 (string-set! new w nc)
				 (loop (+fx r 3)
				       (+fx w 1)
				       (bit-xor checksum i))))
			  (begin
			     (string-set! new w c)
			     (loop (+fx r 1)
				   (+fx w 1)
				   checksum))))))))
      (define (bigloo-demangle-simple)
	 (multiple-value-bind (str offset)
	    (bigloo-demangle-at 4)
	    (values str #unspecified)))
      (define (bigloo-demangle-module)
	 (multiple-value-bind (id offset)
	    (bigloo-demangle-at 4)
	    (multiple-value-bind (module offset)
	       (bigloo-demangle-at offset)
	       (values id module))))
      (cond
	 ((<fx len 8)
	  string)
	 ((substring=? string "BgL_" 4)
	  (bigloo-demangle-simple))
	 ((substring=? string "BGl_" 4)
	  (bigloo-demangle-module))
	 (else
	  string))))

;*---------------------------------------------------------------------*/
;*    bigloo-class-mangled? ...                                        */
;*---------------------------------------------------------------------*/
(define (bigloo-class-mangled? string)
   (let ((len (string-length string)))
      (and (>fx len 8)
	   (char=? (string-ref string (-fx len 1)) #\t)
	   (char=? (string-ref string (-fx len 2)) #\l)
	   (char=? (string-ref string (-fx len 3)) #\g)
	   (char=? (string-ref string (-fx len 4)) #\b)
	   (char=? (string-ref string (-fx len 5)) #\_)
	   (bigloo-mangled? (substring string 0 (-fx len 5))))))
   
;*---------------------------------------------------------------------*/
;*    bigloo-class-demangle ...                                        */
;*---------------------------------------------------------------------*/
(define (bigloo-class-demangle string)
   (string-append (bigloo-demangle
		   (substring string 0 (-fx (string-length string) 5)))
		  "_bglt"))

;*---------------------------------------------------------------------*/
;*    *exit-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *exit-mutex* (make-mutex 'bigloo-exit))

;*---------------------------------------------------------------------*/
;*    bigloo-exit-mutex ...                                            */
;*---------------------------------------------------------------------*/
(define (bigloo-exit-mutex)
   *exit-mutex*)

;*---------------------------------------------------------------------*/
;*    *bigloo-exit-functions* ...                                      */
;*---------------------------------------------------------------------*/
(define *bigloo-exit-functions* '())

;*---------------------------------------------------------------------*/
;*    register-exit-function! ...                                      */
;*---------------------------------------------------------------------*/
(define (register-exit-function! fun)
   (synchronize (bigloo-exit-mutex)
      (if (not (correct-arity? fun 1))
	  (error "bigloo-exit-register!"
	     "Wrong procedure arity"
	     fun)
	  (set! *bigloo-exit-functions* (cons fun *bigloo-exit-functions*)))))

;*---------------------------------------------------------------------*/
;*    unregister-exit-function! ...                                    */
;*---------------------------------------------------------------------*/
(define (unregister-exit-function! fun)
   (synchronize (bigloo-exit-mutex)
      (set! *bigloo-exit-functions* (remq! fun *bigloo-exit-functions*))))

;*---------------------------------------------------------------------*/
;*    bigloo-exit-apply ...                                            */
;*---------------------------------------------------------------------*/
(define (bigloo-exit-apply val)
   (let ((mut (if (mutex? (bigloo-exit-mutex))
		  (bigloo-exit-mutex)
		  (make-mutex 'bigloo-exit))))
      (synchronize mut
	 (let loop ((val val))
	    (let ((val (if (integer? val)
			   val
			   0)))
	       (if (pair? *bigloo-exit-functions*)
		   (let ((fun (car *bigloo-exit-functions*)))
		      (set! *bigloo-exit-functions*
			 (cdr *bigloo-exit-functions*))
		      (let ((nval (fun val)))
			 (loop (if (integer? nval) nval val))))
		   val))))))

;*---------------------------------------------------------------------*/
;*    time ...                                                         */
;*---------------------------------------------------------------------*/
(define (time proc)
   (if (correct-arity? proc 0)
       ($time proc)
       (error 'time "Wrong procedure arity" proc)))

;*---------------------------------------------------------------------*/
;*    bmem-reset! ...                                                  */
;*    -------------------------------------------------------------    */
;*    This dummy function is overriden (LD_PRELOADed) by the bmem.so   */
;*    library. It is used by bmem to reset the allocation statistics   */
;*    gathering during the execution of a program, on user demand.     */
;*---------------------------------------------------------------------*/
(define (bmem-reset!)
   (cond-expand
      (bigloo-c ($bgl-bmem-reset))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bigloo-gc-verbose-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (bigloo-gc-verbose-set! proc)
   (cond-expand
      (bigloo-c ($bgl-gc-verbose-set! proc))
      (else #f)))
   
;*---------------------------------------------------------------------*/
;*    make-cell ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (make-cell val)
   ($make-cell val))

;*---------------------------------------------------------------------*/
;*    cell? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cell? obj)
   ($cell? obj))

;*---------------------------------------------------------------------*/
;*    cell-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (cell-ref cell)
   ($cell-ref cell))

;*---------------------------------------------------------------------*/
;*    cell-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (cell-set! cell val)
   ($cell-set! cell val))
