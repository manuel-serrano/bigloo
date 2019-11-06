;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Rgc/rgcexpand.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep  9 09:21:29 1998                          */
;*    Last change :  Sun Aug 25 09:12:02 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The expanders that implements the RGC user forms.                */
;*    -------------------------------------------------------------    */
;*    This module implements the expanders for:                        */
;*       - regular-grammar                                             */
;*       - string-case                                                 */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/rgc.texi@                                 */
;*       @node Regular Parsing@                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc_expand

   (import  __rgc_rules
	    __rgc_tree
	    __rgc_dfa
	    __rgc_compile
	    __rgc_config
	    __rgc
	    __rgc_set
	    __error)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __param
	    __object
	    __thread
	    __bexit
	    __bignum
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
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r5_control_features_6_4
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r4_vectors_6_8)

   (export  (expand-string-case x e)
	    (expand-regular-grammar x e)))

;*---------------------------------------------------------------------*/
;*    expand-string-case ...                                           */
;*    -------------------------------------------------------------    */
;*    This function expands the @deffn string-case@ form               */
;*    -------------------------------------------------------------    */
;*    This expander expands form like:                                 */
;*       (string-case s                                                */
;*          (("toto") 'match1)                                         */
;*          ((+ (in "abcde")) 'match2)                                 */
;*          (else 'else))                                              */
;*---------------------------------------------------------------------*/
(define (expand-string-case x e)
   (match-case x
      ((?- ?str . ?clauses)
       (let ((port-id (gensym 'port)))
	  (let ((new `(let ((,port-id (open-input-string ,str)))
			 (unwind-protect
			    (read/rp (regular-grammar ()
					,@clauses)
				     ,port-id)
			    (close-input-port ,port-id)))))
	     (set-car! x (car new))
	     (set-cdr! x (cdr new))
	     (e x e))))
      (else
       (error "string-case" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-regular-grammar ...                                       */
;*    -------------------------------------------------------------    */
;*    This function expands the @deffn regular-grammar@ form           */
;*---------------------------------------------------------------------*/
(define (expand-regular-grammar x e)
   (define (split-regular-grammar-options opts)
      (let loop ((os opts)
		 (vars '())
		 (args '()))
	 (cond
	    ((null? os)
	     (values (reverse! vars) (reverse! args)))
	    ((symbol? (car os))
	     (loop (cdr os) vars (cons (car os) args)))
	    (else
	     (loop (cdr os) (cons (car os) vars) args)))))
   (match-case x
      ((?- ?opts . ?clauses)
       (multiple-value-bind (uenv args)
	  (split-regular-grammar-options opts)
	  (multiple-value-bind (tree actions else-num submatch? defs)
	     ;; we normalize the grammar. that is we build one uniq
	     ;; regular expression from the grammar
	     (rules->regular-tree uenv clauses)
	     (multiple-value-bind (node followpos positions submatches)
		;; we build the tree, that is we translate a list into
		;; a data structure that suits the algorithm for building
		;; the dfa
		(regular-tree->node tree)
		;; We now build the dfa transitions.
		(begin
		   (let* ((dfa (node->dfa node followpos positions))
			  (sexp (make-regular-parser
				   args
				   (compile-dfa submatches dfa positions)
				   actions
				   else-num
				   submatch?
				   defs)))
		      (reset-special-match-char!)
		      (reset-tree!)
		      (reset-dfa!)
		      (e sexp e)))))))
      (else
       (error "regular-grammar" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    make-regular-parser ...                                          */
;*---------------------------------------------------------------------*/
(define (make-regular-parser args states actions else-num submatch? defs)
   `(let ((the-rgc-context #unspecified))
       (lambda (iport ,@args)
	  ;; submatches (to be removed because wrong)
	  ,@(if submatch?
		(list
		   '(define rgc-submatches (quote ()))
		   '(define (rgc-submatch-start2! match::int submatch::int forward)
		     (set! rgc-submatches
			(cons (vector match
				 submatch
				 (rgc-buffer-position iport forward)
				 (quote start))
			   rgc-submatches)))
		   '(define (rgc-submatch-start*2! match::int submatch::int forward)
		     (set! rgc-submatches
			(cons (vector match
				 submatch
				 (rgc-buffer-position iport forward)
				 (quote start*))
			   rgc-submatches)))
		   '(define (rgc-submatch-stop2! match::int submatch::int forward)
		     (set! rgc-submatches
			(cons (vector match
				 submatch
				 (rgc-buffer-position iport forward)
				 (quote stop))
			   rgc-submatches))))
		'())	  ;; rgc library functions
	  ;; compiled states
	  ,@states
	  ;; @deffn the-port@
	  (define (the-port::input-port)
	     iport)
	  ;; @deffn the-character@
	  (define (the-character::char)
	     (rgc-buffer-character iport))
	  ;; @deffn the-byte@
	  (define (the-byte::int)
	     (rgc-buffer-byte iport))
	  ;; @deffn the-byte-ref@
	  (define (the-byte-ref::int offset::int)
	     (rgc-buffer-byte-ref iport offset))
	  ;; @deffn the-string@
	  (define (the-string::bstring)
	     (rgc-buffer-substring iport 0 (the-length)))
	  ;; @deffn the-substring@
	  (define (the-substring::bstring min::int max::int)
	     (when (<fx max min) (set! max (+fx (the-length) max)))
	     (if (and (>=fx min 0) (>=fx max min) (<=fx max (the-length)))
		 (rgc-buffer-substring iport min max)
		 (error "the-substring"
		    ((@ format __r4_output_6_10_3)
		     "Illegal range `~a'" (the-string))
		    (cons min max))))
	  ;; @deffn the-escape-substring@
	  (define (the-escape-substring::bstring min::int max::int strict::bool)
	     (when (<fx max 0) (set! max (+fx (the-length) max)))
	     (if (and (>=fx min 0) (>=fx max min) (<=fx max (the-length)))
		 (rgc-buffer-escape-substring iport min max strict)
		 (error "the-escape-substring"
		    ((@ format __r4_output_6_10_3)
		     "Illegal range `~a'" (the-string))
		    (cons min max))))
	  ;; @deffn the-length@
	  (define (the-length::int)
	     (rgc-buffer-length iport))
	  ;; @deffn the-fixnum@
	  (define (the-fixnum::long)
	     (rgc-buffer-fixnum iport))
	  ;; @deffn the-integer@
	  (define (the-integer::obj)
	     (rgc-buffer-integer iport))
	  ;; @deffn the-flonum@
	  (define (the-flonum::double)
	     (rgc-buffer-flonum iport))
	  ;; @deffn the-symbol@
	  (define (the-symbol::symbol)
	     (rgc-buffer-symbol iport))
	  ;; @deffn the-subsymbol@
	  (define (the-subsymbol::symbol min max)
	     (if (<fx max 0)
		 (let ((stop (+fx (the-length) max)))
		    (if (<=fx stop min)
			(error "the-subsymbol" "Illegal range" (cons min max))
			(rgc-buffer-subsymbol iport min stop)))
		 (if (and (>=fx min 0) (<=fx max (the-length)) (>=fx max min))
		     (rgc-buffer-subsymbol iport min max)
		     (error "the-subsymbol" "Illegal range" (cons min max)))))
	  ;; @deffn the-downcase-symbol@
	  (define (the-downcase-symbol::symbol)
	     (rgc-buffer-downcase-symbol iport))
	  ;; @deffn the-downcase-subsymbol@
	  (define (the-downcase-subsymbol::symbol min max)
	     (if (<fx max 0)
		 (let ((stop (+fx (the-length) max)))
		    (if (<=fx stop min)
			(error "the-downcase-subsymbol"
			   "Illegal range" (cons min max))
			(rgc-buffer-downcase-subsymbol iport min stop)))
		 (if (and (>=fx min 0) (<=fx max (the-length)) (>=fx max min))
		     (rgc-buffer-downcase-subsymbol iport min max)
		     (error "the-downcase-subsymbol"
			"Illegal range" (cons min max)))))
	  ;; @deffn the-upcase-symbol@
	  (define (the-upcase-symbol::symbol)
	     (rgc-buffer-upcase-symbol iport))
	  ;; @deffn the-upcase-subsymbol@
	  (define (the-upcase-subsymbol::symbol min max)
	     (if (<fx max 0)
		 (let ((stop (+fx (the-length) max)))
		    (if (<=fx stop min)
			(error "the-upcase-subsymbol"
			   "Illegal range" (cons min max))
			(rgc-buffer-upcase-subsymbol iport min stop)))
		 (if (and (>=fx min 0) (<=fx max (the-length)) (>=fx max min))
		     (rgc-buffer-upcase-subsymbol iport min max)
		     (error "the-upcase-subsymbol"
			"Illegal range" (cons min max)))))
	  ;; @deffn the-keyword@
	  (define (the-keyword::keyword)
	     (rgc-buffer-keyword iport))
	  ;; @deffn the-downcase-keyword@
	  (define (the-downcase-keyword::keyword)
	     (rgc-buffer-downcase-keyword iport))
	  ;; @deffn the-upcase-keyword@
	  (define (the-upcase-keyword::keyword)
	     (rgc-buffer-upcase-keyword iport))
	  ;; @deffn the-failure@
	  (define (the-failure)
	     (if (=fx (rgc-buffer-length iport) 0)
		 ;; this is the end-of-file object
		 (eof-object)
		 (rgc-buffer-character iport)))
	  ;; @deffn the-context@
	  (define (the-context)
	     the-rgc-context)
	  ;; @deffn rgc-context?@
	  (define (rgc-context?::bool context)
	     (eq? the-rgc-context context))
	  ;; @deffn rgc-context@
	  (define (rgc-set-context! context)
	     (set! the-rgc-context context))
	  (define (rgc-context . context)
	     (if (pair? context)
		 (set! the-rgc-context (car context))
		 (set! the-rgc-context #unspecified)))
	  ;; user definitions
	  ,@defs
	  ;; main function
	  (define (ignore)
	     (rgc-start-match! iport)
	     ,@(if submatch?
		   (list '(set! rgc-submatches (quote ())))
		   '())
	     (let ((match::long (,(state-name (get-initial-state))
				 iport
				 ,else-num
				 (rgc-buffer-forward iport)
				 (rgc-buffer-bufpos iport))))
		(rgc-set-filepos! iport)
		,@(if submatch?
		      ;; @deffn the-submatch@
		      '((define (the-submatch num)
			   (if (=fx num 0)
			       (the-string)
			       (multiple-value-bind (start stop)
				  (rgc-the-submatch rgc-submatches
				     (rgc-buffer-position iport
					(rgc-buffer-forward iport))
				     match num)
				  (if (and (>=fx start 0) (>=fx stop start))
				      (the-substring start stop)
				      "")))))
		      '())
		(case match
		   ,@(let loop ((actions actions)
				(num     0)
				(res     '()))
		      (if (null? actions)
			  res
			  (loop (cdr actions)
			     (+fx num 1)
			     (cons `((,num) ,(car actions)) res))))
		   (else
		    (error "regular-grammar" "Illegal match" match)))))
	  ;; we start parsing. See the module __rgc for the definition
	  ;; of the unsafe-rgc variable (@ref rgc.scm:unsafe-rgc@)
	  ;; @label unsafe-rgc@
	  ,(if *unsafe-rgc*
	       '(ignore)
	       '(if (closed-input-port? (the-port))
		 (raise
		    (instantiate::&io-closed-error
		       (proc "regular-grammar")
		       (msg "Can't read on a closed input port")
		       (obj (the-port))))
		 (ignore))))))
		    
