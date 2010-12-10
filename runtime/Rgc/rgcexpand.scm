;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Rgc/rgcexpand.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep  9 09:21:29 1998                          */
;*    Last change :  Fri Nov 26 18:59:04 2010 (serrano)                */
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
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
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
			  (sexp (make-regular-parser args
						     (compile-dfa submatches
								  dfa
								  positions)
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
       (lambda (input-port ,@args)
	  ;; compiled states
	  ,@states
	  ;; rgc library functions
	  ;; @deffn the-port@
	  (define (the-port::input-port)
	     input-port)
	  ;; @deffn the-character@
	  (define (the-character::char)
	     (rgc-buffer-character input-port))
	  ;; @deffn the-byte@
	  (define (the-byte::int)
	     (rgc-buffer-byte input-port))
	  ;; @deffn the-byte-ref@
	  (define (the-byte-ref::int offset::int)
	     (rgc-buffer-byte-ref input-port offset))
	  ;; @deffn the-string@
	  (define (the-string::bstring)
	     (rgc-buffer-substring input-port 0 (the-length)))
	  ;; @deffn the-substring@
	  (define (the-substring::bstring min::int max::int)
	     (when (<fx max 0) (set! max (+fx (the-length) max)))
	     (if (and (>=fx min 0) (>=fx max min) (<=fx max (the-length)))
		 (rgc-buffer-substring input-port min max)
		 (error "the-substring"
			((@ format __r4_output_6_10_3)
			 "Illegal range `~a'" (the-string))
			(cons min max))))
	  ;; @deffn the-escape-substring@
	  (define (the-escape-substring::bstring min::int max::int strict::bool)
	     (when (<fx max 0) (set! max (+fx (the-length) max)))
	     (if (and (>=fx min 0) (>=fx max min) (<=fx max (the-length)))
		 (rgc-buffer-escape-substring input-port min max strict)
		 (error "the-escape-substring"
			((@ format __r4_output_6_10_3)
			 "Illegal range `~a'" (the-string))
			(cons min max))))
	  ;; @deffn the-length@
	  (define (the-length::int)
	     (rgc-buffer-length input-port))
	  ;; @deffn the-fixnum@
	  (define (the-fixnum::long)
	     (rgc-buffer-fixnum input-port))
	  ;; @deffn the-integer@
	  (define (the-integer::obj)
	     (rgc-buffer-integer input-port))
	  ;; @deffn the-flonum@
	  (define (the-flonum::double)
	     (rgc-buffer-flonum input-port))
	  ;; @deffn the-symbol@
	  (define (the-symbol::symbol)
	     (rgc-buffer-symbol input-port))
	  ;; @deffn the-subsymbol@
	  (define (the-subsymbol::symbol min max)
	     (if (<fx max 0)
		 (let ((stop (+fx (the-length) max)))
		    (if (<=fx stop min)
			(error "the-subsymbol" "Illegal range" (cons min max))
			(rgc-buffer-subsymbol input-port min stop)))
		 (if (and (>=fx min 0) (<=fx max (the-length)) (>=fx max min))
		     (rgc-buffer-subsymbol input-port min max)
		     (error "the-subsymbol" "Illegal range" (cons min max)))))
	  ;; @deffn the-downcase-symbol@
	  (define (the-downcase-symbol::symbol)
	     (rgc-buffer-downcase-symbol input-port))
	  ;; @deffn the-upcase-symbol@
	  (define (the-upcase-symbol::symbol)
	     (rgc-buffer-upcase-symbol input-port))
	  ;; @deffn the-keyword@
	  (define (the-keyword::keyword)
	     (rgc-buffer-keyword input-port))
	  ;; @deffn the-downcase-keyword@
	  (define (the-downcase-keyword::keyword)
	     (rgc-buffer-downcase-keyword input-port))
	  ;; @deffn the-upcase-keyword@
	  (define (the-upcase-keyword::keyword)
	     (rgc-buffer-upcase-keyword input-port))
	  ;; @deffn the-failure@
	  (define (the-failure)
	     (if (=fx (rgc-buffer-length input-port) 0)
		 ;; this is the end-of-file object
		 #<0100>
		 (rgc-buffer-character input-port)))
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
	  ,@(if submatch?
		(list
		 '(define rgc-submatches (quote ()))
		 '(define (rgc-submatch-start! match::int submatch::int)
		   (set! rgc-submatches
			 (cons (vector match
				       submatch
				       (rgc-buffer-position input-port)
				       (quote start))
			       rgc-submatches)))
		 '(define (rgc-submatch-start*! match::int submatch::int)
		   (set! rgc-submatches
			 (cons (vector match
				       submatch
				       (rgc-buffer-position input-port)
				       (quote start*))
			       rgc-submatches)))
		 '(define (rgc-submatch-stop! match::int submatch::int)
		   (set! rgc-submatches
			 (cons (vector match
				       submatch
				       (rgc-buffer-position input-port)
				       (quote stop))
			       rgc-submatches))))
		'())
	  ;; user definitions
	  ,@defs
	  ;; main function
	  (define (ignore)
	     (rgc-start-match! input-port)
	     ,@(if submatch?
		   (list '(set! rgc-submatches (quote ())))
		   '())
	     (let ((match::long (,(state-name (get-initial-state))
				 input-port
				 ,else-num)))
		(rgc-set-filepos! input-port)
		,@(if submatch?
		      ;; @deffn the-submatch@
		      '((define (the-submatch num)
			   (if (=fx num 0)
			       (the-string)
			       (multiple-value-bind (start stop)
				  (rgc-the-submatch rgc-submatches
						    (rgc-buffer-position
						     input-port)
						    match
						    num)
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
		 (error "regular-grammar"
			"Can't read on a closed input port"
			(the-port))
		 (ignore))))))
		    
