;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Rgc/rgcrules.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep  9 09:45:00 1998                          */
;*    Last change :  Sun Aug 25 09:11:30 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements the function `rules->tree' that translate */
;*    (canonicalize) the user set of clauses into on tree that         */
;*    represents the whole regular-expression that matches the whole   */
;*    language described by the grammar. The tree is represented by a  */
;*    list. This is easy for debugging and much more easy for writing  */
;*    functions that build the three (because we can use MAP,          */
;*    MATCH-CASE, etc).                                                */
;*    -------------------------------------------------------------    */
;*    During that stage characters may be represented as Scheme        */
;*    characters or as Scheme integers. In the three characters are    */
;*    exclusively represented as integers.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc_rules
   
   (import  __rgc_config
	    __rgc_set
	    __rgc_posix
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
	    __rgc
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

   (export  (rules->regular-tree user-env clauses)
	    (tree-max-char)
	    (special-char?::bool ::int)
	    (special-char-match?::bool ::int)
	    (special-match-char->rule-number::int ::int)
	    (predicate-match ::int)
	    (reset-special-match-char!)))

;*---------------------------------------------------------------------*/
;*    Some constants of the Rgc compiler ...                           */
;*---------------------------------------------------------------------*/
(define *max-rgc-=-num* 81)

;*---------------------------------------------------------------------*/
;*    rules->regular-tree ...                                          */
;*    -------------------------------------------------------------    */
;*    This function returns two values:                                */
;*      - a regular tree.                                              */
;*      - a list of actions.                                           */
;*---------------------------------------------------------------------*/
(define (rules->regular-tree user-env clauses)
   (reset-special-match-char!)
   (if (null? clauses)
       (error #f "RGC:Illegal clauses" clauses)
       (let ((env  (make-variable-env (append user-env (rgc-env))))
	     (dflt `(in (0 ,(- (rgc-max-char) 1)))))
	  (let loop ((clauses  clauses)
		     (match    0)
		     (branches '())
		     (actions  '())
		     (defs     '()))
	     (match-case (car clauses)
		((define . ?-)
		 (loop (cdr clauses)
		       match
		       branches
		       actions
		       (cons (car clauses) defs)))
		((?rule . (and (? pair?) ?acts))
		 (let ((act `(begin ,@acts)))
		    (if (null? (cdr clauses))
			;; this is the last clause
			(if (eq? rule 'else)
			    (values `(or ,(expand-match-rule match env dflt)
					 ,@branches)
				    (reverse! (cons act actions))
				    match
				    *submatch?*
				    defs)
			    (values `(or ,(expand-match-rule (+fx match 1)
							     env
							     dflt)
					 ,(expand-match-rule match env rule)
					 ,@branches)
				    (reverse! (cons* '(the-failure)
						     act
						     actions))
				    (+fx 1 match)
				    *submatch?*
				    defs))
			;; this is not the last clause
			(if (eq? rule 'else)
			    (error #f "RGC:Illegal else clause" clauses)
			    (let ((erule (expand-match-rule match env rule))) 
			       (loop (cdr clauses)
				     (+fx match 1)
				     (cons erule branches)
				     (cons act actions)
				     defs))))))
		(else
		 (error #f "RGC:Illegal clause" clauses)))))))

;*---------------------------------------------------------------------*/
;*    make-variable-env ...                                            */
;*    -------------------------------------------------------------    */
;*    This function simplify the regular expressions that are bound    */
;*    in the environment.                                              */
;*---------------------------------------------------------------------*/
(define (make-variable-env bindings)
   (if (null? bindings)
       '()
       (match-case (car bindings)
	  (((and (? symbol?) ?var) ?rule)
	   (let ((env (make-variable-env (cdr bindings))))
	      (cons (cons var rule) env)))
	  (else
	   (error #f
		  "RGC:Illegal regular variable definition"
		  (car bindings))))))

;*---------------------------------------------------------------------*/
;*    get-new-special-char ...                                         */
;*    -------------------------------------------------------------    */
;*    Special char are used to implement dummy transition in the DFA.  */
;*    A match of a rule, use a special char.                           */
;*---------------------------------------------------------------------*/
(define (get-new-special-char)
   (set! special-char-num (+fx 1 special-char-num))
   special-char-num)

;*---------------------------------------------------------------------*/
;*    special-char-num ...                                             */
;*---------------------------------------------------------------------*/
(define special-char-num (-fx (rgc-max-char) 1))

;*---------------------------------------------------------------------*/
;*    special-char? ...                                                */
;*    -------------------------------------------------------------    */
;*    Is a character a special RGC character? The answer is true       */
;*    iff CHAR is out of (rgc-max-char).                               */
;*---------------------------------------------------------------------*/
(define (special-char? char)
   (>=fx char (rgc-max-char)))

;*---------------------------------------------------------------------*/
;*    tree-max-char ...                                                */
;*    -------------------------------------------------------------    */
;*    This function returns the number of char used by the constructed */
;*    tree. This function is used when building the DFA so it is       */
;*    exported and used by __rgc_dfa.                                  */
;*---------------------------------------------------------------------*/
(define (tree-max-char)
   (+fx 1 special-char-num))

;*---------------------------------------------------------------------*/
;*    *special-match-char ...                                          */
;*---------------------------------------------------------------------*/
(define *special-match-char* '())
(define *special-start-match-char* '())
(define *special-stop-match-char* '())
(define *predicates* '())
(define *submatch?* #f)

;*---------------------------------------------------------------------*/
;*    reset-special-match-char! ...                                    */
;*---------------------------------------------------------------------*/
(define (reset-special-match-char!)
   (set! *submatch?* #f)
   (set! *predicates* '())
   (set! *special-match-char* '()))

;*---------------------------------------------------------------------*/
;*    add-special-match-char! ...                                      */
;*---------------------------------------------------------------------*/
(define (add-special-match-char! char rule)
   (set! *special-match-char* (cons (cons char rule) *special-match-char*)))

;*---------------------------------------------------------------------*/
;*    special-char-match? ...                                          */
;*---------------------------------------------------------------------*/
(define (special-char-match? char)
   (pair? (assq char *special-match-char*)))

;*---------------------------------------------------------------------*/
;*    special-match-char->rule-number ...                              */
;*---------------------------------------------------------------------*/
(define (special-match-char->rule-number char)
   (cdr (assq char *special-match-char*)))

;*---------------------------------------------------------------------*/
;*    *submatch-count* ...                                             */
;*---------------------------------------------------------------------*/
(define *submatch-count* 0)

;*---------------------------------------------------------------------*/
;*    reset-submatch! ...                                              */
;*---------------------------------------------------------------------*/
(define (reset-submatch!)
   (set! *submatch-count* 0))

;*---------------------------------------------------------------------*/
;*    get-new-submatch ...                                             */
;*---------------------------------------------------------------------*/
(define (get-new-submatch)
   (set! *submatch?* #t)
   (set! *submatch-count* (+fx 1 *submatch-count*))
   *submatch-count*)

;*---------------------------------------------------------------------*/
;*    add-predicate-match! ...                                         */
;*---------------------------------------------------------------------*/
(define (add-predicate-match! match predicate)
   (let ((cell (assq match *predicates*)))
      (if (pair? cell)
	  (set-cdr! cell (cons predicate (cdr cell)))
	  (set! *predicates* (cons (cons match (list predicate))
				   *predicates*)))))

;*---------------------------------------------------------------------*/
;*    predicate-match ...                                              */
;*---------------------------------------------------------------------*/
(define (predicate-match match::int)
   (let ((cell (assq match *predicates*)))
      (if (pair? cell)
	  (cdr cell)
	  #f)))

;*---------------------------------------------------------------------*/
;*    expand-match-rule ...                                            */
;*    -------------------------------------------------------------    */
;*    This function makes the pre-parsing of regular expression.       */
;*    In particular, it parses context and BOL, EOL, BOF and EOF       */
;*    forms.                                                           */
;*---------------------------------------------------------------------*/
(define (expand-match-rule match env rule)
   (reset-submatch!)
   (let ((special-char (get-new-special-char)))
      ;; we remember that SPECIAL-CHAR has been created for marking
      ;; the match of rule number MATCH
      (add-special-match-char! special-char match)
      (let loop ((rule rule))
	 (make-sequence
	    (list (let loop ((rule rule))
		     (match-case rule
			((when ?pred ?rule)
			 (add-predicate-match! match pred)
			 (loop rule))
			((context ?context ?rule)
			 (add-predicate-match! match
			    `(eq? the-rgc-context ',context))
			 (loop rule))
			((bol ?rule)
			 (add-predicate-match! match
			    '(rgc-buffer-bol? iport))
			 (loop rule))
			((eol ?rule)
			 (add-predicate-match! match
			    '(let ((r (rgc-buffer-eol? iport forward bufpos)))
			      (set! forward (rgc-buffer-forward iport))
			      (set! bufpos (rgc-buffer-bufpos iport))
			      r))
			 (loop rule))
			((bof ?rule)
			 (add-predicate-match! match
			    '(rgc-buffer-bof? iport))
			 (loop rule))
			((eof ?rule)
			 (add-predicate-match! match
			    '(let ((r (rgc-buffer-eof2? iport forward bufpos)))
			      (set! forward (rgc-buffer-forward iport))
			      (set! bufpos (rgc-buffer-bufpos iport))
			      r))
			 (loop rule))
			(else
			 (expand-rule match env rule))))
	       special-char)))))

;*---------------------------------------------------------------------*/
;*    expand-rule ...                                                  */
;*    -------------------------------------------------------------    */
;*    The general function that expands every constructions.           */
;*---------------------------------------------------------------------*/
(define (expand-rule match env rule)
   (if (not (pair? rule))
       (expand-atom match env rule)
       (match-case rule
	  ((... ?num ?r)             (expand-dots match env num r rule))
	  ((uncase ?rule)            (expand-uncase match env rule))
	  ((* ?rule)                 (expand-* match env rule))
	  ((+ ?rule)                 (expand-+ match env rule))
	  (((kwote ?) ?rule)         (expand-? match env rule))
	  (((kwote or) . ?rules)     (expand-or match env rules))
	  ((= ?num ?r)               (expand-= match env num r rule))
	  ((>= ?num ?r)              (expand->= match env num r rule))
	  ((** ?min ?max ?r)         (expand-** match env min max r rule))
	  ((in . ?values)            (expand-in match env values rule))
	  ((out . ?values)           (expand-out match env values rule))
	  (((kwote and) ?val1 ?val2) (expand-and match env val1 val2 rule))
	  ((but ?val1 ?val2)         (expand-but match env val1 val2 rule))
	  ((submatch ?r)             (expand-submatch match env r rule))
	  (((or : seq) . ?rules)     (expand-sequence match env rules))
	  ((posix ?string)           (expand-posix match env string rule))
	  (else (error #f "RGC:Illegal construction" rule)))))

;*---------------------------------------------------------------------*/
;*    expand-atom ...                                                  */
;*    -------------------------------------------------------------    */
;*    A legal atom is either a char, a string or a symbol that is      */
;*    declared in the environment.                                     */
;*---------------------------------------------------------------------*/
(define (expand-atom match env rule)
   (cond
      ((char? rule)
       (char->integer rule))
      ((and (fixnum? rule)
	    (>= rule 0)
	    (< rule (rgc-max-char)))
       (error #f "RGC:illegal atom" rule))
      ((string? rule)
       (expand-string env rule))
      ((symbol? rule)
       (let ((cell (assq rule env)))
	  (if (pair? cell)
	      ;; bindings rule may not have been already expanded
	      ;; (this is impossible because variable may hold
	      ;; submatch) so we have to expand the rule bound to
	      ;; variables.
	      (expand-rule match env (cdr cell))
	      (error #f "RGC:regular variable unbound" rule))))
      (else
       (error #f "RGC:Illegal regular expression" rule))))

;*---------------------------------------------------------------------*/
;*    expand-string ...                                                */
;*    -------------------------------------------------------------    */
;*    The regexp that matches the string.                              */
;*---------------------------------------------------------------------*/
(define (expand-string env rule::bstring)
   (if (=fx (string-length rule) 0)
       (error #f "RGC:Illegal empty string" rule)
       (make-sequence (map char->integer (string->list rule)))))

;*---------------------------------------------------------------------*/
;*    expand-dots ...                                                  */
;*    -------------------------------------------------------------    */
;*    This operator applies to sequences. It constructs a regexp       */
;*    that matches, the 1fst, the 1fst and 2nd, the 1fst and 2nd and   */
;*    3th, etc... parts of the regexp. For instance                    */
;*       (expand-dots (sequence r1 r2 r3 r4))                          */
;*          ->                                                         */
;*       (or (sequence r1)                                             */
;*           (sequence r1 r2)                                          */
;*           (sequence r1 r2 r3)                                       */
;*           (sequence r1 r2 r3 r4))                                   */
;*---------------------------------------------------------------------*/
(define (expand-dots match env num rule err)
   (define (explode-sequence rules)
      (let loop ((rules rules)
		 (i 0))
	 (if (or (null? rules) (=fx i num))
	     '()
	     (let ((first (car rules)))
		(cons (list first)
		      (map (lambda (r) (cons first r))
			   (loop (cdr rules) (+fx i 1))))))))
   (if (not (and (fixnum? num) (> num 0) (< num *max-rgc-=-num*)))
       (error #f "RGC:Illegal regular range" err)
       (match-case (expand-rule match env rule)
	  ((sequence . ?rules)
	   `(or ,@(map make-sequence (explode-sequence rules))))
	  (else
	   (error #f "RGC:Illegal regular expression" err)))))

;*---------------------------------------------------------------------*/
;*    expand-uncase ...                                                */
;*    -------------------------------------------------------------    */
;*    This function makes a recursive graph traversal over a           */
;*    regular-tree two make each characters either upper case or       */
;*    lower case. For instance:                                        */
;*       (uncase (sequence #\a #\b                                     */
;*                         (or (sequence #\c #\d)                      */
;*                             (sequence #\e #\f))))                   */
;*    is turned into:                                                  */
;*       (sequence (or #\A #\a)                                        */
;*                 (or #\B #\b)                                        */
;*                 (or (sequence (or #\C #\c) (or #\D #\d))            */
;*                     (sequence (or #\E #\e) (or #\F #\f))))          */
;*                                                                     */
;*    This function does not know anything about the operators that    */
;*    are used to represent a tree, it recursively walk thru a list,   */
;*    stopping on each leave and checking if it is a char or not.      */
;*---------------------------------------------------------------------*/
(define (expand-uncase match env rule)
   (let loop ((rule (expand-rule match env rule))
	      (res  '()))
      (cond
	 ((null? rule)
	  (reverse! res))
	 ((not (pair? rule))
	  (if (fixnum? rule)
	      (if (rgc-alphabetic? rule)
		  ;; when a char is found we emit the tree that will
		  ;; match it either is lower case or upper case. that is,
		  ;; we turn a char into into an `or' construction.
		  `(or ,(rgc-upcase rule) ,(rgc-downcase rule) ,@res)
		  rule)
	      rule))
	 ((pair? (car rule))
	  (loop (cdr rule)
		(cons (loop (car rule) '()) res)))
	 ((fixnum? (car rule))
	  (if (rgc-alphabetic? (car rule))
	      ;; when a char is found we emit the tree that will
	      ;; match it either is lower case or upper case. that is,
	      ;; we turn a char into into an `or' construction.
	      (loop (cdr rule)
		    (cons `(or ,(rgc-upcase (car rule))
			       ,(rgc-downcase (car rule)))
			  res))
	      (loop (cdr rule)
		    (cons (car rule) res))))
	 (else
	  (loop (cdr rule)
		(cons (car rule) res))))))

;*---------------------------------------------------------------------*/
;*    expand-* ...                                                     */
;*---------------------------------------------------------------------*/
(define (expand-* match env rule)
   `(* ,(expand-rule match env rule)))   

;*---------------------------------------------------------------------*/
;*    expand-+ ...                                                     */
;*---------------------------------------------------------------------*/
(define (expand-+ match env rule)
   (let ((erule (expand-rule match env rule)))
      `(sequence ,erule (* ,erule))))

;*---------------------------------------------------------------------*/
;*    expand-? ...                                                     */
;*---------------------------------------------------------------------*/
(define (expand-? match env rule)
   `(or epsilon ,(expand-rule match env rule)))

;*---------------------------------------------------------------------*/
;*    expand-or ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-or match env rules)
   (if (null? rules)
       'epsilon
       (let loop ((rules rules)
		  (res   '()))
	  (if (null? rules)
	      `(or ,@(reverse! res))
	      (let ((rule (expand-rule match env (car rules))))
		 (if (and (pair? rule) (eq? (car rule) 'or))
		     (loop (cdr rules) (append (reverse! (cdr rule)) res))
		     (loop (cdr rules) (cons rule res))))))))

;*---------------------------------------------------------------------*/
;*    expand-= ...                                                     */
;*    -------------------------------------------------------------    */
;*    This operator stands for n times the rule. So we expand:         */
;*       (= 3 r)                                                       */
;*    into:                                                            */
;*       (sequence r r r)                                              */
;*---------------------------------------------------------------------*/
(define (expand-= match env num rule err)
   (if (not (and (fixnum? num) (> num 0) (< num *max-rgc-=-num*)))
       (error #f "RGC:Illegal regular range" err)
       (let ((erule (expand-rule match env rule)))
	  (make-sequence (vector->list (make-vector num erule))))))

;*---------------------------------------------------------------------*/
;*    expand->= ...                                                    */
;*    -------------------------------------------------------------    */
;*    This operator stands for n or more times the rule. So we expand: */
;*       (>= 3 r)                                                      */
;*    into:                                                            */
;*       (sequence r r r (* r))                                        */
;*---------------------------------------------------------------------*/
(define (expand->= match env num rule err)
   (if (not (and (fixnum? num) (> num 0) (< num *max-rgc-=-num*)))
       (error #f "RGC:Illegal regular range" err)
       (let ((erule (expand-rule match env rule)))
	  (make-sequence (append (vector->list (make-vector num erule))
				 `((* ,erule)))))))

;*---------------------------------------------------------------------*/
;*    expand-** ...                                                    */
;*    -------------------------------------------------------------    */
;*    This operator stands for n to m times the rule. So we expand:    */
;*       (** 3 5 r)                                                    */
;*    into:                                                            */
;*       (or (= 3 r) (= 4 r) (= 5 r))                                  */
;*---------------------------------------------------------------------*/
(define (expand-** match env min max rule err)
   (if (not (and (fixnum? min)
		 (> min 0)
		 (fixnum? max)
		 (> max min)
		 (< max *max-rgc-=-num*)))
       (error #f "RGC:Illegal regular range" err)
       (let ((erule (expand-rule match env rule)))
	  (let loop ((min min)
		     (res '()))
	     (if (> min max)
		 `(or ,@(reverse! res))
		 (loop (+ min 1)
		       (cons (make-sequence
			      (vector->list (make-vector min erule)))
			     res)))))))

;*---------------------------------------------------------------------*/
;*    expand-in ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function performs characters and string set expansion.      */
;*    Each value is either a char, a string, a list of two chars       */
;*    a list of string. Here is the expansion examples:                */
;*       (in #\a)       --> #\a                                        */
;*       (in "abcd")    --> (or #\a #\b #\c #\d)                       */
;*       (in (#\a #\d)) --> (or #\a #\b #\c #\d)                       */
;*       (in ("ad"))    --> (or #\a #\b #\c #\d)                       */
;*---------------------------------------------------------------------*/
(define (expand-in match env values err)
   (define (rgc-char? x)
      ;; this predicate is true if a char or an integer is a legal
      ;; char for rgc (bound checking).
      (or (char? x)
	  (and (fixnum? x)
	       (>=fx x 0)
	       (<fx x (rgc-max-char)))))
   (define (char-range min max)
      (let ((min (if (char? min) (char->integer min) min))
	    (max (if (char? max) (char->integer max) max)))
	 (cond
	    ((>=fx max min)
	     (let loop ((max max)
			(res '()))
		(if (=fx max min)
		    (cons min res)
		    (loop (-fx max 1) (cons max res)))))
	    (else
	     (error #f "RGC:Illegal range" err)))))
   (define (string-range string)
      (let ((len (string-length string)))
	 (if (> (remainderfx len 2) 0)
	     (error #f "RGC:Illegal range string" err)
	     (let loop ((i   0)
			(res '()))
		(if (=fx i len)
		    res
		    (loop (+fx i 2)
			  (append (char-range (string-ref string i)
					      (string-ref string (+fx i 1)))
				  res)))))))
   (if (null? values)
       'epsilon
       (let loop ((values values)
		  (chars  '()))
	  (if (null? values)
	      `(or ,@chars)
	      (let ((value (car values)))
		 (match-case value
		    ((? rgc-char?)
		     (loop (cdr values) (cons (if (char? value)
						  (char->integer value)
						  value)
					      chars)))
		    ((? string?)
		     (if (=fx (string-length value) 0)
			 'epsilon
			 (loop (cdr values)
			       (append (map char->integer (string->list value))
				       chars))))
		    (((and (? rgc-char?) ?min) (and (? rgc-char?) ?max))
		     (loop (cdr values) (append (char-range min max) chars)))
		    (((and ?str (? string?)))
		     (loop (cdr values) (append (string-range str) chars)))
		    (((kwote or) . ?or-chars)
		     (let laap ((cs or-chars)
				(res '()))
			(cond
			   ((null? cs)
			    (loop (cdr values)
				  (append res chars)))
			   ((rgc-char? (car cs))
			    (laap (cdr cs) (cons (car cs) res)))
			   ((and (pair? (car cs)) (eq? (caar cs) 'or))
			    (laap (append (cdar cs) (cdr cs))
				  res))
			   (else
			    (error #f "RGC:Illegal construction" err)))))
		    (else
		     (loop (cons (expand-rule match env (car values))
				 (cdr values))
			   chars))))))))
 
;*---------------------------------------------------------------------*/
;*    expand-out ...                                                   */
;*    -------------------------------------------------------------    */
;*    To compute the `out' operator, we simply computes the negation   */
;*    of the in. That is, we start computing in and then, we inverse   */
;*    the result.                                                      */
;*---------------------------------------------------------------------*/
(define (expand-out match env values err)
   (let* ((in (expand-in match env values err))
	  (bc (list->rgcset (cdr in) (rgc-max-char))))
      (rgcset-not! bc)
      `(or ,@(rgcset->list bc))))

;*---------------------------------------------------------------------*/
;*    expand-and ...                                                   */
;*    -------------------------------------------------------------    */
;*    This operator is and intersection of char sets.                  */
;*---------------------------------------------------------------------*/
(define (expand-and match env val1 val2 err)
   (let* ((in1 (expand-in match env (list val1) err))
	  (in2 (expand-in match env (list val2) err))
	  (bc1 (list->rgcset (cdr in1) (rgc-max-char)))
	  (bc2 (list->rgcset (cdr in2) (rgc-max-char))))
      (rgcset-and! bc1 bc2)
      `(or ,@(rgcset->list bc1))))

;*---------------------------------------------------------------------*/
;*    expand-but ...                                                   */
;*    -------------------------------------------------------------    */
;*    The disjonction of char sets.                                    */
;*---------------------------------------------------------------------*/
(define (expand-but match env val1 val2 err)
   (let* ((in1 (expand-in match env (list val1) err))
	  (in2 (expand-in match env (list val2) err))
	  (bc1 (list->rgcset (cdr in1) (rgc-max-char)))
	  (bc2 (list->rgcset (cdr in2) (rgc-max-char))))
      (rgcset-but! bc1 bc2)
      `(or ,@(rgcset->list bc1))))

;*---------------------------------------------------------------------*/
;*    *lock-submatch*  ...                                             */
;*---------------------------------------------------------------------*/
(define *lock-submatch* #f)

;*---------------------------------------------------------------------*/
;*    expand-submatch ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-submatch match env rule err)
   (if *lock-submatch*
       (error #f "RGC:Illegal nested submatches" err)
       (let ((submatch (get-new-submatch)))
	  (set! *lock-submatch* #t)
	  (let ((res `(submatch ,match
				,submatch
				,(expand-rule match env rule))))
	     (set! *lock-submatch* #f)
	     res))))

;*---------------------------------------------------------------------*/
;*    expand-special ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-special match env operator rule)
   `(,operator ,(expand-rule match env rule)))
   
;*---------------------------------------------------------------------*/
;*    expand-sequence ...                                              */
;*    -------------------------------------------------------------    */
;*    The sequence of regexp (that is, regexp1, then regexp2, then     */
;*    regexp3, etc, etc.).                                             */
;*---------------------------------------------------------------------*/
(define (expand-sequence match env rules::pair)
   (make-sequence (map (lambda (r) (expand-rule match env r)) rules)))

;*---------------------------------------------------------------------*/
;*    make-sequence ...                                                */
;*    -------------------------------------------------------------    */
;*    This function normalizes regexp. That is:                        */
;*       (sequence 1 2 3 (sequence 4 5 6) 7 8)                         */
;*    is turned into                                                   */
;*       (sequence 1 2 3 4 5 6 7 8)                                    */
;*    We _don't_ have to check for deep nesting of sequence            */
;*    forms because _each_ sequence is added by this                   */
;*    function thus, this situation is impossible:                     */
;*       (sequence 1 (sequence 2 (sequence 3) ...) ...)                */
;*---------------------------------------------------------------------*/
(define (make-sequence rules::pair)
   (let loop ((rules rules)
	      (res  '()))
      (match-case rules
	 (()
	  `(sequence ,@(reverse! res)))
	 (((sequence . ?rs) . ?rules)
	  (loop rules (append (reverse rs) res)))
	 (else
	  (loop (cdr rules) (cons (car rules) res))))))
   
;*---------------------------------------------------------------------*/
;*    expand-posix ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-posix match env string err)
   (if (not (string? string))
       (error #f "RGC:Illegal construction" err)
       (expand-rule match env (posix->rgc string))))
   
	      
