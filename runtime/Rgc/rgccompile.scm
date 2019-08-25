;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Rgc/rgccompile.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 13 07:46:39 1998                          */
;*    Last change :  Sun Aug 25 09:07:40 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements the DFA compilation. Each state is        */
;*    compiled into a lambda expression.                               */
;*=====================================================================*/
  
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc_compile

   (import  __rgc_rules
	    __rgc_dfa
	    __rgc_set
	    __rgc_config
	    __error)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __os
	    __pp
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

   (export (compile-dfa submatches dfa positions)))

;*---------------------------------------------------------------------*/
;*    compile-dfa ...                                                  */
;*---------------------------------------------------------------------*/
(define (compile-dfa submatches states positions)
   (init-compile-member-vector!)
   (let ((res (map (lambda (state)
		      (compile-state submatches state positions))
		   states)))
      (reset-compile-member-vector!)
      res))

;*---------------------------------------------------------------------*/
;*    *case-threshold* ...                                             */
;*    -------------------------------------------------------------    */
;*    The transition numbers threshold. More than *CASE-THRESHOLD*     */
;*    transitions for the DFA compiler to use a CASE construction      */
;*    instead of a COND. I have conducted experience with the          */
;*    compilation of the Read/read.scm file to set up this value.      */
;*---------------------------------------------------------------------*/
(define *case-threshold*
   (let ((value 80))
      (if *rgc-optim* (/fx value 2) value)))

;*---------------------------------------------------------------------*/
;*    compile-state ...                                                */
;*    -------------------------------------------------------------    */
;*    A DFA state is compiled into a Bigloo local function. These      */
;*    functions accepts exactly two arguments:                         */
;*       - LAST-CHAR the last-char read                                */
;*       - LAST-MAX the last matching rule                             */
;*    Before reading any new char, the function test for special       */
;*    chars (such as match, submatch, eol, ...) then, if necessary,    */
;*    it reads a new char a select a new transition.                   */
;*    -------------------------------------------------------------    */
;*    The variable POSITIONS-TO-CHAR is a mapping from position        */
;*    to real chars. It is used only when compiling submatch.          */
;*---------------------------------------------------------------------*/
(define (compile-state submatches state positions-to-char)
   ;; we start splittint transition in two sets:
   ;;    1. set for special
   ;;    2. set for regular chars
   `(define (,(state-name state) iport last-match forward bufpos)
       ,(let ((transitions (state-transitions state))
	      (positions (state-positions state)))
	   (if (null? transitions)
	       'last-match
	       (multiple-value-bind (special-trans regular-trans)
		  (split-transitions transitions)
		  (let ((match-body (compile-match special-trans)))
		     (if match-body
			 `(let ((new-match ,match-body))
			     ,(compile-regular
				 submatches state
				 regular-trans 'new-match positions-to-char))
			 (compile-regular
			    submatches state
			    regular-trans 'last-match positions-to-char))))))))

;*---------------------------------------------------------------------*/
;*    split-transitions ...                                            */
;*    -------------------------------------------------------------    */
;*    Split the out transition of a state in two sets, one for regular */
;*    chars and one for special chars.                                 */
;*---------------------------------------------------------------------*/
(define (split-transitions transitions)
   (let loop ((transitions transitions)
	      (specials '())
	      (regulars '()))
      (cond
	 ((null? transitions)
	  (values specials regulars))
	 ((special-char? (car (car transitions)))
	  (loop (cdr transitions)
		(cons (car transitions) specials)
		regulars))
	 (else
	  (loop (cdr transitions)
		specials
		(cons (car transitions) regulars))))))
	  
;*---------------------------------------------------------------------*/
;*    compile-regular ...                                              */
;*---------------------------------------------------------------------*/
(define (compile-regular submatches current-state transitions last-match p->c)
   ;; the first set is to build the <state x transitions>
   ;; association list
   (let ((state-trans (state-transition-list transitions))
	 (positions (state-positions current-state)))
      (if (null? state-trans)
	  last-match
	  `(if (=fx forward bufpos)
	       ;; the buffer is empty
	       (if (rgc-fill-buffer iport)
		   ,(compile-jump-to-state current-state 'last-match
		       '(rgc-buffer-forward iport)
		       '(rgc-buffer-bufpos iport))
		   ,last-match)
	       ;; Generate a `case' construction instead of `cond' when the
	       ;; the number of transitions exceeds a fixed threshold
	       (let* ((cur::int (rgc-buffer-get-char iport forward)))
		  ,@(compile-submatches 'cur submatches positions p->c)
		  ,(if (<=fx (length state-trans) 12)
		       (compile-cond-regular
			  current-state state-trans last-match)
		       (compile-case-regular
			  current-state state-trans last-match)))))))

;*---------------------------------------------------------------------*/
;*    compile-jump-to-state ...                                        */
;*---------------------------------------------------------------------*/
(define (compile-jump-to-state state match forward bufpos)
   `(,(state-name state) iport ,match ,forward ,bufpos))

;*---------------------------------------------------------------------*/
;*    compile-case-regular ...                                         */
;*---------------------------------------------------------------------*/
(define (compile-case-regular current-state state-trans match)
   
   (define (compile-case-transition state-trans)
      (let ((set (cdr state-trans))
	    (state (car state-trans)))
	 (let ((case-test (rgcset->list set)))
	    `(,case-test ,(compile-jump-to-state state match
			     '(+fx 1 forward) 'bufpos)))))
   
   `(case cur
       ,@(map compile-case-transition state-trans)
       (else
	,match)))

;*---------------------------------------------------------------------*/
;*    compile-cond-regular ...                                         */
;*---------------------------------------------------------------------*/
(define (compile-cond-regular current-state state-trans match)
   
   (define (compile-cond-transition state-trans prev-test-len)
      (let ((set   (cdr state-trans))
	    (state (car state-trans)))
	 (multiple-value-bind (cond-test cond-cost)
	    (compile-cond-test set 'cur prev-test-len)
	    (values `(,cond-test
			,(compile-jump-to-state state match
			    '(+fx 1 forward) 'bufpos))
	       cond-cost))))

   (let loop ((trans state-trans)
	      (tests '())
	      (cost 0)
	      (prev-len 0)
	      (elsep #f))
      (if (or (null? trans) elsep)
	  (begin
	     (if (>fx cost *case-threshold*)
		 (compile-case-regular current-state state-trans match)
		 `(cond
		     ,@(reverse! tests)
		     ,@(if elsep
			'()
			`((else ,match))))))
	  (multiple-value-bind (test c)
	     (compile-cond-transition (car trans) prev-len)
	     (loop (cdr trans)
		(cons test tests)
		(+fx c cost)
		(+fx prev-len (rgcset-length (cdr (car trans))))
		(and (pair? test) (eq? (car test) 'else)))))))

;*---------------------------------------------------------------------*/
;*    compile-cond-test ...                                            */
;*---------------------------------------------------------------------*/
(define (compile-cond-test set var prev-test-len)
   
   (define (find-next-member start set)
      (let ((max (rgc-max-char)))
	 (let loop ((i start))
	    (cond
	       ((=fx i max) -1)
	       ((rgcset-member? set i) i)
	       (else (loop (+fx i 1)))))))
   
   (define (find-next-non-member start set)
      (let ((max (rgc-max-char)))
	 (let loop ((i start))
	    (cond
	       ((=fx i max) max)
	       ((rgcset-member? set i) (loop (+fx i 1)))
	       (else i)))))
   
   (define (compile-test start stop set)
      (cond
	 ((=fx (-fx stop 1) start)
	  (values `(=fx ,var ,start) 1))
	 ((<fx (-fx stop start) 4)
	  (values `(or ,@(let loop ((start start)
				    (res   '()))
			    (if (=fx start stop)
				res
				(loop (+fx start 1)
				      (cons `(=fx ,var ,start) res)))))
		  (-fx stop start)))
	 ((and (=fx stop (rgc-max-char)) (=fx start 1))
	  (values #t 0))
	 ((=fx stop (rgc-max-char))
	  (values `(>=fx ,var ,start) 1))
	 ((=fx start 1)
	  (values `(<fx ,var ,stop) 1))
	 (else
	  (values `(and (>=fx ,var ,start) (<fx ,var ,stop)) 3))))
   
   (define (compile-range-test set)
      (let loop ((start (find-next-member 1 set))
		 (tests '())
		 (cost  0))
	 (if (=fx start -1)
	     (values `(or ,@(reverse! tests)) cost)
	     (let ((stop (find-next-non-member start set)))
		(multiple-value-bind (test c)
		   (compile-test start stop set)
		   (loop (find-next-member stop set)
			 (cons test tests)
			 (+fx c cost)))))))
   
   (let ((max (rgc-max-char))
	 (len (rgcset-length set)))
      (cond
	 ((=fx (rgc-max-char) (+fx len prev-test-len))
	  ;; this test, checks for the complementary test of
	  ;; what have been already tested. in consequence, we may
	  ;; here generate an ELSE test
	  (values 'else 0))
	 ((>fx len (+fx 2 (/fx max 2)))
	  ;; its better to compute the negation of the test
	  (multiple-value-bind (test cost)
	     (compile-range-test (rgcset-not set))
	     (values `(not ,test) (+fx 1 cost))))
	 (else
	  (compile-range-test set)))))
   
;*---------------------------------------------------------------------*/
;*    state-transition-list ...                                        */
;*---------------------------------------------------------------------*/
(define (state-transition-list transitions)
   (let loop ((transitions transitions)
	      (res '()))
      (if (null? transitions)
	  res
	  (let* ((transition (car transitions))
		 (char (car transition))
		 (state (cdr transition)))
	     (let ((cell (assq state res)))
		(if (not (pair? cell))
		    (let ((set (list->rgcset (list char) (rgc-max-char))))
		       (loop (cdr transitions)
			  (cons (cons state set) res)))
		    (begin
		       (rgcset-add! (cdr cell) char)
		       (loop (cdr transitions) res))))))))

;*---------------------------------------------------------------------*/
;*    insort ...                                                       */
;*    -------------------------------------------------------------    */
;*    Insert EL in the list LST preversing the sort in LST             */
;*---------------------------------------------------------------------*/
(define (insort el lst)
   (cond
      ((null? lst) (list el))
      ((<fx el (car lst)) (cons el lst))
      ((=fx el (car lst)) lst)
      (else (cons (car lst) (insort el (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    compile-match ...                                                */
;*    -------------------------------------------------------------    */
;*    This function scans all special transitions seeking for match    */
;*    transitions. Match transitions are sorted. If no context         */
;*    match is found, the smaller one is returned, otherwise a         */
;*    COND construction is inserted.                                   */
;*---------------------------------------------------------------------*/
(define (compile-match transitions)
   
   ;; we compile a match list into a body that, at runtime, will compute
   ;; we new matching rule. the MATCHES list is sorted.
   (define (compile-matches matches)
      (if (null? matches)
	  #f
	  ;; we loop over MATCHES until we find a non context match
	  (let loop ((matches matches))
	     (if (null? matches)
		 'last-match
		 (let* ((match (car matches))
			(preds (predicate-match match)))
		    (if (pair? preds)
			;; this rule is conditional
			`(if (and ,@preds)
			     (begin
				(rgc-stop-match! iport forward)
				,match)
			     ,(loop (cdr matches)))
			`(begin
			   (rgc-stop-match! iport forward)
			   ,match)))))))
   
   (let loop ((transitions transitions)
	      (matches '()))
      (if (null? transitions)
	  (compile-matches matches)
	  (let* ((transition (car transitions))
		 (char (car transition))
		 (state (cdr transition)))
	     (if (special-char-match? char)
		 (loop (cdr transitions)
		       (insort (special-match-char->rule-number char) matches))
		 (loop (cdr transitions)
		       matches))))))

;*---------------------------------------------------------------------*/
;*    compile-member-vector ...                                        */
;*---------------------------------------------------------------------*/
(define compile-member-vector #unspecified)

;*---------------------------------------------------------------------*/
;*    init-compile-member-vector! ...                                  */
;*---------------------------------------------------------------------*/
(define (init-compile-member-vector!)
   (if (not (vector? compile-member-vector))
       (set! compile-member-vector (make-vector (+ 1 (rgc-max-char))))))

;*---------------------------------------------------------------------*/
;*    reset-compile-member-vector! ...                                 */
;*---------------------------------------------------------------------*/
(define (reset-compile-member-vector!)
   (set! compile-member-vector #unspecified))

;*---------------------------------------------------------------------*/
;*    chars->char-ranges ...                                           */
;*    -------------------------------------------------------------    */
;*    This function takes a list of chars and return a list of         */
;*    ranges over these chars. For instance:                           */
;*      (65 66 67 70 71 72 73 74) -> ((65 . 67) (70 . 74))             */
;*---------------------------------------------------------------------*/
(define (chars->char-ranges chars)
   
   (let ((max (rgc-max-char)))
      
      (define (find-range-start i)
	 (let loop ((i i))
	    (cond
	       ((=fx i max) #f)
	       ((vector-ref compile-member-vector i) i)
	       (else (loop (+fx i 1))))))
      
      (define (find-range-stop i)
	 (let loop ((i i))
	    (cond
	       ((=fx i max) max)
	       ((vector-ref compile-member-vector i) (loop (+fx i 1)))
	       (else i))))
      
      (define (get-next-range i)
	 (let ((start (find-range-start i)))
	    (if (not start)
		max
		(cons start (-fx (find-range-stop start) 1)))))
      
      (vector-fill! compile-member-vector #f)
      (for-each (lambda (x) (vector-set! compile-member-vector x #t)) chars)
      (let loop ((i      0)
		 (ranges '()))
	 (if (>=fx i max)
	     (reverse! ranges)
	     (let ((range (get-next-range i)))
		(if (pair? range)
		    (loop (+fx (cdr range) 1) (cons range ranges))
		    (loop range ranges)))))))

;*---------------------------------------------------------------------*/
;*    char-ranges->test ...                                            */
;*---------------------------------------------------------------------*/
(define (char-ranges->test current ranges)
   
   (define (char-range->test range)
      (let ((start (car range))
	    (stop  (cdr range)))
	 (if (=fx start stop)
	     `(=fx ,current ,start)
	     `(and (>=fx ,current ,start) (<=fx ,current ,stop)))))
   
   `(or ,@(map char-range->test ranges)))

;*---------------------------------------------------------------------*/
;*    compile-member-test ...                                          */
;*    -------------------------------------------------------------    */
;*    This function emits the test is CURRENT in the CHARS list?       */
;*---------------------------------------------------------------------*/
(define (compile-member-test current chars)
   (match-case chars
      ((?char)
       `(=fx ,current ,char))
      (else
       (let ((ranges (chars->char-ranges chars)))
	  (if (>fx (length ranges) (/fx (length chars) 3))
	      `(memq ,current ',chars)
	      (char-ranges->test current ranges))))))
   
;*---------------------------------------------------------------------*/
;*    compile-submatches ...                                           */
;*---------------------------------------------------------------------*/
(define (compile-submatches current submatches positions positions-to-char)
   
   (define (find-same-submatch cell sm)
      (let loop ((sm sm))
	 (cond
	    ((null? sm)
	     '())
	    ((equal? cell (cdr (car sm)))
	     (car sm))
	    (else
	     (loop (cdr sm))))))
   
   (define (add-to-submatch! char cell)
      (set-car! cell (cons char (car cell)))
      cell)
   
   (let loop ((positions (rgcset->list positions))
	      (char-submatches '()))
      (if (null? positions)
	  (map (match-lambda
		  ((?char () (?m . ?sm))
		   `(if ,(compile-member-test current char)
			(rgc-submatch-stop2! ,m ,sm (+fx 1 forward))))
		  ((?char ((?s ?m ?sm)))
		   `(if ,(compile-member-test current char)
			,(if s
			     ;; this is a submatch at a
			     ;; nullable position
			     `(rgc-submatch-start*2! ,m ,sm (+fx 1 forward))
			     `(rgc-submatch-start2! ,m ,sm (+fx 1 forward)))))
		  ((?char ((?s ?m ?sm)) (?ts . ?tsm))
		   `(if ,(compile-member-test current char)
			(begin
			   ,(if s
				;; this is a submatch at a
				;; nullable position
				`(rgc-submatch-start*2! ,m ,sm (+fx 1 forward))
				`(rgc-submatch-start2! ,m ,sm (+fx 1 forward)))
			   (rgc-submatch-stop2! ,ts ,tsm (+fx 1 forward)))))
		  (?else
		   (error "compile-submatches"
			  "Illegal char description"
			  else)))
	       char-submatches)
	  (let* ((pos  (car positions))
		 (char (vector-ref positions-to-char pos))
		 (cell (vector-ref submatches pos)))
	     (if (null? cell)
		 (loop (cdr positions)
		       char-submatches)
		 (let ((old (find-same-submatch cell char-submatches)))
		    (if (pair? old)
			(begin
			   (add-to-submatch! char old)
			   (loop (cdr positions)
				 char-submatches))
			(loop (cdr positions)
			      (cons (cons (list char) cell)
				    char-submatches)))))))))



		   
