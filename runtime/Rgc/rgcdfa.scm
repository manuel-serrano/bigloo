;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Rgc/rgcdfa.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 11 16:36:03 1998                          */
;*    Last change :  Tue Apr 17 07:40:09 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    From the set of followpos (see the tree construction) and the    */
;*    association position x character, we build the DFA.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc_dfa
    
   (include "Rgc/rgc-node.sch")
   
   (import  __rgc_config
	    __rgc_set
	    __rgc_rules
	    __error)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __hash
	    __bit
	    __param
	    __object
	    __thread
	    __bexit
	    __bignum
	    __rgc
	    
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
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3)
   
   (export  (node->dfa node ::vector ::vector)
	    (print-dfa dfa)
	    (state-name <state>)
	    (state-transitions <state>)
	    (state-positions <state>)
	    (state?::bool obj)
	    (get-initial-state)
	    (reset-dfa!)))

;*---------------------------------------------------------------------*/
;*    bucket-len ...                                                   */
;*---------------------------------------------------------------------*/
(define bucket-len 64)

;*---------------------------------------------------------------------*/
;*    node->dfa ...                                                    */
;*---------------------------------------------------------------------*/
(define (node->dfa root followpos positions)
   ;; we setup the state initialization
   (init-states!)
   ;; we iterate as long as we have unmarked state we implement the
   ;; algorithm of the dragon book, pae 141:
   ;; initially, the only unmarked state in Dstates is firstpos(root),
   ;;   where root is the root of the syntax tree for (r)#;
   (let ((initial-state (make-state (node-firstpos root)))
	 (positions-num (vector-length positions)))
      (set! *initial-state* initial-state)
      (let loop ((d-states (list initial-state)))
;* 	 (print "d-states: " (map __state-number d-states))            */
	 (if (null? d-states)
	     (states-list)
	     ;; there is an unmarked state T in d-states
	     ;;    mark T
	     (let* ((new-d-states (cdr d-states))
		    (t            (car d-states))
		    (t-positions  (state-positions t)))
;* 		(print "  t: "                                         */
;* 		       (__state-number t) " "                          */
;* 		       (rgcset->list t-positions))                     */
		;; for each input symbol a
		(for-each-rgcset
		 (lambda (a)
		    (let ((u        (make-rgcset positions-num))
			  (u-empty? #t))
		       (for-each-rgcset
			(lambda (p)
			   (if (=fx a (vector-ref positions p))
			       (begin
				  (set! u-empty? #f)
				  (rgcset-or! u (vector-ref followpos p)))))
			t-positions)
;* 		       (print "   symbol: " a " U: " (rgcset->list u)) */
		       ;; if U is not empty and is not in Dstates
		       (if (not u-empty?)
			   (let ((us (let ((ostate
					    (hashtable-get *states* u)))
					(if (not (state? ostate))
					    (let ((new-state (make-state u)))
					       ;; the add U as an unmarked
					       ;; state to Dstates
					       (set! new-d-states
						     (cons new-state
							   new-d-states))
					       new-state)
					    ostate))))
			      ;; Dtran[T,a] := U
			      (state-add-transition! t a us)))))
		 (input-symbol-set t-positions positions))
		;; we are now ready for another iteration
		(loop new-d-states))))))

;*---------------------------------------------------------------------*/
;*    reset-dfa! ...                                                   */
;*---------------------------------------------------------------------*/
(define (reset-dfa!)
   (set! *initial-state* #unspecified)
   (set! *states* #unspecified)
   (set! *state-num* #unspecified))
   
;*---------------------------------------------------------------------*/
;*    *initial-state* ...                                              */
;*---------------------------------------------------------------------*/
(define *initial-state* #unspecified)

;*---------------------------------------------------------------------*/
;*    get-initial-state ...                                            */
;*---------------------------------------------------------------------*/
(define (get-initial-state)
   *initial-state*)

;*---------------------------------------------------------------------*/
;*    __state ...                                                      */
;*    -------------------------------------------------------------    */
;*    The structure that implements states                             */
;*---------------------------------------------------------------------*/
(define-struct __state name number transitions position-set)

;*---------------------------------------------------------------------*/
;*    *states* ...                                                     */
;*---------------------------------------------------------------------*/
(define *states* #unspecified)

;*---------------------------------------------------------------------*/
;*    states-list ...                                                  */
;*---------------------------------------------------------------------*/
(define (states-list)
   (hashtable->list *states*))

;*---------------------------------------------------------------------*/
;*    init-states! ...                                                 */
;*---------------------------------------------------------------------*/
(define (init-states!)
   (set! *state-num* -1)
   (set! *states* (make-hashtable 1024 bucket-len rgcset-equal? rgcset->hash)))

;*---------------------------------------------------------------------*/
;*    *state-num* ...                                                  */
;*---------------------------------------------------------------------*/
(define *state-num* #unspecified)

;*---------------------------------------------------------------------*/
;*    get-new-state-num ...                                            */
;*---------------------------------------------------------------------*/
(define (get-new-state-num)
   (set! *state-num* (+fx *state-num* 1))
   *state-num*)

;*---------------------------------------------------------------------*/
;*    make-state ...                                                   */
;*    -------------------------------------------------------------    */
;*    A state is created from a position set. State are created        */
;*    with an empty transition set.                                    */
;*---------------------------------------------------------------------*/
(define (make-state positions) 
   (let* ((num   (get-new-state-num))
	  (name  (gensym (string-append "STATE-" (number->string num) "-")))
	  (state (__state name num '() positions)))
;*       (print "*** make-state(" num "): " (rgcset->list positions))  */
      (hashtable-put! *states* (__state-position-set state) state)
      state))
   
;*---------------------------------------------------------------------*/
;*    state ...                                                        */
;*    -------------------------------------------------------------    */
;*    This function fetches a state from a number.                     */
;*---------------------------------------------------------------------*/
(define (state num)
   (vector-ref *states* num))

;*---------------------------------------------------------------------*/
;*    state-positions ...                                              */
;*---------------------------------------------------------------------*/
(define (state-positions state)
   (__state-position-set state))

;*---------------------------------------------------------------------*/
;*    state-name ...                                                   */
;*---------------------------------------------------------------------*/
(define (state-name state)
   (__state-name state))
		    
;*---------------------------------------------------------------------*/
;*    state-number ...                                                 */
;*---------------------------------------------------------------------*/
(define (state-number state)
   (__state-number state))

;*---------------------------------------------------------------------*/
;*    state-transitions ...                                            */
;*---------------------------------------------------------------------*/
(define (state-transitions state)
   (__state-transitions state))

;*---------------------------------------------------------------------*/
;*    state? ...                                                       */
;*---------------------------------------------------------------------*/
(define (state? obj)
   (__state? obj))

;*---------------------------------------------------------------------*/
;*    state-add-transition! ...                                        */
;*---------------------------------------------------------------------*/
(define (state-add-transition! from char to)
;*    [assert (char to) (and (fixnum? char) (__state? to))]            */
   (__state-transitions-set! from
			     (cons (cons char to) (__state-transitions from))))
 
;*---------------------------------------------------------------------*/
;*    input-symbol-set ...                                             */
;*---------------------------------------------------------------------*/
(define (input-symbol-set position-set positions)
   (let ((symbol-set (make-rgcset (tree-max-char))))
      (for-each-rgcset (lambda (i)
			  (rgcset-add! symbol-set (vector-ref positions i)))
		       position-set)
      symbol-set))

;*---------------------------------------------------------------------*/
;*    print-dfa ...                                                    */
;*---------------------------------------------------------------------*/
(define (print-dfa dfa)
   (print "========= DFA ====================================")
   (for-each (lambda (state)
		(print "state: " (__state-number state))
		'(for-each (lambda (trans)
			     (print "   " (car trans)
				    " [" 
				    (if (and (< (car trans) 256)
					(let ((c (integer->char (car trans))))
					   (if (or (char-alphabetic? c)
						   (char-numeric? c))
					       c
					       "ascii")))
					"special")
				    "]"
				    "  -->  "
				    (__state-number (cdr trans))))
			  (__state-transitions state)))
	     dfa)
   (print "==================================================")
   (newline))

