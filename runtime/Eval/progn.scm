;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/progn.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 10:07:31 1994                          */
;*    Last change :  Sun Aug 25 09:15:02 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    La normalisation des formes `begin'                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __progn
   
   (import  __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __param
	    __expand
	    __object
	    __thread
	    
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
	    __r4_output_6_10_3)

   (use     __type
	    __evenv
	    __bit)
   
   (export  (evepairify::obj ::obj ::obj)
	    (evepairify*::obj ::obj ::obj)
	    (evepairify-deep::obj ::obj ::obj)
	    (expand-progn ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    make-epair ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-epair a::obj b::obj e::obj)
   (cond
      ((epair? e)
       (econs a b (cer e)))
      ((epair? b)
       (econs a b (cer b)))
      ((epair? a)
       (econs a b (cer a)))
      (else
       (cons a b))))

;*---------------------------------------------------------------------*/
;*    evepairify ...                                                   */
;*    -------------------------------------------------------------    */
;*    If the struct definition was an extended pair (that is if we     */
;*    were tracking the source location of the structure), we          */
;*    propagate inside the generated function, the define-struct       */
;*    location.                                                        */
;*---------------------------------------------------------------------*/
(define (evepairify pair epair)
   (cond
      ((not (epair? epair))
       pair)
      ((epair? pair)
       pair)
      ((not (pair? pair))
       pair)
      (else
       (econs (car pair) (cdr pair) (cer epair)))))

;*---------------------------------------------------------------------*/
;*    evepairify* ...                                                  */
;*    -------------------------------------------------------------    */
;*    If the struct definition was an extended pair (that is if we     */
;*    were tracking the source location of the structure), we          */
;*    propagate inside the generated function, the define-struct       */
;*    location.                                                        */
;*---------------------------------------------------------------------*/
(define (evepairify* pair epair)
   (if (not (epair? epair))
       pair
       (let ((e (cer epair)))
	  (let loop ((obj pair))
	     (cond
		((not (pair? obj))
		 obj)
		((epair? obj)
		 obj)
		(else
		 (econs (loop (car obj)) (loop (cdr obj)) e)))))))

;*---------------------------------------------------------------------*/
;*    evepairify-deep ...                                              */
;*---------------------------------------------------------------------*/
(define (evepairify-deep pair epair)
   (cond
      ((not (epair? epair))
       pair)
      ((not (pair? pair))
       pair)
      ((epair? pair)
       pair)
      (else
       (econs (evepairify-deep (car pair) (car epair))
	      (evepairify-deep (cdr pair) (cdr epair))
	      (cer epair)))))

;*---------------------------------------------------------------------*/
;*    expand-progn ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function builds a single expression from a list             */
;*    of expressions.                                                  */
;*---------------------------------------------------------------------*/
(define (expand-progn exps::pair-nil)
   (define (flatten-sequence exps)
      (let loop ((es exps))
	 (cond
	    ((or (null? es) (null? (cdr es)))
	     es)
	    ((not (pair? es))
	     (error "begin" "Illegal form" exps))
	    ((not (pair? (car es)))
	     (loop (cdr es)))
	    ((eq? (caar es) 'begin)
	     (loop (evepairify (append (cdar es) (loop (cdr es))) (car es))))
	    (else
	     (make-epair (car es) (loop (cdr es)) es)))))
   (if (null? exps)
       #unspecified
       (if (null? (cdr exps))
	   (car exps)
	   (let ((es (flatten-sequence exps)))
	      (cond
		 ((null? es)
		  #unspecified)
		 ((not (pair? es))
		  es)
		 ((null? (cdr es))
		  (car es))
		 (else
		  (make-epair 'begin es es)))))))

;*---------------------------------------------------------------------*/
;*    normalize-progn ...                                              */
;*    sexp --> sexp                                                    */
;*    -------------------------------------------------------------    */
;*    Cette fonction doit etre utilisee pour normalise du code         */
;*    utilisateur tel qu'il est lu par le lecteur.                     */
;*---------------------------------------------------------------------*/
(define (normalize-progn body)
   (cond
      ((not (pair? body))
       `(begin ,body))
      ((and (pair? body) (null? (cdr body)))
       (car body))
      (else
       (let ((res `(begin ,@(let loop ((body (if (eq? (car body) 'begin)
						 (cdr body)
						 body)))
			       (if (null? body)
				   '()
				   (let ((expr (car body)))
				      (if (and (pair? expr)
					       (eq? (car expr) 'begin))
					  (append (cdr expr) (loop (cdr body)))
					  (cons expr (loop (cdr body))))))))))
	  (cond
	     ((epair? body)
	      (evepairify res body))
	     ((epair? (car body))
	      (econs (car res) (cdr res) (cer (car body))))
	     (else
	      res)))))) 


