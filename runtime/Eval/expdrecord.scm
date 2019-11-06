;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Eval/expdrecord.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Sven Hartrumpf                                    */
;*    Creation    :  Thu Mar 30 08:02:33 2000                          */
;*    Last change :  Sun Aug 25 09:15:44 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    SRFI-9 Record expansion.                                         */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/struct.texi@                              */
;*       @node Records (SRFI-9)@                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __expander_record
   
   (import  __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __param
	    __object
	    __thread
	    
	    __match_normalize
	     
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
	    
	    __progn)
   
   (use     __type
	    __evenv
	    __bit)
   
   (export  (expand-define-record-type x e)))

;*---------------------------------------------------------------------*/
;*    expand-define-record-type ...                                    */
;*---------------------------------------------------------------------*/
(define (expand-define-record-type x e)
   (match-case x
      ((?- ?name ?constr ?predicate . ?fields)
       (evepairify (e (record->struct name constr predicate fields) e) x))
      (else
       (expand-error #f "invalid form" x))))

;*---------------------------------------------------------------------*/
;*    record->struct ...                                               */
;*---------------------------------------------------------------------*/
(define (record->struct name constr predicate fields)
   (cond
      ((not (list? fields))
       (expand-error #f "invalid fields" fields))
      ((not (list? constr))
       (expand-error #f "invalid constructor" constr))
      (else
       (let ((tmp (gensym))
	     (val (gensym))
	     (key (symbol-append '< name '>)))
	  `(begin
	      (define ,constr
		 (let ((,tmp (make-struct ',key ,(length fields) #unspecified)))
		    (begin
		       ,@(let loop ((fields fields)
				    (i 0))
			    (cond
			       ((null? fields)
				'())
			       ((memq (caar fields) (cdr constr))
				(cons `(struct-set! ,tmp ,i ,(caar fields))
				      (loop (cdr fields) (+ i 1))))
			       (else
				(loop (cdr fields) (+ i 1)))))
		       ,tmp)))
	      (define (,predicate o)
		 (and (struct? o)
		      (eq? (struct-key o) ',key)
		      (= (struct-length o) ,(length fields))))
	      ,@(let loop ((fields fields)
			   (i 0))
		   (if (null? fields)
		       '()
		       (let ((field (car fields)))
			  (cond
			     ((= (length field) 2)
			      (cons `(define (,(cadr field) ,tmp)
					(struct-ref ,tmp ,i))
				    (loop (cdr fields) (+ i 1))))
			     ((= (length field) 3)
			      (cons `(begin
					(define (,(cadr field) ,tmp)
					   (struct-ref ,tmp ,i))
					(define (,(caddr field) ,tmp ,val)
					   (struct-set! ,tmp ,i ,val)))
				    (loop (cdr fields) (+ i 1))))
			     (else
			      (expand-error #f
					    "invalid field spec"
					    field)))))))))))

;*---------------------------------------------------------------------*/
;*    expand-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-error p m x)
   (let ((loc (when (epair? x) (cer x))))
      (if (and (pair? loc) (pair? (cdr loc)) (pair? (cddr loc)))
	  (error/location p m x (cadr loc) (caddr loc))
	  (error p m x))))

