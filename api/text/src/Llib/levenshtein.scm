;*=====================================================================*/
;*    .../prgm/project/bigloo/api/text/src/Llib/levenshtein.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 12:07:40 2013                          */
;*    Last change :  Tue Jun  4 15:16:54 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Levenshtein string distance implementation                       */
;*=====================================================================*/

;;; Copyright @copyright{} 2004--2009 Neil Van Dyke.  This program is Free
;;; Software; you can redistribute it and/or modify it under the terms of the
;;; GNU Lesser General Public License as published by the Free Software
;;; Foundation; either version 3 of the License (LGPL 3), or (at your option)
;;; any later version.  This program is distributed in the hope that it will be
;;; useful, but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See
;;; @indicateurl{http://www.gnu.org/licenses/} for details.  For other licenses
;;; and consulting, please contact the author.


;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __text_levenshtein
   (export (levenshtein-string::long a::bstring b::bstring)
	   (levenshtein-list::long a::pair-nil b::pair-nil)
	   (levenshtein-vector::long a::vector b::vector)
	   (levenshtein::long ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    %identity ...                                                    */
;*---------------------------------------------------------------------*/
(define (%identity x)
   x)

;*---------------------------------------------------------------------*/
;*    %vector-empty? ...                                               */
;*---------------------------------------------------------------------*/
(define (%vector-empty? v)
   (zerofx? (vector-length v)))

;*---------------------------------------------------------------------*/
;*    %string->vector ...                                              */
;*---------------------------------------------------------------------*/
(define (%string->vector s)
   (let* ((l (string-length s))
	  (v (make-vector l)))
      (let loop ((i 0))
	 (if (=fx l i)
	     v
	     (begin
		(vector-set! v i (string-ref s i))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    levenshtein-vector/predicate ...                                 */
;*---------------------------------------------------------------------*/
(define (levenshtein-vector/predicate a::vector b::vector pred::procedure)
   (let ((a-len (vector-length a))
	 (b-len (vector-length b)))
      (cond
	 ((zerofx? a-len)
	  b-len)
	 ((zerofx? b-len)
	  a-len)
	 (else
	  (let ((w (make-vector (+fx 1 b-len)))
		(next #f))
	     (let fill ((k b-len))
		(vector-set-ur! w k k)
		(or (zerofx? k) (fill (- k 1))))
	     (let loop-i ((i 0))
		(if (=fx i a-len)
		    next
		    (let ((a-i (vector-ref-ur a i)))
		       (let loop-j ((j 0)
				    (cur (+fx 1 i)))
			  (if (=fx j b-len)
			      (begin
				 (vector-set-ur! w b-len next)
				 (loop-i (+fx 1 i)))
			      (begin
				 (set! next (minfx
					       (+fx 1 (vector-ref-ur w (+fx 1 j)))
					       (+fx 1 cur)
					       (if (pred a-i (vector-ref-ur b j))
						   (vector-ref-ur w j)
						   (+fx 1 (vector-ref-ur w j)))))
				 (vector-set-ur! w j cur)
				 (loop-j (+fx 1 j) next))))))))))))

(define (levenshtein-vector/eq a b)
  (levenshtein-vector/predicate a b eq?))
(define (levenshtein-vector/eqv a b)
  (levenshtein-vector/predicate a b eqv?))
(define (levenshtein-vector/equal a b)
  (levenshtein-vector/predicate a b equal?))

(define (levenshtein-vector a b)
   (levenshtein-vector/equal a b))

;*---------------------------------------------------------------------*/
;*    levenshtein-list/predicate ...                                   */
;*---------------------------------------------------------------------*/
(define (levenshtein-list/predicate a::pair-nil b::pair-nil pred)
   (cond
      ((null? a)
       (length b))
      ((null? b)
       (length a))
      (else
       (levenshtein-vector/predicate (list->vector a) (list->vector b) pred))))

(define (levenshtein-list/eq a b)
   (levenshtein-list/predicate a b eq?))

(define (levenshtein-list/eqv a b)
   (levenshtein-list/predicate a b eqv?))

(define (levenshtein-list/equal a b)
   (levenshtein-list/predicate a b equal?))

(define (levenshtein-list a b)
   (levenshtein-list/equal a b))

;*---------------------------------------------------------------------*/
;*    levenshtein-datatype ...                                         */
;*---------------------------------------------------------------------*/
(define (levenshtein-datatype a b pred a-emp a-len a-vec)
   
   (define (bar b-emp b-len b-vec)
      (if (b-emp b)
	  (a-len a)
	  (levenshtein-vector/predicate (a-vec a) (b-vec b) pred)))
   
   (cond
      ((vector? b)
       (bar %vector-empty? vector-length %identity))
      ((string? b)
       (bar string-null? string-length %string->vector))
      ((list? b)
       (bar null? length list->vector))
      (else
       (error "levenshtein" "term 2 must be vector, list, or string:" b))))

;*---------------------------------------------------------------------*/
;*    levenshtein/predicate ...                                        */
;*---------------------------------------------------------------------*/
(define (levenshtein/predicate a b pred)

   (define (%levenshtein-string/predicate a b pred)
      (cond
	 ((string-null? a)
	  (string-length b))
	 ((string-null? b)
	  (string-length a))
	 (else
	  (levenshtein-vector/predicate
	     (%string->vector a)
	     (%string->vector b)
	     pred))))
   
   (cond
      ((vector? a)
       (if (vector? b)
	   (levenshtein-vector/predicate a b pred)
	   (levenshtein-datatype a b pred
	      %vector-empty?
	      vector-length
	      %identity)))
      ((string? a)
       (if (string? b)
	   (%levenshtein-string/predicate a b pred)
	   (levenshtein-datatype a b pred
	      string-null? 
	      string-length
	      %string->vector)))
      ((list? a)
       (if (list? b)
	   (levenshtein-list/predicate a b pred)
	   (levenshtein-datatype a b pred null? length list->vector)))
      (else
       (error "levenshtein" "term 1 must be vector, list, or string:" a))))

;*---------------------------------------------------------------------*/
;*    levenshtein-string ...                                           */
;*---------------------------------------------------------------------*/
(define (levenshtein-string::long a::bstring b::bstring)
   (cond
      ((string-null? a)
       (string-length b))
      ((string-null? b)
       (string-length a))
      (else
       (levenshtein-vector/predicate
	  (%string->vector a)
	  (%string->vector b)
	  char=?))))

;*---------------------------------------------------------------------*/
;*    levenshtein ...                                                  */
;*---------------------------------------------------------------------*/
(define (levenshtein::long a::obj b::obj)
   (if (and (string? a) (string? b))
       (levenshtein-string a b)
       (levenshtein/predicate a b equal?)))

