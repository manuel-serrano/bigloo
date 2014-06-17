;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/srfi4.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:39:09 1992                          */
;*    Last change :  Tue Jun 17 19:45:14 2014 (serrano)                */
;*                                                                     */
;*    On test les operations primitives sur les vecteurs               */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module srfi4
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-srfi4)))

;*---------------------------------------------------------------------*/
;*    test-hvector ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (test-hvector id val)
   (let ((s? (symbol-append id 'vector?))
	 (tname (symbol->string (symbol-append id 'vector)))
	 (mk (symbol-append 'make- id 'vector))
	 (mnk (if (eq? id 's8)
		  (symbol-append 'make-u8vector)
		  (symbol-append 'make-s8vector)))
	 (sref (symbol-append id 'vector-ref))
	 (list->vec (symbol-append 'list-> id 'vector))
	 (vec->list (symbol-append id 'vector->list)))
      `(begin
	  (test ,(format "~avector.1" id) (,s? 1) #f)
	  (test ,(format "~avector.2" id) (,s? '1) #f)
	  (test ,(format "~avector.3" id) (,s? '()) #f)
	  (test ,(format "~avector.4" id) (,s? '#()) #f)
	  (test ,(format "~avector.5" id) (,s? '#(1)) #f)
	  (test ,(format "~avector.6" id) (,sref (,mk 3 ,val) 1) ,val)
	  (test ,(format "~avector.7" id) (,s? (,mk 1)) #t)
	  (test ,(format "~avector.8" id) (,s? (,mnk 1)) #f)
	  (let* ((o (,mk 10))
		 (s (with-output-to-string (lambda () (write o))))
		 (o2 (with-input-from-string s (lambda () (read)))))
	     (test ,(format "~avector.9" id) (find-runtime-type o2) ,tname)
	     (let ((s2 (obj->string o)))
		(test ,(format "~avector.10" id) (,s? o2) #t)
		(test ,(format "~avector.11" id) o2 o)
		(let ((o3 (string->obj s2)))
		   (test ,(format "~avector.12" id) (find-runtime-type o3) ,tname)
		   (test ,(format "~avector.13" id) (,s? o3) #t)
		   (test ,(format "~avector.14" id) o3 o)
		   (let ((o4 (,list->vec (,vec->list o))))
		      (test ,(format "~avector.15" id) o4 o)))))
	  (let* ((o (,mk 10 ,val))
		 (s (with-output-to-string (lambda () (write o))))
		 (o2 (with-input-from-string s (lambda () (read))))
		 (s2 (obj->string o))
		 (o3 (string->obj s2))
		 (l (,vec->list o))
		 (o4 (,list->vec l)))
	     (test ,(format "~avector.16" id) (,s? o2) #t)
	     (test ,(format "~avector.17" id) o2 o)
	     (test ,(format "~avector.18" id) (,s? o3) #t)
	     (test ,(format "~avector.19" id) o3 o)
	     (test ,(format "~avector.20" id) o4 o)
	     (test ,(format "~avector.21" id) (apply + l) (* 10 ,val))))))
 
;*---------------------------------------------------------------------*/
;*    test-srfi4 ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-srfi4)
   (test-module "srfi4" "srfi4.scm")
   (test "cond-expand" (cond-expand
			  (srfi-4 20)
			  (else 21))
	 20)
   (test "cond-expand" (eval '(cond-expand
				 (srfi-4 30)
				 (else 31)))
	 30)
   (test "cnst.1" (s8vector-ref '#s8(111 2) 1) 2)
   (test "cnst.2" (s8vector-ref '#s8(1 -2) 1) -2)
   (test "cnst.3" (u8vector-ref '#u8(1 2) 1) 2)
   (test "cnst.4" (s16vector-ref '#s16(1 2) 1) 2)
   (test "cnst.5" (u16vector-ref '#u16(1 2) 1) 2)
   (test "cnst.6" (s32vector-ref '#s32(1 2) 1) 2)
   (test "cnst.7" (u32vector-ref '#u32(1 2) 1) 2)
   (test "cnst.8" (s64vector-ref '#s64(5 2) 1) #s64:2)
   (test "cnst.9" (u64vector-ref '#u64(6 2) 1) #u64:2)
   (test "cnst.10" (f32vector-ref '#f32(1. 2.) 1) 2.)
   (test "cnst.11" (f64vector-ref '#f64(1. 2.) 1) 2.)
   (test "vector?.1" (vector? (make-s8vector 2)) #f)
   (test "vector?.2" (s8vector? (make-vector 2)) #f)
   (test-hvector s8 (fixnum->int8 1))
   (test-hvector u8 (fixnum->uint8 2))
   (test-hvector s16 -3)
   (test-hvector u16 4)
   (test-hvector s32 5)
   (test-hvector u32 6)
   (test-hvector s64 #s64:-7)
   (test-hvector u64 #u64:8)
   (test-hvector f32 -1.0)
   (test-hvector f64 1.0)
   )
