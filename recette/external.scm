;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/external.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 25 11:52:25 2000                          */
;*    Last change :  Thu Sep 18 11:03:34 2008 (serrano)                */
;*    Copyright   :  2000-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Jvm foreign interface tests                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module external
   (include "test.sch") 
   (import  (main "main.scm"))
   (java (abstract-class intf
            (method met::int (::intf) "abstract_method")
            "Intf")
	 (class point::intf
            (constructor new-default ())
            (field x::int "x")
	    (method met::int (::point) "abstract_method")
            (method show::void (::point) "show")
            (method static statistics::int () "PointStatistics")
            "Point")
         (class point::intf
            (field y::int "y")
            (field static num::int "point_num")
            (constructor new (::int ::int)))
	 (array int* ::int)
	 (class arraytest
	    (constructor new (::int*))
	    (method static hello::int (::int*) "hello")
	    "ArrayTest")
         (export callback "callback"))
   (export (callback::int ::int))
   (export (test-external)))

;*---------------------------------------------------------------------*/
;*    test-jvm-class ...                                               */
;*---------------------------------------------------------------------*/
(define (test-jvm-class)
   (let loop ((num 10)
	      (val 0))
      (if (<fx num 0)
          (let ((pt (point-new-default)))
	     (+ val (point-statistics)))
          (let ((pt (point-new num num)))
             (loop (-fx num 1)
		   (+ val (point-x pt) (point-y pt)))))))

;*---------------------------------------------------------------------*/
;*    test-jvm-array ...                                               */
;*---------------------------------------------------------------------*/
(define (test-jvm-array)
   (let ((tab (make-int* 2)))
      (int*-set! tab 0 3)
      (int*-set! tab 1 6)
      (when (arraytest? (arraytest-new tab))
	 (arraytest-hello tab))))

;*---------------------------------------------------------------------*/
;*    callback ...                                                     */
;*---------------------------------------------------------------------*/
(define (callback x)
   (+ 1 x))

;*---------------------------------------------------------------------*/
;*    test-external ...                                                */
;*---------------------------------------------------------------------*/
(define (test-external)
   (test-module "external" "external.scm")
   (test "Java class" (test-jvm-class) 133)
   (test "Java array" (test-jvm-array) 11)
   (test "Java eq?" (foreign-eq? (point-new-default) (point-new-default)) #f)
   (test "Java eq?" (let ((o (point-new-default)))
		       (foreign-eq? o o))
	 #t)
   (test "Java static" point-num 15)
   (test "java foreign?" (foreign? (point-new-default)) #t)
   (test "Java null" (foreign-null? (point-new-default)) #f)
   (test "invoke interface" (intf-met (point-new-default)) 1)
   (test "invoke virtual" (point-met (point-new-default)) 1))
