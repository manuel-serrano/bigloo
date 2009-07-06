;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Jclass/main.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 26 10:22:02 2000                          */
;*    Last change :  Mon Oct 30 17:52:40 2006 (serrano)                */
;*    Copyright   :  2000-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    An example of Java connection.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module main
   (java (abstract-class intf
	    (method met::int (::intf) "abstract_method")
	    "Intf")
	 (class point::intf
	    (constructor new ())
	    (field x::int "x")
	    (method met::int (::point) "abstract_method")
	    (method show::void (::point) "show")
	    (method static statistics::int () "PointStatistics")
	    "Point")
	 (class point::intf
	    (field y::int "y")
	    (field static num::int "point_num")
	    (constructor new/int (::int ::int)))
	 (export callback "callback"))
   (export (callback::int ::int))
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let loop ((num (if (null? (cdr argv))
		       10
		       (string->integer (cadr argv)))))
      (if (<fx num 0)
	  (let ((pt (point-new)))
	     (print "A interface method: " (intf-met pt))
	     (print "A class method: " (point-met pt))
	     (point-show pt)
	     (newline)
	     (print "Number of allocated points: " (point-statistics))
	     (print "point-num: " point-num))
	  (let ((pt (point-new/int num num)))
	     (point-show pt)
	     (print "  <-->  Point: " pt " x:" (point-x pt) " y:" (point-y pt))
	     (newline)
	     (loop (-fx num 1))))))

;*---------------------------------------------------------------------*/
;*    callback ...                                                     */
;*---------------------------------------------------------------------*/
(define (callback x)
   (+ 1 x))
