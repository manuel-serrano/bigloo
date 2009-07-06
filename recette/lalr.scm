;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/lalr.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 15 12:20:33 2002                          */
;*    Last change :  Fri Mar 15 12:27:34 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Lalr grammar tests                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module lalr
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-lalr)))

(define rg
   (regular-grammar ((number (posix "[0-9]+"))
		     (whitespace (+ (in "\r\n\t \v\f"))))
      (number  (cons 'number (the-fixnum)))
      (whitespace (ignore))
      (#\+     'op-add)
      (#\-     'op-sub)
      (#\*     'op-mult)
      (#\/     'op-div)
      (#\(     'op-lparen)
      (#\)     'op-rparen)
      (#\;     'op-semicolon)))

(define lalr-plain
   (lalr-grammar
      (op-mult op-div
       op-add op-sub
       op-lparen op-rparen
       op-semicolon
       number)
      
      (file
       (())
       ((file stmt) stmt))
      (stmt
       ((expr op-semicolon) expr))
      (expr
       ((number) number)
       ((expr@a op-add expr@b) (+ a b))
       ((expr@a op-sub expr@b) (- a b))
       ((expr@a op-mult expr@b) (* a b))
       ((expr@a op-div expr@b) (/ a b))
       ((op-lparen expr op-rparen) expr))))

(define lalr-assoc
   (lalr-grammar
      ((left: op-mult op-div)
       (left: op-add op-sub)
       op-lparen op-rparen
       op-semicolon
       number)
      
      (file
       (())
       ((file stmt) stmt))
      (stmt
       ((expr op-semicolon) expr))
      (expr
       ((number) number)
       ((expr@a op-add expr@b) (+ a b))
       ((expr@a op-sub expr@b) (- a b))
       ((expr@a op-mult expr@b) (* a b))
       ((expr@a op-div expr@b) (/ a b))
       ((op-lparen expr op-rparen) expr))))



;*---------------------------------------------------------------------*/
;*    test-lalr ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-lalr)
   (test-module "lalr" "lalr.scm")
   (test "plain"
	 (with-input-from-string "3*2+5-2*5;"
	    (lambda ()
	       (read/lalrp lalr-plain rg (current-input-port))))
	 -9)
   (test "assoc"
	 (with-input-from-string "3*2+5-2*5;"
	    (lambda ()
	       (read/lalrp lalr-assoc rg (current-input-port))))
	 1))
