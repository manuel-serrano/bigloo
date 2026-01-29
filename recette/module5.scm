;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/recette/module5.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 17 10:40:09 2025                          */
;*    Last change :  Thu Jan 29 10:46:11 2026 (serrano)                */
;*    Copyright   :  2025-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Module5 tests                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module5
   (import :version 4 "./utils.scm")
   (import "module5_ex0.scm")
   (import "module5_ex1.scm")
   (import "module5_ex1.scm" ex1a EX1b)
   (import "module5_ex2.scm" (EX2A EX2a))
   (import "module5_ex3.scm" ex3a)
   (import "module5_ex4.scm")
   (import "module5_ex7.scm")
   (import "module5_ex8.scm")
   (import "module5_ex9.scm" ex9ma (EX9MB ex9mb) ex9f)
   (import "module5_ex10.scm")
   (import "module5_ex11.scm")
   (import :version 4 "module5_ex12.scm")
   (import "module5_ex13.scm")
   (import "module5_ex14.scm")
   ;;(import "module5_ex15.scm")
   (import "module5_ex16.scm" (ex16a ex5a))
   (import "module5_ex17.scm" (ex17))
   (import "module5_ex18.scm")
   (export test-module5))

;*---------------------------------------------------------------------*/
;*    test.sch                                                         */
;*---------------------------------------------------------------------*/
(include "test.sch")

;*---------------------------------------------------------------------*/
;*    test-module5 ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-module5)
   (ex10i "from module5.scm")
   (test-module "module5" "module5.scm")
   (test "ex0.1" (typeof ex0a) "procedure")
   (test "ex0.2" (ex0a 4) 4)
   (test "ex0.3" (ex0b 1 2) (cons 1 2))
   (test "ex0.4" (ex0a 1 2) (list 1 2))
   (test "ex1.1" (typeof ex1a) "procedure")
   (test "ex1.2" (typeof EX1b) "procedure")
   (test "ex1.3" (EX1b 100 200) (cons* 'ex1b 100 200))
   (test "ex1.4" (ex1c 100 200) (cons* 'ex1c 100 200))
   (test "ex2.1" (typeof EX2A) "procedure")
   (test "ex2.2" (EX2A 100 200) (list 'ex2a 100 200))
   (test "ex3.1" (ex3a 200) (vector 'ex3a 200))
   (test "ex3.2" (ex3b 'a 'b 'c) (list 'ex3b 'a 'b 'c))
   (test "ex4.1" (EX4A 'abc) (vector 'ex3a 'abc))
   (test "ex8.1" (ex8m "from module5.scm") (cons "from module5.scm" 'ex8m))
   (test "ex8.2" (ex8f) (cons 124 'ex8m))
   (test "ex9.1" (ex9ma "from module5.scm") (cons "from module5.scm" 'ex9m))
   (test "ex9.2" (EX9MB "from module5.scm") (cons "from module5.scm" 'ex9mb))
   (test "ex9.3" (ex9f) (cons 340 'ex9m))
   (test "ex10.1" (ex10i "from module5.scm") (cons 'ex10m "from module5.scm"))
   (test "ex11.1" (ex11f) (list 10 20 "C2"))
   (test "ex12.1" (to-string (ex12 4)) "==> C12  4")
   (test "ex12.2" (to-string (ex12i 4 3 9)) "==> bint ")
   (test "ex13.1" G 45)
   (test "ex14.1" (ex14 '(1 2 3)) 2)
;*    (test "ex15.1" (ex15f) "procedure")                              */
   (test "ex16.1" (ex16a) 'ex5a)
   (test "ex17.1" (ex5a@ex17) 'ex5a)
   (test "ex17.2" (ex17a@ex17 1 2) (list 1 2))
   (test "ex18.1" (ex18) 'not-shadowed)
   )

;*---------------------------------------------------------------------*/
;*    ex18-shadow ...                                                  */
;*---------------------------------------------------------------------*/
(define (xex18-shadowx)
   'shadowed)
