;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/recette/module.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan 29 10:56:02 1995                          */
;*    Last change :  Thu Oct  9 17:03:48 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Les tests d'exportations                                         */
;*=====================================================================*/

(module module
   (include "test.sch")
   (import  (x import1 "import1.scm")
	    (y z import1 "import1.scm")
	    (import2 "import2.scm")
	    ((renamed-test1 import-test1) import1 "import1.scm")
	    ((renamed-test2 import-test2) import2 "import2.scm")
	    (utils "utils.scm"))
   (export  fun
	    (test-modulel)
	    c-dummy1
	    c-dummy2))

;*---------------------------------------------------------------------*/
;*    Le bug signale par David Gurr                                    */
;*---------------------------------------------------------------------*/
(define (foo x)
   x)

(define fun foo)

;*---------------------------------------------------------------------*/
;*    Bug pointed out by David Fox                                     */
;*---------------------------------------------------------------------*/
(define c-dummy1 #f)
(define c-dummy2 #f)

;*---------------------------------------------------------------------*/
;*    Le pbm de l'init avec le lecteur                                 */
;*---------------------------------------------------------------------*/
(define read 'bogus)
(set! read "re-bogus")

(define (init/read-test)
   (cons 'init '(1 2 3 4)))
 
;*---------------------------------------------------------------------*/
;*    test-modulel ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-modulel)
   (test-module "module" "module.scm")
   (test "@" (@ x import1) 1)
   (test "@" (@ x import2) 2)
   (test "@" (or (eq? x (@ x import1)) (eq? x (@ x import2))) #t)
   (test "inline/type" (z) 1)
   (test "init/read" (init/read-test) '(init . (1 2 3 4)))
   (test "rename.1" (renamed-test1) 'import-test1)
   (test "rename.2" (renamed-test2) 'import-test2))
