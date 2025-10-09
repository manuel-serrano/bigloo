;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/recette/crypto.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May  4 08:30:19 2009                          */
;*    Last change :  Thu Oct  9 15:03:41 2025 (serrano)                */
;*    Copyright   :  2009-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Testing cryptography                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module crypto
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-crypto)))

;*---------------------------------------------------------------------*/
;*    test-crypto ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-crypto)
   (test-module "crypto" "crypto.scm")
   #;(let* ((key (make-rsa-key-pair))
	  (privk (private-rsa-key key))
	  (pubk (public-rsa-key key))
	  (str "foo bar gee"))
      (test "rsa.encrypt" (string? (rsa-encrypt-string str pubk)) #t)
      (test "rsa.decrypt" (rsa-decrypt-string (rsa-encrypt-string str pubk) privk) str)))
