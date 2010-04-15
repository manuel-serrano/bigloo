;*=====================================================================*/
;*    .../prgm/project/bigloo/api/crypto/src/Misc/make_lib.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*        adapted :  Florian Loitsch                                   */
;*    Copyright   :  2001-09 Manuel Serrano, Florian Loitsch           */
;*    -------------------------------------------------------------    */
;*    The module is used to build the heap file.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __crypto-makelib

   (import __crypto-block-ciphers
	   __crypto-ciphers

	   __crypto-string2key

	   __crypto-dsa
	   __crypto-rsa
	   __crypto-elgamal
	   __crypto-pem)

   (eval   (export-all)
	   (class Block-Cipher)
	   (class Dsa-Key)
	   (class Complete-Dsa-Key)
	   (class Rsa-Key)
	   (class Complete-Rsa-Key)
	   (class ElGamal-Key)
	   (class Complete-ElGamal-Key)
	   )

   (export (%crypto-eval)))

;*---------------------------------------------------------------------*/
;*    %crypto-eval ...                                                 */
;*---------------------------------------------------------------------*/
(define (%crypto-eval)
   #unspecified)
