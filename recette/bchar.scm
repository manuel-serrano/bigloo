;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/wasm/recette/bchar.scm               */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec  1 09:29:00 1992                          */
;*    Last change :  Thu Oct  9 15:09:03 2025 (serrano)                */
;*                                                                     */
;*    On test les caracteres                                           */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module bchar
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-char)))

;*---------------------------------------------------------------------*/
;*    test-char ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-char)
   (test-module "char" "char.scm")
   (test "char?.1" (char? #\a) #t)
   (test "char?.2" (char? 1) #f)
   (test "char=?.1" (let ((x #\a)) (char=? x #\a)) #t)
   (test "char=?.2" (let ((x #\b)) (char=? x #\a)) #f)
   (test "char<?.1" (let ((x #\b)) (char<? x #\a)) #f)
   (test "char<?.2" (let ((x #\b)) (char<? #\a x)) #t)
   (test "char>?.1" (let ((x #\b)) (char>? x #\a)) #t)
   (test "char>?.2" (let ((x #\b)) (char>? #\a x)) #f)
   (let ((s "été"))
      (test "char>?.3" (char>? (string-ref s 0) #a127) #t))
   (test "char-ci=?.1" (let ((x #\A)) (char-ci=? x #\a)) #t)
   (test "char-ci=?.2" (let ((x #\B)) (char-ci=? x #\a)) #f)
   (test "char-ci<?.1" (let ((x #\B)) (char-ci<? x #\a)) #f)
   (test "char-ci<?.2" (let ((x #\B)) (char-ci<? #\a x)) #t)
   (test "char-ci>?.1" (let ((x #\B)) (char-ci>? x #\a)) #t)
   (test "char-ci>?.2" (let ((x #\B)) (char-ci>? #\a x)) #f)
   (test "char-alphabetic?.1" (char-alphabetic? #\a) #t)
   (test "char-alphabetic?.2" (char-alphabetic? #\0) #f)
   (test "char-numeric?.1" (char-numeric? #\a) #f)
   (test "char-numeric?.2" (char-numeric? #\0) #t)
   (test "char-whitespace?.1" (char-whitespace? #\a) #f)
   (test "char-whitespace?.2" (char-whitespace? #\space) #t)
   (test "char-upper-case?.1" (char-upper-case? #\A) #t)
   (test "char-upper-case?.2" (char-upper-case? #\a) #f)
   (test "char-lower-case?.1" (char-lower-case? #\A) #f)
   (test "char-lower-case?.2" (char-lower-case? #\a) #t)
   (test "char->integer.1" (char->integer #\0) 48)
   (test "char->integer.2" (char->integer #a200) 200)
   (test "integer->char" (integer->char 48) #\0)
   (test "char-upcase.1" (char-upcase #\a) #\A)
   (test "char-upcase.2" (char-upcase #\A) #\A)
   (test "char-downcase.1" (char-downcase #\a) #\a)
   (test "char-downcase.2" (char-downcase #\A) #\a)
   (test "unsigned char" (char->integer (integer->char 128)) 128))
			    
   
