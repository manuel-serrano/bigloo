;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/mmap.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep  4 07:43:45 2005                          */
;*    Last change :  Thu Nov  9 14:43:00 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MMAP testing                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mmap
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-mmap)))

;*---------------------------------------------------------------------*/
;*    test-mmap ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-mmap)
   (test-module "test-mmap" "mmap.scm")
   (let* ((path "misc/input.txt")
	  (mm (open-mmap path))
	  (c (with-input-from-file path (lambda () (read-char))))
	  (s (with-input-from-file path
		(lambda ()
		   (read-chars 20)
		   (read-chars 9)))))
      (test "mmap?.1" (mmap? mm) #t)
      (test "mmap?.2" (mmap? list) #f)
      (test "mmap?.3" (mmap? 1) #f)
      (test "mmap?.4" (mmap? "foo") #f)
      (test "mmap-length" (mmap-length mm) (file-size path))
      (test "mmap-read-position" (mmap-read-position mm) #e0)
      (test "mmap-write-position" (mmap-write-position mm) #e0)
      (test "mmap-ref.1" (mmap-ref mm 0) c)
      (mmap-set! mm 0 #\a)
      (test "mmap-set.1!" (mmap-ref mm 0) #\a)
      (mmap-set! mm 0 c)
      (test "mmap-set!.2" (mmap-ref mm 0) c)
      (test "mmap-read-position.2" (mmap-read-position mm) #e1)
      (test "mmap-write-position.2" (mmap-write-position mm) #e1)
      (test "mmap-substring" (mmap-substring mm 20 29) s)
      (test "mmap-read-position.3" (mmap-read-position mm) #e29)
      (mmap-substring-set! mm 20 "abcdefghi")
      (test "mmap-substring-set!" (mmap-substring mm 20 29) "abcdefghi")
      (mmap-substring-set! mm 20 s)
      (test "mmap-read-position.3" (mmap-read-position mm) #e29)
      (mmap-read-position-set! mm 20)
      (mmap-write-position-set! mm 21)
      (test "mmap-get-char.1" (mmap-get-char mm) (string-ref s 0))
      (test "mmap-read-position.4" (mmap-read-position mm) #e21)
      (mmap-put-char! mm (string-ref s 1))
      (test "mmap-put-char!.1" (mmap-get-char mm) (string-ref s 1))
      (test "mmap-write-position.4" (mmap-write-position mm) #e22)
      (mmap-read-position-set! mm #e20)
      (mmap-write-position-set! mm #e20)
      (test "mmap-get-string.1" (mmap-get-string mm 9) s)
      (mmap-put-string! mm s)
      (mmap-read-position-set! mm #e20)
      (test "mmap-put-string!.1" (mmap-get-string mm 9) s)
      (test "mmap-read-position.5" (mmap-read-position mm) #e29)
      (test "mmap-write-position.5" (mmap-write-position mm) #e29)
      (test "close-mmap" (close-mmap mm) #t)
       (let ((test-str (make-string 16 #a000))) (test "mmap->bstring"
                     (mmap->bstring (string->mmap test-str))
                     test-str))))
      
