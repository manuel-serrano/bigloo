(module bps
   (import (main "main.scm"))
   (include "test.sch")
   (export (test-bps)) )

(define *maybe* #f)
(define (reset)
   (let ( (n (cons 2 3)) )
      (set! *maybe* (eq? (cddr (cons n n)) (cdr n))) ))

(define (maybe n)
   (if *maybe* n 'never) )

;;
;; string-as-read
;;
(define (ecs s)
   (let* ((ss (string-as-read s))
	  (n (string-length ss)))
      (if (=fx n 1)
	  (char->integer (string-ref ss 0))
	  (if (=fx n 0)
	      -1000
	      (- n)))))

;;
;; All-char-in-string
;;
(define *all-chars-in-string*
   (let ( (r (make-string 256)) )
      (let loop ( (i 0) )
	 (if (=fx i 256)
	     r
	     (begin (string-set! r i (integer->char i))
		    (loop (+fx i 1)) )))))

(define *all-chars-in-for-read-string*
   "\\000\\001\\002\\003\\004\\005\\006\\007\\b\\t\\n\\v\\f\\r\\016\\017\\020\\021\\022\\023\\024\\025\\026\\027\\030\\031\\032\\033\\034\\035\\036\\037 !\\\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\177\200\201\202\203\204\205\206\207\210\211\212\213\214\215\216\217\220\221\222\223\224\225\226\227\230\231\232\233\234\235\236\237\240\241\242\243\244\245\246\247\250\251\252\253\254\255\256\257\260\261\262\263\264\265\266\267\270\271\272\273\274\275\276\277\300\301\302\303\304\305\306\307\310\311\312\313\314\315\316\317\320\321\322\323\324\325\326\327\330\331\332\333\334\335\336\337\340\341\342\343\344\345\346\347\350\351\352\353\354\355\356\357\360\361\362\363\364\365\366\367\370\371\372\373\374\375\376\377" )

;;
;;
;;
(define (test-bps)
   (test-module "bps" "bps.scm")
   (reset)
   ;; CHARACTER
   (test "char?.1" (char? (maybe #\a)) #t)
   (test "char?.2" (char? (maybe 32)) #f)
   (test "char?.3" (char? (string-ref (maybe "hello") 1)) #t)
   (test "char->integer" (char->integer (maybe #\a)) (char->integer #\a))
   (test "char->integer" (char->integer (maybe #\é)) (char->integer #\é))
   ;; string-as-read against integer
   (test "string-as-read.1" (ecs "\\n") 10)
   (test "string-as-read.2" (ecs "\\t") 9)
   (test "string-as-read.3" (ecs "\\b") 8)
   (test "string-as-read.4" (ecs "\\r") 13)
   (test "string-as-read.5" (ecs "\\f") 12)
   (test "string-as-read.6" (ecs "\\v") 11)
   (test "string-as-read.7" (ecs "\\\\") 92)
   (test "string-as-read.8" (ecs "\\'") 39)
   (test "string-as-read.9" (ecs "\\\"") 34)
   (test "string-as-read.10" (ecs "\\a") 97)
   (test "string-as-read.11" (ecs "\\?") 63)
   (test "string-as-read.12" (ecs "\\040") 32)
   (test "string-as-read.13" (ecs "\\000") 0)
   (test "string-as-read.14" (ecs "\\001") 1)
   (test "string-as-read.15" (ecs "\\377") 255)
   (test "string-as-read.16" (ecs "\\200") 128)
   ;; string-as-read against #"" (runtime against compiletime)
   (test "string-as-read.17" (string-as-read "\\n") #"\n")
   (test "string-as-read.18" (string-as-read "\\t") #"\t")
   (test "string-as-read.19" (string-as-read "\\b") #"\b")
   (test "string-as-read.20" (string-as-read "\\r") #"\r")
   (test "string-as-read.21" (string-as-read "\\f") #"\f")
   (test "string-as-read.22" (string-as-read "\\v") #"\v")
   (test "string-as-read.23" (string-as-read "\\\\") #"\\")
   (test "string-as-read.24" (string-as-read "\\'") #"\'")
   (test "string-as-read.25" (string-as-read "\\\"") #"\"")
   (test "string-as-read.26" (string-as-read "\\a") #"\a")
   (test "string-as-read.27" (string-as-read "\\?") #"\?")
   (test "string-as-read.28" (string-as-read "\\040") #"\040")
   (test "string-as-read.29" (string-as-read "\\000") #"\000")
   (test "string-as-read.30" (string-as-read "\\001") #"\001")
   (test "string-as-read.31" (string-as-read "\\377") #"\377")
   (test "string-as-read.32" (string-as-read "\\200") #"\200")
   (test "string-as-read.33" (string-as-read "k") "k")
   (test "string-as-read.34" (string-as-read "\\\\k") "\\k")
   ;; string-for-read
   (test "string-for-read.35" (string-for-read *all-chars-in-string*)
	 *all-chars-in-for-read-string* )
   ;; blit-string
   (let ( (s "abcdefghijklmnopqrstuvwxyz") )
      (test "blit-string" (begin (blit-string! s 1 s 1 11) s)
	    "abcdefghijklmnopqrstuvwxyz" )
      (test "blit-string" (begin (blit-string! s 1 s 3 11) s)
	    "abcbcdefghijklopqrstuvwxyz" )
      (test "blit-string" (begin (blit-string! s 4 s 1 17) s)
	    "acdefghijklopqrstustuvwxyz" ) )
   (test "higher order"
	 ((((((((((((((((((((((((((((((((((((((lambda (x) x) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) (lambda (i) i)) #t)
	 #t))

