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
;; escape-C-string
;;
(define (ecs s)
   (let* ( (ss (escape-C-string s))
	   (n (string-length ss)) )
      (if (=fx n 1)
	  (char->integer (string-ref ss 0))
	  (if (=fx n 0)
	      -1000
	      (- n) ))))

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
   "\\000\\001\\002\\003\\004\\005\\006\\007\\b\\t\\n\\v\\f\\r\\016\\017\\020\\021\\022\\023\\024\\025\\026\\027\\030\\031\\032\\033\\034\\035\\036\\037 !\\\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\\177\\200\\201\\202\\203\\204\\205\\206\\207\\210\\211\\212\\213\\214\\215\\216\\217\\220\\221\\222\\223\\224\\225\\226\\227\\230\\231\\232\\233\\234\\235\\236\\237\\240\\241\\242\\243\\244\\245\\246\\247\\250\\251\\252\\253\\254\\255\\256\\257\\260\\261\\262\\263\\264\\265\\266\\267\\270\\271\\272\\273\\274\\275\\276\\277\\300\\301\\302\\303\\304\\305\\306\\307\\310\\311\\312\\313\\314\\315\\316\\317\\320\\321\\322\\323\\324\\325\\326\\327\\330\\331\\332\\333\\334\\335\\336\\337\\340\\341\\342\\343\\344\\345\\346\\347\\350\\351\\352\\353\\354\\355\\356\\357\\360\\361\\362\\363\\364\\365\\366\\367\\370\\371\\372\\373\\374\\375\\376\\377" )

;;
;;
;;
(define (test-bps)
   (test-module "bps" "bps.scm")
   (reset)
   ;; CHARACTER
   (test "char?" (char? (maybe #\a)) #t)
   (test "char?" (char? (maybe 32)) #f)
   (test "char?" (char? (string-ref (maybe "hello") 1)) #t)
   (test "char->integer" (char->integer (maybe #\a)) (char->integer #\a))
   (test "char->integer" (char->integer (maybe #\é)) (char->integer #\é))
   ;; escape-C-string against integer
   (test "escape-C-string" (ecs "X\\n") 10)
   (test "escape-C-string" (ecs "X\\t") 9)
   (test "escape-C-string" (ecs "X\\b") 8)
   (test "escape-C-string" (ecs "X\\r") 13)
   (test "escape-C-string" (ecs "X\\f") 12)
   (test "escape-C-string" (ecs "X\\v") 11)
   (test "escape-C-string" (ecs "X\\\\") 92)
   (test "escape-C-string" (ecs "X\\'") 39)
   (test "escape-C-string" (ecs "X\\\"") 34)
   (test "escape-C-string" (ecs "X\\a") 97)
   (test "escape-C-string" (ecs "X\\?") 63)
   (test "escape-C-string" (ecs "X\\040") 32)
   (test "escape-C-string" (ecs "X\\000") 0)
   (test "escape-C-string" (ecs "X\\001") 1)
   (test "escape-C-string" (ecs "X\\377") 255)
   (test "escape-C-string" (ecs "X\\200") 128)
   ;; escape-C-string against #"" (runtime against compiletime)
   (test "escape-C-string" (escape-C-string "X\\n") #"\n")
   (test "escape-C-string" (escape-C-string "X\\t") #"\t")
   (test "escape-C-string" (escape-C-string "X\\b") #"\b")
   (test "escape-C-string" (escape-C-string "X\\r") #"\r")
   (test "escape-C-string" (escape-C-string "X\\f") #"\f")
   (test "escape-C-string" (escape-C-string "X\\v") #"\v")
   (test "escape-C-string" (escape-C-string "X\\\\") #"\\")
   (test "escape-C-string" (escape-C-string "X\\'") #"\'")
   (test "escape-C-string" (escape-C-string "X\\\"") #"\"")
   (test "escape-C-string" (escape-C-string "X\\a") #"\a")
   (test "escape-C-string" (escape-C-string "X\\?") #"\?")
   (test "escape-C-string" (escape-C-string "X\\040") #"\040")
   (test "escape-C-string" (escape-C-string "X\\000") #"\000")
   (test "escape-C-string" (escape-C-string "X\\001") #"\001")
   (test "escape-C-string" (escape-C-string "X\\377") #"\377")
   (test "escape-C-string" (escape-C-string "X\\200") #"\200")
   ;; escape-scheme-string
   (test "escape-scheme-string" (escape-scheme-string "ok") "ok")
   (test "escape-scheme-string" (escape-scheme-string "\\o\\k") "ok")
   (test "escape-scheme-string" (escape-scheme-string "\\\\\\k") "\\k")
   ;; string-for-read
   (test "string-for-read" (string-for-read *all-chars-in-string*)
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

