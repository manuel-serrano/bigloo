(module pragma
   (extern (include "strings.h")))

(define (bar y)
   y)

(define (foo x)
   (bar (begin
	   (pragma "{ char aux[ 100 ];")
	   (pragma "strcpy( aux, \"toto\" );")
	   (let ((x (pragma "string_to_bstring( aux )")))
	      (print x)
	      (pragma "}")
	      (unspecified)))))

(foo 4)
