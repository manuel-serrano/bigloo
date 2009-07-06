(module repl
   (extern (macro _printf::int (::string ::string) "printf"))
   (export (printf fmt str))
   (eval   (export-all)))

(define (printf fmt value)
   (_printf fmt value))

(repl)
