(module loader
   (main main))

(define (main argv)
   (print "loading: " (cadr argv))
   (dynamic-load (cadr argv))
   (print "loaded...")
   (dynamic-unload (cadr argv))
   (print "unloaded..."))
