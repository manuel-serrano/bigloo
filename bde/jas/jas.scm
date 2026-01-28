(module jas
   (import jas_as jas_peep)
   (main jas) )

(define (jas a)
   (if (and (pair? (cdr a))
	    (or (string=? (cadr a) "-help")
		(string=? (cadr a) "--help")))
       (print "usage: jas <filein> [-nopeep] <fileout>")
       (match-case (cdr a)
	  ((?filein ?fileout)
	   (doit filein fileout) )
	  ((?filein ?- ?fileout)
	   (set! *jas-peephole* #f)
	   (doit filein fileout) )
	  (else
	   (error "jas"
	      (format "bad command line (see \"~a --help\")" (car a))
	      a) ))))

(define (doit filein fileout)
   (let ( (def (call-with-input-file filein read)) )
      (let ((port (open-output-binary-file fileout)))
	 (jvm-as def port)
	 (close-binary-port port))))
