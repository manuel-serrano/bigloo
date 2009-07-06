;;;; s e r v e r  . s c m               -- A simple sever
;;;;
;;;; Copyright © 1993-1996 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
;;;; 
;;;; Permission to use, copy, and/or distribute this software and its
;;;; documentation for any purpose and without fee is hereby granted, provided
;;;; that both the above copyright notice and this permission notice appear in
;;;; all copies and derived works.  Fees for distribution or use of this
;;;; software or derived works may only be charged with express written
;;;; permission of the copyright holder.  
;;;; This software is provided ``as is'' without express or implied warranty.
;;;;
;;;; This software is a derivative work of other copyrighted softwares; the
;;;; copyright notices of these softwares are placed in the file COPYRIGHTS
;;;;
;;;;           Author: Erick Gallesio [eg@kaolin.unice.fr]
;;;;    Creation date:  4-Feb-1995 18:17
;;;; Last file update: 12-Feb-1995 11:57
(module socket)

(define s1 (make-server-socket))
(define s2 #unspecified)

(dynamic-wind 
   ;; Init: Launch an xterm with telnet running
   ;; on the s listening port and connect
   (lambda ()
      (run-process "/usr/X11R6/bin/xterm" "-display" ":0" "-e" "telnet" "localhost" 
		   (number->string (socket-port-number s1)))
      (set! s2 (socket-accept s1))
      (display #"\nWelcome on the socket REPL.\n\n> " (socket-output s2))
      (flush-output-port (socket-output s2)))

   ;; Action: A toplevel like loop
   (lambda ()
      (let loop ()
	 (let ((obj (eval (read (socket-input s2)))))
	    (fprint (socket-output s2) "; Result: " obj)
	    (display "> " (socket-output s2))
	    (flush-output-port (socket-output s2))
	    (loop))))

   ;; Termination: We go here when 
   ;;     -a: an error occurs 
   ;;     -b: connection is closed
   (lambda ()
      (print #"Shutdown ......\n")
      (socket-close s2)
      (socket-shutdown s1)))
