;; ---------------------------------------------------------------------- ;;
;; FICHIER               : driver.scm                                     ;;
;; DATE DE CREATION      : Mon Jul  3 11:47:26 1995                       ;;
;; DERNIERE MODIFICATION : Thu Jul  6 10:44:58 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Le moteur des analyseurs syntaxiques LALR(1)...                        ;;
;; ---------------------------------------------------------------------- ;;

(module __lalr_driver

   (import  __error
	    __param)
   
   (use     __type
            __bigloo
            __tvector
            __structure
            __tvector
	    __bexit
	    __bignum
	    __object
	    __thread
	    __rgc
	    __bit

            __r4_numbers_6_5
            __r4_numbers_6_5_fixnum
            __r4_numbers_6_5_flonum
            __r4_numbers_6_5_flonum_dtoa
            __r4_characters_6_6
            __r4_equivalence_6_2
            __r4_booleans_6_1
            __r4_symbols_6_4
            __r4_strings_6_7
            __r4_pairs_and_lists_6_3
            __r4_input_6_10_2
            __r4_control_features_6_9
            __r4_vectors_6_8
            __r4_ports_6_10_1
            __r4_output_6_10_3
	    __evenv)
   
   (export (__make-parser action-table reduction-table)))

(define (lalr-debug)           100)

(define *max-stack-size*       500)
(define *stack-size-increment* 200)

(define (grow-stack! v)
  (let* ((len (vector-length v))
	 (v2  (make-vector (+fx len *stack-size-increment*) 0)))
    (let loop ((i 0))
      (if (<fx i len)
	  (begin
	    (vector-set! v2 i (vector-ref v i))
	    (loop (+fx i 1)))
	  v2))))

(define (__make-parser action-table reduction-function)

   (lambda (rgc input-port is-eof?)
      
      (define (action x l)
	 (let ((y (assq x l)))
	    (if y 
		(cdr y) 
		(cdar l))))
      
      (let ((stack (make-vector *max-stack-size* 0))
	    (state   #f)
	    (input   #f)
	    (in      #f)
	    (attr    #f)
	    (acts    #f)
	    (act     #f)
	    (eof?    #f)
	    (debug   (>=fx (bigloo-debug) (lalr-debug))))
	 
	 (let loop ((sp 0))
	    (set! state (vector-ref stack sp))
	    (set! acts (vector-ref action-table state))
	    
	    (if (null? (cdr acts))
		(set! act (cdar acts))
		(begin
		   (if (not input)
		       (set! input (read/rp rgc input-port)))
		   (if (not input)
		       (error/errno $errno-io-parse-error
				    'parser "Illegal `#f' token"
				    #f))
		   
		   (cond 
		      ((is-eof? input)
		       (set! in '*eoi*)
		       (set! attr #f)
		       (set! eof? #t))
		      ((pair? input)
		       (set! in (car input))
		       (set! attr (cdr input)))
		      (else   
		       (set! in input)
		       (set! attr #f)))
		   
		   (set! act (action in acts))))

	    (when debug
	       (display "LALR TRACE: input=" (current-error-port))
	       (write in (current-error-port))
	       (display " state=" (current-error-port))
	       (write state (current-error-port))
	       (display " sp=" (current-error-port))
	       (write sp (current-error-port))
	       (newline (current-error-port)))
	    
	    (cond
	       
	       ;; Input succesfully parsed
	       ((eq? act 'accept)
		(vector-ref stack 1))
	       
	       ;; Syntax error in input
	       ((or (eq? act '*error*) (eq? act 'error))
		(let ((msg (string-append
			    "parse error (unexpected token `"
			    (cond
			       ((symbol? in)
				(symbol->string in))
			       ((char? in)
				(make-string 1 in))
			       (else
				(let ((port (open-output-string)))
				   (write in port)
				   (close-output-port port))))
			    "')")))
		   (error/errno $errno-io-parse-error "parser" msg input)))
	       
	       ;; Shift current token on top of the stack
	       ((>=fx act 0)
		(if (>=fx sp (-fx (vector-length stack) 4))
		    (set! stack (grow-stack! stack)))
		(vector-set! stack (+fx sp 1) attr)
		(vector-set! stack (+fx sp 2) act)
		(if (not eof?)
		    (set! input #f))
		(loop (+fx sp 2)))
	       
	       ;; Reduce by rule (- act)
	       (else 
		(loop (reduction-function (negfx act) stack sp))))))))


