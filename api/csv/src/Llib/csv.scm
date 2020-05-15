;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/api/csv/src/Llib/csv.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Joseph Donaldson (donaldsonjw@yahoo.com)          */
;*    Creation    :  Fri Feb 24 07:12:29 2012                          */
;*    Last change :  Thu Oct 17 14:25:42 2019 (serrano)                */
;*    Copyright   :  2011-19 Joseph Donaldson                          */
;*    -------------------------------------------------------------    */
;*    This file is part of bigloo-csv.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module csv

   (option (set! *dlopen-init-gc* #t))
   
   (include "csv.sch")
   
   (export +csv-lexer+
	   +ssv-lexer+
           +tsv-lexer+
           +psv-lexer+
           (read-csv-record in #!optional (lexer +csv-lexer+))
           (read-csv-records in #!optional (lexer +csv-lexer+))
           (csv-for-each proc in #!optional (lexer +csv-lexer+))
           (csv-map proc in #!optional (lexer +csv-lexer+))))


;*---------------------------------------------------------------------*/
;*    unique unspecified value for csv                                 */
;*---------------------------------------------------------------------*/
(define +csv-unspecified+ '(#unspecified))

;*---------------------------------------------------------------------*/
;*    default csv lexers                                               */
;*---------------------------------------------------------------------*/
(define +csv-lexer+ (make-csv-lexer #\, #\"))

(define +ssv-lexer+ (make-csv-lexer #\; #\"))

(define +tsv-lexer+ (make-csv-lexer #\tab #\"))

(define +psv-lexer+ (make-csv-lexer #\| #\"))

;*---------------------------------------------------------------------*/
;*    +csv-parser+ ...                                                 */
;*---------------------------------------------------------------------*/
(define +csv-parser+
   (lalr-grammar (separator text)
		 
      (fields
	 ((field)
	  (list field))
	 ((field separator fields)
	  (cons field fields)))
      
      (field
	 (()
	  "")
	 ((text field)
	  (string-append text field)))))
		 
;*---------------------------------------------------------------------*/
;*    read-csv-record ...                                              */
;*---------------------------------------------------------------------*/
(define (read-csv-record in #!optional (lexer +csv-lexer+))
   (when (not (input-port? in))
      (raise (instantiate::&io-port-error (proc "read-csv-record")
                                          (msg "invalid input port")
                                          (obj in))))
   (let loop ((token (read/rp lexer in))
              (last-token +csv-unspecified+)
              (res '()))
      (cond ((or (eq? token 'newline)
                 (eof-object? token))
             (if (and (eof-object? token)
                      (eq? last-token +csv-unspecified+))
                 #eof-object
                  (reverse! res)))
            ((and (pair? token)
                  (eq? (car token) 'text))
             (loop (read/rp lexer in)
                (car token)
                (if (eq? last-token 'text)
                    (cons (string-append (car res) (cdr token)) (cdr res))
                    (cons (cdr token) res))))
            ((eq? token 'separator)
             (loop (read/rp lexer in)
                'separator
                ;; make sure we include blank/empty fields
                (if (eq? last-token 'separator)
                    (cons "" res)
                    res)
                ))
            (else
             (loop (read/rp lexer in)
                'text
                res)))))

;*---------------------------------------------------------------------*/
;*    read-csv-records ...                                             */
;*---------------------------------------------------------------------*/
(define (read-csv-records in #!optional (lexer +csv-lexer+))
   (let loop ((curr (read-csv-record in lexer))
	      (res '()))
      (if (eof-object? curr)
	  (reverse! res)
	  (loop (read-csv-record in lexer)
	     (cons curr res)))))

;*---------------------------------------------------------------------*/
;*    csv-for-each ...                                                 */
;*---------------------------------------------------------------------*/
(define (csv-for-each proc in #!optional (lexer +csv-lexer+))
   (let loop ((curr (read-csv-record in lexer)))
      (if (eof-object? curr)
	  #unspecified
	  (begin
	     (proc curr)
	     (loop (read-csv-record in lexer))))))

;*---------------------------------------------------------------------*/
;*    csv-map ...                                                      */
;*---------------------------------------------------------------------*/
(define (csv-map proc in #!optional (lexer +csv-lexer+))
   (let loop ((curr (read-csv-record in lexer))
	      (res '()))
      (if (eof-object? curr)
	  (reverse! res)
	  (loop (read-csv-record in lexer)
	     (cons (proc curr) res)))))
