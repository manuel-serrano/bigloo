;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/csv/src/Llib/csv.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Joseph Donaldson (donaldsonjw@yahoo.com)          */
;*    Creation    :  Fri Feb 24 07:12:29 2012                          */
;*    Last change :  Mon Jul 17 07:57:53 2017 (serrano)                */
;*    Copyright   :  2011-17 Joseph Donaldson                          */
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
           +tsv-lexer+
           +psv-lexer+
           (read-csv-record in #!optional (lexer +csv-lexer+))
           (read-csv-records in #!optional (lexer +csv-lexer+))
           (csv-for-each proc in #!optional (lexer +csv-lexer+))
           (csv-map proc in #!optional (lexer +csv-lexer+))))

;*---------------------------------------------------------------------*/
;*    default csv lexers                                               */
;*---------------------------------------------------------------------*/
(define +csv-lexer+ (make-csv-lexer #\, #\"))

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
   (if (input-port? in)
       (let ((pc (peek-char in)))
	  (if (eof-object? pc)
	      pc
	      (read/lalrp +csv-parser+ lexer in
		 (lambda (x) (or (eof-object? x) (eq? x 'newline))))))
       (raise
	  (instantiate::&io-port-error
	     (proc "read-csv-record")
	     (msg "invalid input port")
	     (obj in)))))

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
