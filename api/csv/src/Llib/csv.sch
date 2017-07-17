;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/csv/src/Llib/csv.sch             */
;*    -------------------------------------------------------------    */
;*    Author      :  Joseph Donaldson (donaldsonjw@yahoo.com)          */
;*    Creation    :  Fri Feb 24 07:12:29 2012                          */
;*    Last change :  Mon Jul 17 08:05:36 2017 (serrano)                */
;*    Copyright   :  2011-17 Joseph Donaldson                          */
;*    -------------------------------------------------------------    */
;*    This file is part of bigloo-csv.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    make-csv-lexer                                                   */
;*---------------------------------------------------------------------*/
(define-macro (make-csv-lexer sep quot)
   (if (and (char? sep) (char? quot))
       `(regular-grammar ((quote ,quot)
			  (separator ,sep))
	   (quote
	    (let loop ((curr (read-char (the-port)))
		       (res ""))
	       (cond ((eof-object? curr)
		      (raise (instantiate::&io-parse-error
				(proc "csv lexer")
				(msg "failed to parse")
				(obj curr))))
		     ((and (char=? curr ,quot)
			   (not (eof-object? (peek-char (the-port))))
			   (char=? (peek-char (the-port)) ,quot))
		      (read-char (the-port))
		      (loop (read-char (the-port))
			 (string-append res (string ,quot))))
		     ((char=? curr ,quot)
		      (cons 'text res))
		     (else
		      (loop (read-char (the-port))
			 (string-append res (string curr)))))))
	   (separator
	    'separator)
	   ((or (: #\return #\newline) #\newline)
	    'newline)
	   ((+ (out quote separator #\return #\newline))
	    (cons 'text (the-string)))
	   (else 
	    (let ((c (the-failure)))
	       (if (eof-object? c)
		   c
		   (error "csv-lexer" "Illegal character" c)))))
       (error "csv-lexer"
	  "separator and quote must be a single character"
	  (list sep quot))))
