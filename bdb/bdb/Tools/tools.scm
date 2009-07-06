;*=====================================================================*/
;*    serrano/prgm/project/bdk/kbdb/src/Tools/tools.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 15:50:51 1999                          */
;*    Last change :  Thu Jan 25 01:12:23 2001 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Various tools (string tokenification, ...).                      */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_tools
   (import engine_param)
   (export (num->string::bstring ::obj)
	   (string-from::bstring ::bstring ::char ::int)
	   (string-from-at::bstring ::bstring ::int ::char ::int)
	   (string-until::bstring ::bstring ::char ::int)
	   (string-until-at::bstring ::bstring ::int ::char ::int)
	   (string-char-after::char ::bstring ::char)
	   (string-prefix?::bool ::bstring ::bstring)
	   (newline?::bool ::bstring)
	   (bigloo-ident?::bool ::bstring)
	   (fetch-source-line::bstring ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    num->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (num->string num)
   (if (< num 0.01)
       "0.0"
       (let* ((num (if (and (flonum? num) (>fl num 100.0))
                       (inexact->exact num)
                       num))
              (str (number->string num)))
          (if (and (flonum? num) (>fx (string-length str) 4))
              (substring str 0 4)
              str))))

;*---------------------------------------------------------------------*/
;*    newline? ...                                                     */
;*    -------------------------------------------------------------    */
;*    Is a string a plain newline?                                     */
;*---------------------------------------------------------------------*/
(define (newline? string)
   (char=? (string-ref string 0) #\Newline))

;*---------------------------------------------------------------------*/
;*    string-from ...                                                  */
;*    -------------------------------------------------------------    */
;*    Skip all characters until MARK, returns the rest of the          */
;*    string. Delta is how many characters to skip from the marker.    */
;*---------------------------------------------------------------------*/
(define (string-from string mark delta)
   (let ((len (string-length string)))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     "")
	    ((char=? (string-ref string i) mark)
	     (substring string (+fx i delta) len))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-from-at ...                                               */
;*    -------------------------------------------------------------    */
;*    Skip all characters until MARK, returns the rest of the          */
;*    string. Delta is how many characters to skip from the marker.    */
;*---------------------------------------------------------------------*/
(define (string-from-at string start mark delta)
   (let ((len (string-length string)))
      (let loop ((i start))
	 (cond
	    ((=fx i len)
	     "")
	    ((char=? (string-ref string i) mark)
	     (substring string (+fx i delta) len))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-until ...                                                 */
;*    -------------------------------------------------------------    */
;*    Skip all characters after MARK, returns the rest of the          */
;*    string. Delta is how many characters to skip from the marker.    */
;*---------------------------------------------------------------------*/
(define (string-until string mark delta)
   (string-until-at string 0 mark delta))

;*---------------------------------------------------------------------*/
;*    string-until-at ...                                              */
;*    -------------------------------------------------------------    */
;*    Skip all characters after MARK, returns the rest of the          */
;*    string. Delta is how many characters to skip from the marker.    */
;*---------------------------------------------------------------------*/
(define (string-until-at string start mark delta)
   (let ((len (string-length string)))
      (let loop ((i start))
	 (cond
	    ((>=fx i len)
	     "")
	    ((char=? (string-ref string i) mark)
	     (substring string start (+fx i delta)))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-char-after ...                                            */
;*    -------------------------------------------------------------    */
;*    Return the char following the first occurrence of CHAR in        */
;*    STRING.                                                          */
;*---------------------------------------------------------------------*/
(define (string-char-after string char)
   (let ((len (-fx (string-length string) 1)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i len)
	     (error "string-char-after:Can't find char" string char))
	    ((char=? (string-ref string i) char)
	     (string-ref string (+fx i 1)))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-prefix? ...                                               */
;*    -------------------------------------------------------------    */
;*    Is LEFT a prefix of RIGHT?                                       */
;*---------------------------------------------------------------------*/
(define (string-prefix? left::bstring right::bstring)
   (substring=? left right (string-length left)))

;*---------------------------------------------------------------------*/
;*    bigloo-ident? ...                                                */
;*    -------------------------------------------------------------    */
;*    The only way to write a correct parser here is to use the        */
;*    Bigloo regular-expression for identifier, simply removing lower  */
;*    case symbols.                                                    */
;*---------------------------------------------------------------------*/
(define (bigloo-ident? str)
   (if (memq *bdb-mode* '(mixte scheme))
       (let* ((g (regular-grammar ((letter  (in ("AZ") (#a128 #a255)))
				   (special (in "z!@~$%^&*></-_+\\|=?.:"))
				   (id      (: (* digit)
					       (or letter special)
					       (* (or letter
						      special
						      digit
						      (in ",'`"))))))
		    (id (=fx (the-length) (string-length str)))
		    (else #f)))
	      (p (open-input-string str))
	      (r (read/rp g p)))
	  (close-input-port p)
	  r)
       #f))
	 
;*---------------------------------------------------------------------*/
;*    fetch-source-line ...                                            */
;*    -------------------------------------------------------------    */
;*    Extract one source line. If an error is raised, produced a       */
;*    gdb like error string message.                                   */
;*---------------------------------------------------------------------*/
(define (fetch-source-line::bstring file::bstring sline::bstring)
   (let ((line (string->integer sline)))
      (if (not (file-exists? file))
	  (string-append "No source file named " file ".")
	  (let ((port (open-input-file file)))
	     (unwind-protect
		(let loop ((str  (read-line port))
			   (lnum 1))
		   (cond
		      ((eof-object? str)
		       (string-append "Line number "
				      sline
				      " out of range; "
				      (basename file)
				      " has " (integer->string lnum) "lines."))
		      ((=fx lnum line)
		       (string-append sline
				      (cond 
					 ((< line 10) ":    ")
					 ((< line 100) ":   ")
					 ((< line 1000) ":  "))
				      str))
		      (else
		       (loop (read-line port) (+fx lnum 1)))))
		(close-input-port port))))))
   
