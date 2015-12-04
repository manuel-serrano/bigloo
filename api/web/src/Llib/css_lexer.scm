;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/css_lexer.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 20 07:51:32 2005                          */
;*    Last change :  Thu Dec  3 11:11:21 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    CSS lexing                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_css-lexer

   (option (set! *unsafe-type* #t)
	   (set! *unsafe-arity* #t))

   (export (css-lexer)
	   (inline css-extension?::bool v)))

;*---------------------------------------------------------------------*/
;*    css-lexer ...                                                    */
;*---------------------------------------------------------------------*/
(define (css-lexer)
   *css-lexer*)

;*---------------------------------------------------------------------*/
;*    css-parse-error ...                                              */
;*---------------------------------------------------------------------*/
(define (css-parse-error msg obj port)
   (raise
    (instantiate::&io-parse-error
       (proc 'css-parse)
       (msg msg)
       (obj obj)
       (fname (input-port-name port))
       (location (input-port-position port)))))

;*---------------------------------------------------------------------*/
;*    css-extension ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (css-extension? v)
   (and (pair? v) (eq? (car v) 'EXTENSION)))

;*---------------------------------------------------------------------*/
;*    return ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (return key . val)
   `(list ,key
	  ,(if (pair? val) (car val) '(the-string))
	  (input-port-name (the-port))
	  (input-port-position (the-port))))

;*---------------------------------------------------------------------*/
;*    *css-lexer* ...                                                  */
;*---------------------------------------------------------------------*/
(define *css-lexer*
   (regular-grammar ((h (uncase (in ("09af"))))
		     (nonascii (in (#a128 #a255)))
		     (unicode (: #\\ (+ h) (? (in " \t\r\n\f"))))
		     (escape (or unicode
				 (: #\\ (or (in " -~") (in (#a128 #a255))))))
		     (nmstart (or "-"
				  "_"
				  (uncase (in ("az")))
				  nonascii
				  escape))
		     (nmchar (or nmstart (in ("09"))))
		     
		     (string1 (: #\" (* (out #\")) #\"))
		     (string2 (: #\' (* (out #\')) #\'))
		     (string (or string1 string2))
		     
		     (ident (: nmstart (* nmchar)))
		     (name (+ nmchar))
		     (num (or (+ (in ("09")))
			      (: (* (in ("09"))) #\. (+ (in ("09"))))))
		     (+/-num (: (? (in "+-")) num))
		     (url (+ (or (in "_/:!#$%&*-~.") nonascii escape
				 digit alpha)))
		     (w (* (in " \t\r\n\f")))
		     (nl (or (in "\n\r\f") "\r\n"))
		     (range (+ (or h #\?)))
		     
		     extension
		     eof)
      
      ((+ (in " \t\r\n\f"))
       (return 'S #f))
      
      ((: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*)))) (+ #\*) "/")
       (ignore))
      
      ("<!--"
       (return 'CDO))
      
      ("-->"
       (return 'CDC))
      
      ("~="
       (return 'INCLUDES))
      
      ("|="
       (return 'DASHMATCH))
      
      (string
       (return 'STRING))
      
      ("and"
       (return 'AND_SYM))
      
      ("not"
       (return 'NOT_SYM))
      
      ("only"
       (return 'ONLY_SYM))
      
      (ident
       (return 'IDENT))
      
      ((: "#" name)
       (return 'HASH (the-substring 1 (the-length))))
      
      ("@import"
       (return 'IMPORT_SYM))
      ("@page"
       (return 'PAGE_SYM))
      ("@media"
       (return 'MEDIA_SYM))
      ("@font-face"
       (return 'FONT_FACE_SYM))
      ("@charset"
       (return 'CHARSET_SYM))
      ((: "@" (? (: "-" (+ (out #\-)) "-")) "keyframes")
       (return 'KEYFRAMES_SYM (the-string)))
      ((: "@" ident)
       (return 'ATKEYWORD))
      
      ((: #\! w "important")
       (return 'IMPORTANT_SYM))
      
      ((: +/-num "em")
       (return 'EMS))
      ((: +/-num "ex")
       (return 'EXS))
      ((: +/-num "ch")
       (return 'CHS))
      ((: +/-num "rem")
       (return 'REMS))
      ((: +/-num (or "px" "cm" "mm" "in" "pt" "pc" "vw" "vh" "vmin" "vmax"))
       (return 'LENGTH))
      ((: +/-num (or "deg" "rad" "grad"))
       (return 'ANGLE))
      ((: +/-num (or "ms" "s"))
       (return 'TIME))
      ((: +/-num (or (uncase "Hz") (uncase "hkz")))
       (return 'FREQ))
      ((: +/-num ident)
       (return 'DIMEN))
      ((: +/-num "%")
       (return 'PERCENTAGE))
      (+/-num
       (return 'NUMBER))
      
      ((: "url(" w string w ")")
       (return 'URI (the-substring 4 -1)))
      ((: "url(" w url w ")")
       (return 'URI (the-substring 4 -1)))
      ((: "not" (* (in " \t\r\n\f")) "(")
       (return 'NOT_PSEUDO ))
      ((: ident "(")
       (return 'FUNCTION (the-substring 0 -1)))
      
      ((: "U+" range)
       (return 'UNICODERANGE))
      ((: "U+" (: (** 1 6 h) #\- (** 1 6 h)))
       (return 'UNICODERANGE))
      
      ((: "#" (or (= 3 (uncase (in ("09af"))))
		  (= 6 (uncase (in ("09af"))))))
       (return 'RGB))
      
      (#\:
       (return 'COLON))
      
      (#\;
       (return 'SEMI-COLON))
      
      (#\,
       (return 'COMMA))
      
      (#\(
       (return 'PAR-OPEN))
      (#\)
       (return 'PAR-CLO))
      
      (#\{
       (return 'BRA-OPEN))
      
      (#\}
       (return 'BRA-CLO))
      
      (#\[
       (return 'ANGLE-OPEN))
      
      (#\]
       (return 'ANGLE-CLO))
      
      (#\/
       (return 'SLASH))
      
      (#\+
       (return '+))
      
      (#\*
       (return '*))
      
      (#\>
       (return '>))
      
      (#\~
       (return '~))
      
      (#\-
       (return '-))
      
      (#\.
       (return 'DOT))
      
      (#\=
       (return '=))
      
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (let ((e (extension c (the-port))))
		 (cond
		    ((or (not e) (eq? e #unspecified) (symbol? e))
		     (ignore))
		    ((string? e)
		     (unread-string! e (the-port))
		     (ignore))
		    (else
		     (return 'VALUE e)))))))))


