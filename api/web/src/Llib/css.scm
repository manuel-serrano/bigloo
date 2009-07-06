;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/css.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 20 07:33:38 2005                          */
;*    Last change :  Sat Apr 11 06:54:21 2009 (serrano)                */
;*    Copyright   :  2005-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    CSS parsing                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_css

   (import __web_css-lexer
	   __web_css-parser
	   __web_css-ast)
   
   (export (css->ast::css-stylesheet ::input-port #!key extension eoff)
	   (css-parse::pair-nil ::input-port
				#!key
				class
				element-name
				declaration
				extension
				eoff)))

;*---------------------------------------------------------------------*/
;*    css->ast ...                                                     */
;*---------------------------------------------------------------------*/
(define (css->ast iport::input-port #!key extension eoff)
   (let ((reader (let ((buf '())
		       (lexer (lambda (p)
				 (read/rp (css-lexer) p
					  (or extension (lambda (e p) #f))
					  (or eoff (lambda (e p) #f))))))
		    (lambda (p)
		       (let loop ()
			  (define (doread ip)
			     (let ((v (lexer ip)))
				(if (css-extension? v)
				    (begin
				       (set! buf (cons (cadr v) buf))
				       (loop))
				    v)))
			  (cond
			     ((null? buf)
			      (doread p))
			     ((string? (car buf))
			      (let ((p2 (open-input-string (car buf))))
				 (set! buf (cons p2 (cdr buf)))
				 (loop)))
			     ((input-port? (car buf))
			      (let ((v (doread (car buf))))
				 (if (eof-object? v)
				     (begin
					(close-input-port (car buf))
					(set! buf (cdr buf))
					(loop))
				     v)))
			     (else
			      (error 'css-parse "Illegal state" buf))))))))
      (read/lalrp (css-grammar) reader iport)))

;*---------------------------------------------------------------------*/
;*    css-parse ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function is only given for backward compatiblity. For new   */
;*    code, the function css->ast should be prefered.                  */
;*---------------------------------------------------------------------*/
(define (css-parse::pair-nil iport::input-port
			     #!key
			     class
			     element-name
			     declaration
			     extension
 			     eoff)
   (css-parser (css->ast iport :extension extension :eoff eoff)
	       (or class make-klass)
	       (or element-name identity)
	       (or declaration make-declaration)))
	      
;*---------------------------------------------------------------------*/
;*    identity ...                                                     */
;*---------------------------------------------------------------------*/
(define (identity x)
   x)

;*---------------------------------------------------------------------*/
;*    make-klass ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-klass name)
   (string-append "." name))

;*---------------------------------------------------------------------*/
;*    make-declaration ...                                             */
;*---------------------------------------------------------------------*/
(define (make-declaration property expr prio)
   (list "  " property ": " expr prio))
