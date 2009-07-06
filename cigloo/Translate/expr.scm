;*=====================================================================*/
;*    serrano/prgm/project/cigloo/Translate/expr.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 29 15:49:22 1995                          */
;*    Last change :  Mon Dec  4 14:48:26 1995 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The expression handling                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module translate_expr
   (include "Translate/ast.sch"
	    "Translate/type.sch")
   (import  translate_type
	    translate_tspec
	    translate_decl)
   (export  (expr->scheme-id expr)))

;*---------------------------------------------------------------------*/
;*    expr->scheme-id ...                                              */
;*---------------------------------------------------------------------*/
(define (expr->scheme-id expr)
   (string->scheme-id (expr->string expr)))

;*---------------------------------------------------------------------*/
;*    expr->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (expr->string expr)
   (cond
      ((symbol? expr)
       (symbol->string expr))
      ((number? expr)
       (number->string expr))
      ((string? expr)
       expr)
      ((pair? expr)
       (let loop ((expr (reverse! expr))
		  (res  ""))
	  (if (null? expr)
	      res
	      (loop (cdr expr)
		    (string-append (expr->string (car expr)) res)))))
      (else
       (if (ast? expr)
	   (ast-case expr
	      ((t-name)
	       (let ((t (let ((t (tspec->type (t-name-type-spec-list expr))))
			   (if (t-name-adecl expr)
			       (type+adecl->type t (t-name-adecl expr))
			       t))))
		  (if (type-$ t)
		      (replace-$ (type-c-name t) "")
		      (type-c-name t))))
	      (else
	       "_"))
	   "_"))))

;*---------------------------------------------------------------------*/
;*    string->scheme-id ...                                            */
;*---------------------------------------------------------------------*/
(define (string->scheme-id string)
   (define (correct-char? char)
      (cond
	 ((char-alphabetic? char)
	  #t)
	 ((char-numeric? char)
	  #t)
	 ((memq char '(#\! #\@ #\# #\$ #\% #\^ #\& #\* #\_ #\+ #\=
		       #\| #\\ #\~ #\: #\< #\> #\? #\/))
	  #t)
	 (else
	  #f)))
   (let loop ((i (-fx (string-length string) 1)))
      (cond
	 ((=fx i -1)
	  string)
	 ((correct-char? (string-ref string i))
	  (loop (-fx i 1)))
	 (else
	  (string-set! string i #\-)
	  (loop (-fx i 1))))))
