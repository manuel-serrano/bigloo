;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Engine/translate.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 24 10:53:17 1995                          */
;*    Last change :  Mon Jul 31 10:08:20 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The translation on an input-port                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module engine_translate
   (include "Parser/coord.sch"
	    "Translate/ast.sch")
   (import  engine_param
	    parser_parser
	    parser_lexer
	    translate_function
	    translate_declaration
	    translate_type
	    tools_speek
	    tools_error)
   (export  (translate ::input-port ::bstring ::symbol)))

;*---------------------------------------------------------------------*/
;*    cpp ...                                                          */
;*---------------------------------------------------------------------*/
(define *cpp-command* "cpp")
(define *cpp-args* '("-I." "-dDI"))

(define (cpp iport)
  (let* ((proc (apply run-process *cpp-command* input: pipe: output: pipe: *cpp-args*))
         (pin (process-input-port proc)))
    (send-chars iport pin)
    (close-output-port pin)
    (process-output-port proc)))

;*---------------------------------------------------------------------*/
;*    translate ...                                                    */
;*---------------------------------------------------------------------*/
(define (translate iport name mode)
   (let ((iport (if *use-cpp* (cpp iport) iport)))
     (verbose 0 name #\: #\Newline)
     (let ((old-iname *iname*))
        (set! *iname* (prefix (basename name)))
        (with-exception-handler
           (lambda (e)
              (error-notify e)
              (if (string? *dest*)
          	(begin
          	   (close-output-port *oport*)
          	   (if (file-exists? *dest*)
          	       (delete-file *dest*))
          	   (exit -1))))
           (lambda ()
              ;; we must print the name of the file before 
              ;; starting parsing.
              (if (eq? mode 'open)
          	(fprint *oport* "   ;; beginning of " name))
              ;; if the -include-directive has been used we produce
              ;; a bigloo include directive
              (if *include-directive*
          	(fprint *oport* "   (include \"" name "\")"))
              (let ((ast (parse iport)))
                 (for-each translate-ast ast)
                 (translate-function-declarations)
                 (translate-types!)
                 (if (eq? mode 'open)
          	   (fprint *oport* "   ;; end of " name)))))
        (set! *iname* old-iname))))

;*---------------------------------------------------------------------*/
;*    parse ...                                                        */
;*---------------------------------------------------------------------*/
(define (parse iport) 
   (try (reverse! (read/lalrp parser lexer iport))
	(lambda (escape proc mes obj)
	   (match-case obj
	      ((cpp ?coord ?obj)
	       (error/location proc
			       mes
			       obj
			       (coord-fname coord)
			       (coord-pos coord)))
	      ((?token #{coord ?fname ?pos} . ?-)
	       (error/location "cigloo" proc "parse error" fname pos))
	      (else
	       (error proc mes obj))))))

;*---------------------------------------------------------------------*/
;*    translate-ast ...                                                */
;*---------------------------------------------------------------------*/
(define (translate-ast ast)
   (if (not (ast? ast))
       'ignore
       (ast-case ast
	  ((fun-def)
	   (translate-function-definition ast))
	  ((declare)
	   (translate-declaration ast))
	  (else
	   (error/ast "cigloo"
		      "Don't know what to do with this expression"
		      ast)))))
	    
