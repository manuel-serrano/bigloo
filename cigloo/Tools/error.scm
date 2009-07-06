;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Tools/error.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 28 10:12:19 1995                          */
;*    Last change :  Thu Sep 24 08:13:22 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Error handling                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_error
   (include "Translate/ast.sch"
	    "Parser/coord.sch")
   (export  (error/ast symbol mes ast)))

;*---------------------------------------------------------------------*/
;*    error/ast ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/ast symbol mes ast)
   (let ((coord (ast-coord ast)))
      (if (coord? coord)
	  (error/location "cigloo"
			  symbol
			  mes
			  (coord-fname coord)
			  (coord-pos coord))
	  (error "cigloo" symbol mes))))
