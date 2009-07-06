;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Gdb/tools.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 15:41:13 1999                          */
;*    Last change :  Wed Aug  9 17:17:05 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    General GDB parsing tools. This module implements small GDB      */
;*    facilities to gather small informations about expressions.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gdb_tools
   (import tools_tools)
   (export (gdb-typeof::bstring ::bstring)
	   (gdb-current-scope))
   (import gdb_invoke))

;*---------------------------------------------------------------------*/
;*    gdb-typeof ...                                                   */
;*    -------------------------------------------------------------    */
;*    Ask GDB for the type of a variable. Returns it as a string.      */
;*---------------------------------------------------------------------*/
(define (gdb-typeof::bstring var::bstring)
   (let* ((cmd (string-append "whatis " var))
	  (res (gdb-server->string cmd)))
      ;; whatis format is <TYPE> = <TYPE VALUE>. to fetch the type
      ;; value we skip everything until the = characters
      (string-from res #\= 2)))

;*---------------------------------------------------------------------*/
;*    gdb-current-scope ...                                            */
;*    -------------------------------------------------------------    */
;*    Returns the current scope (this function does do any parsing,    */
;*    it returns a plain string as produced by gdb).                   */
;*---------------------------------------------------------------------*/
(define (gdb-current-scope)
   ;; we have to compute the current line number
   (let ((line (gdb-server->string "info line")))
      (if (substring=? "Line " line 5)
	  (let* ((lnum (string-until-at line 5 #\space 0))
		 (cmd  (string-append "info scope " lnum)))
	     (if (= (string->integer lnum) 0)
		 ;; there is no line for that file
		 #f
		 ;; then we can request an "info scope"
		 (gdb-server->string cmd)))
	  #f)))
   
