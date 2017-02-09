;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cc/exec.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr 29 09:09:34 1995                          */
;*    Last change :  Mon Feb  6 10:35:29 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    When we don't need to make any action after invoking the C       */
;*    compiler (i.e. when the C compiler is called in a tail           */
;*    call position), rather than invoking it with the `system'        */
;*    we use the `execp' command. This module implements a             */
;*    private version of `exec' which take  parameters that            */
;*    tells if the command must return or not.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cc_exec
   (include "Tools/trace.sch")
   (extern  (macro execl::int (::string ::string ::string ::string ::void*)
		   "execl")
	    (macro NULL::void* "0L"))
   (import  tools_error
	    engine_param
	    init_main)
   (export  (exec ::bstring ::bool ::bstring)
	    (unix-filename::bstring . args)))

;*---------------------------------------------------------------------*/
;*    unix-filename ...                                                */
;*---------------------------------------------------------------------*/
(define (unix-filename . args)
   (let ((s (apply string-append args)))
      (if (string-index s #\space)
	  (string-append "\"" s "\"")
	  s)))

;*---------------------------------------------------------------------*/
;*    exec-fname ...                                                   */
;*---------------------------------------------------------------------*/
(define (exec-fname)
   "$myselfx1001")

;*---------------------------------------------------------------------*/
;*    exec ...                                                         */
;*---------------------------------------------------------------------*/
(define (exec cmd come-back name)
   (if (or come-back (not (string? *shell*)) (string-null? *shell*))
       (begin
	  (trace cc "system: " cmd #\Newline)
	  (let ((res (system cmd)))
	     (if (not (=fx res 0))
		 (compiler-exit 1)
		 res)))
       (begin
	  (trace cc "system/kill: " cmd #\Newline)
	  (system/kill cmd))))

;*---------------------------------------------------------------------*/
;*    system/kill ...                                                  */
;*---------------------------------------------------------------------*/
(define (system/kill cmd)
   (if (string=? (os-class) "mingw")
       (cond-expand
	  (bigloo-c (print (system cmd)))
	  (else (error "system/kill"
		       "This system is not implemented for 'mingw'"
		       #unspecified)))
       (begin
	  (cond-expand
	     (bigloo-jvm (system cmd))
	     (bigloo-.net (system cmd))
	     (bigloo-c (print (execl *shell* *shell* "-c" cmd NULL)))
	     (else (error "system/kill"
			  "System not implemented on the current architecture"
			  #unspecified)))
	  (internal-error "system/kill" "Can't execute cmd" cmd))))
 
   


