;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Tools/io.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  2 14:12:14 1999                          */
;*    Last change :  Wed Aug  9 14:01:26 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The io handling.                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_io
   (import engine_param)
   (export (console-echo ::bstring)
	   (get-console-echo-function::procedure)
	   (console-error ::bstring)
	   (console-prompt ::bstring)
	   (console-source ::bstring)
	   (console-echo-edit ::bstring ::bstring)
	   (console-echo-explore ::bstring ::bstring)
	   (console-echo-dprint ::bstring ::bstring)
	   (console-echo-command ::bstring ::bstring ::procedure)
	   (console-flush)
	   (console-newline)
	   (console-output ::bstring)
	   (set-console-echo-function! ::procedure)
	   (set-console-echo-edit-function! ::procedure)
	   (set-console-echo-explore-function! ::procedure)
	   (set-console-echo-dprint-function! ::procedure)
	   (set-console-echo-command-function! ::procedure)
	   (set-console-flush-function! ::procedure)
	   (set-console-prompt-function! ::procedure)
	   (set-console-source-function! ::procedure)
	   (set-console-error-function! ::procedure)
	   (set-console-output-function! ::procedure)))

;*---------------------------------------------------------------------*/
;*    Global state variables                                           */
;*---------------------------------------------------------------------*/
(define *console-echo* console-tty-echo)
(define *console-echo-edit* (lambda (a _) (console-tty-echo a)))
(define *console-echo-explore* (lambda (a _) (console-tty-echo a)))
(define *console-echo-dprint* (lambda (a _) (console-tty-echo a)))
(define *console-echo-command* (lambda (a _ _) (console-tty-echo a)))
(define *console-error* console-tty-error)
(define *console-flush* console-tty-flush)
(define *console-prompt* console-tty-prompt)
(define *console-source* console-tty-source)
(define *console-output* console-tty-output)

;*---------------------------------------------------------------------*/
;*    set-console-echo-function! ...                                   */
;*---------------------------------------------------------------------*/
(define (set-console-echo-function! fun)
   (set! *console-echo* fun))

;*---------------------------------------------------------------------*/
;*    get-console-echo-function ...                                    */
;*---------------------------------------------------------------------*/
(define (get-console-echo-function)
   *console-echo*)

;*---------------------------------------------------------------------*/
;*    set-console-echo-edit-function! ...                              */
;*---------------------------------------------------------------------*/
(define (set-console-echo-edit-function! fun)
   (set! *console-echo-edit* fun))

;*---------------------------------------------------------------------*/
;*    set-console-echo-explore-function! ...                           */
;*---------------------------------------------------------------------*/
(define (set-console-echo-explore-function! fun)
   (set! *console-echo-explore* fun))

;*---------------------------------------------------------------------*/
;*    set-console-echo-dprint-function! ...                            */
;*---------------------------------------------------------------------*/
(define (set-console-echo-dprint-function! fun)
   (set! *console-echo-dprint* fun))

;*---------------------------------------------------------------------*/
;*    set-console-echo-command-function! ...                           */
;*---------------------------------------------------------------------*/
(define (set-console-echo-command-function! fun)
   (set! *console-echo-command* fun))

;*---------------------------------------------------------------------*/
;*    set-console-error-function! ...                                  */
;*---------------------------------------------------------------------*/
(define (set-console-error-function! fun)
   (set! *console-error* fun))

;*---------------------------------------------------------------------*/
;*    set-console-flush-function! ...                                  */
;*---------------------------------------------------------------------*/
(define (set-console-flush-function! fun)
   (set! *console-flush* fun))

;*---------------------------------------------------------------------*/
;*    set-console-prompt-function! ...                                 */
;*---------------------------------------------------------------------*/
(define (set-console-prompt-function! fun)
   (set! *console-prompt* fun))

;*---------------------------------------------------------------------*/
;*    set-console-source-function! ...                                 */
;*---------------------------------------------------------------------*/
(define (set-console-source-function! fun)
   (set! *console-source* fun))

;*---------------------------------------------------------------------*/
;*    set-console-output-function! ...                                 */
;*---------------------------------------------------------------------*/
(define (set-console-output-function! fun)
   (set! *console-output* fun))

;*---------------------------------------------------------------------*/
;*    console-tty-echo ...                                             */
;*---------------------------------------------------------------------*/
(define (console-tty-echo args)
   (display args *pout*))

;*---------------------------------------------------------------------*/
;*    console-tty-error ...                                            */
;*---------------------------------------------------------------------*/
(define (console-tty-error args)
   (display args *perr*))

;*---------------------------------------------------------------------*/
;*    console-tty-output ...                                           */
;*---------------------------------------------------------------------*/
(define (console-tty-output str)
   (let ((len (string-length str)))
      (if (>fx len 0)
	  (begin
	     (display str *pout*)
	     (if (not (char=? (string-ref str (-fx len 1)) #\Newline))
		 (newline *pout*))
	     (flush-output-port *pout*)))))
   
;*---------------------------------------------------------------------*/
;*    console-echo ...                                                 */
;*---------------------------------------------------------------------*/
(define (console-echo args)
   (*console-echo* args))

;*---------------------------------------------------------------------*/
;*    console-edit ...                                                 */
;*---------------------------------------------------------------------*/
(define (console-echo-edit arg1 arg2)
   (*console-echo-edit* arg1 arg2))

;*---------------------------------------------------------------------*/
;*    console-explore ...                                              */
;*---------------------------------------------------------------------*/
(define (console-echo-explore arg1 arg2)
   (*console-echo-explore* arg1 arg2))

;*---------------------------------------------------------------------*/
;*    console-dprint ...                                               */
;*---------------------------------------------------------------------*/
(define (console-echo-dprint arg1 arg2)
   (*console-echo-dprint* arg1 arg2))

;*---------------------------------------------------------------------*/
;*    console-command ...                                              */
;*---------------------------------------------------------------------*/
(define (console-echo-command arg1 arg2 arg3)
   (*console-echo-command* arg1 arg2 arg3))

;*---------------------------------------------------------------------*/
;*    console-error ...                                                */
;*---------------------------------------------------------------------*/
(define (console-error args)
   (*console-error* args))

;*---------------------------------------------------------------------*/
;*    console-newline ...                                              */
;*---------------------------------------------------------------------*/
(define (console-newline)
   (*console-echo* #"\n"))

;*---------------------------------------------------------------------*/
;*    console-tty-flush ...                                            */
;*---------------------------------------------------------------------*/
(define (console-tty-flush)
   (flush-output-port *pout*))

;*---------------------------------------------------------------------*/
;*    console-flush ...                                                */
;*---------------------------------------------------------------------*/
(define (console-flush)
   (*console-flush*))

;*---------------------------------------------------------------------*/
;*    console-tty-prompt ...                                           */
;*---------------------------------------------------------------------*/
(define (console-tty-prompt prompt)
   (display prompt *pout*))

;*---------------------------------------------------------------------*/
;*    console-prompt ...                                               */
;*---------------------------------------------------------------------*/
(define (console-prompt prompt)
   (*console-prompt* prompt))

;*---------------------------------------------------------------------*/
;*    console-tty-source ...                                           */
;*---------------------------------------------------------------------*/
(define (console-tty-source line)
   (display line *pout*)
   (newline *pout*))

;*---------------------------------------------------------------------*/
;*    console-source ...                                               */
;*---------------------------------------------------------------------*/
(define (console-source source)
   (*console-source* source))

;*---------------------------------------------------------------------*/
;*    console-output ...                                               */
;*---------------------------------------------------------------------*/
(define (console-output output)
   (*console-output* output))
