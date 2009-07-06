;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Init/main.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 09:51:40 1993                          */
;*    Last change :  Fri Nov  5 14:57:52 2004 (serrano)                */
;*    Copyright   :  1992-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The main entry point.                                            */
;*=====================================================================*/
  
;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module init_main
   (main main)
   (import init_setrc
	   init_parse-args
	   engine_engine
	   tools_trace)
   (export (compiler-exit value)))
 
;*---------------------------------------------------------------------*/
;*    *compiler-exit* ...                                              */
;*    -------------------------------------------------------------    */
;*    The default value of this variable is some kind of suicide       */
;*    value, an absolute emergency exit.                               */
;*---------------------------------------------------------------------*/
(define *compiler-exit* (lambda (x) (exit x)))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; Pour le moment il n'y a pas de error handler. Si une
   ;; erreur survient avant l'installation de celui-ci, on
   ;; quit bigloo
   (if (not (member "-q" argv)) (setup-default-values))
   (bind-exit (exit)
      (set! *compiler-exit* exit)
      (unwind-protect
	 (if (profile args (parse-args argv))
	     (engine)
	     -1)
	 (stop-trace))))

;*---------------------------------------------------------------------*/
;*    compiler-exit ...                                                */
;*---------------------------------------------------------------------*/
(define (compiler-exit value)
   (*compiler-exit* value))

		       
