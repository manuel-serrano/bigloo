;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Init/main.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 11:47:28 1995                          */
;*    Last change :  Thu Aug 10 21:46:46 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The main function of Bigloo debugger.                            */
;*    -------------------------------------------------------------    */
;*    This is a brand new implementation of Bdb. We have made          */
;*    drastic simplifications in order to get the debugger as small    */
;*    as possible. For instance we have try our best to remove the     */
;*    need for the debuggee to be statically linked. The goad of       */
;*    this new version of the debugger is the ability to debug the     */
;*    compiler itself.                                                 */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module init_bdb
   (import init_setrc
	   init_parse-args
	   command_all-cmd
	   engine_engine
	   engine_repl)
   (main   main)) 

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (if (not (member "-q" argv))
       (setup-default-values))
   (if (parse-args argv)
       (engine bdb-repl)
       -1))
