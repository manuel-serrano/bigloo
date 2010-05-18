;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/all-cmd.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 11:06:21 1999                          */
;*    Last change :  Sun Jul 30 09:04:32 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    A sort of dummy modules that imports all the module defining a   */
;*    BDB command.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_all-cmd
   (import command_help
	   command_quit
	   command_run
	   command_file
	   command_break
	   command_mangle
	   command_info
	   command_backtrace
	   command_print
	   command_display
	   command_explore
	   command_whatis
	   command_set))
