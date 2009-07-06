;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Gdb/autoconf.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug  9 06:39:24 2000                          */
;*    Last change :  Tue Nov 13 12:04:53 2001 (serrano)                */
;*    Copyright   :  2000-01 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Gdb autoconfiguration. Basically, we fetch the Gdb version       */
;*    number.                                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gdb_autoconf
   (import engine_param)
   (export (gdb-version::bstring)))
 
;*---------------------------------------------------------------------*/
;*    gdb-version ...                                                  */
;*    -------------------------------------------------------------    */
;*    Here, we suppose that [gdb -version] produces something like:    */
;*      GNU gdb 4.18                                                   */
;*      Copyright 1998 Free Software Foundation, Inc.                  */
;*      ...                                                            */
;*---------------------------------------------------------------------*/
(define (gdb-version)
   (call-with-input-file "| gdb -version"
      (regular-grammar ()
	 ((: "GNU gdb " (submatch (+ (out #\space))))
	  (the-submatch 1)))))
