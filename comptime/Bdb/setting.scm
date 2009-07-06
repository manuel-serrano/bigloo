;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Bdb/setting.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr  9 14:07:32 1998                          */
;*    Last change :  Wed Nov 16 12:24:56 2005 (serrano)                */
;*    Copyright   :  1992-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The bdb setting setting.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bdb_setting
   (import engine_param
	   tools_shape
	   tools_error
	   tools_misc)
   (export (bdb-setting!)
	   *bdb-module*
	   *jvm-debug-module*))

;*---------------------------------------------------------------------*/
;*    *bdb-module* ...                                                 */
;*---------------------------------------------------------------------*/
(define *bdb-module* '(__bdb))
(define *jvm-debug-module* '(__pp_circle))

;*---------------------------------------------------------------------*/
;*    bdb-setting! ...                                                 */
;*    -------------------------------------------------------------    */
;*    The global options to be used when compilling for bdb            */
;*---------------------------------------------------------------------*/
(define (bdb-setting!)
   (set! *rm-tmp-files* #f)
   (set! *c-debug* #t)
   (set! *strip* #f)
   (set! *indent* #f)
   (set! *inlining?* #f)
   (library-translation-table-add! 'bdb "bigloobdb"))
