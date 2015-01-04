;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/pipe.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 10:29:52 2014                          */
;*    Last change :  Mon Dec 29 07:37:54 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV pipe                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_pipe
   
   (include "uv.sch")
   
   (import  __libuv_types
	    __libuv_loop)
   
   (export  (uv-pipe-open::int ::UvPipe ::int)
	    (uv-pipe-bind ::UvPipe ::bstring)
	    (uv-pipe-ipc?::bool ::UvPipe)
	    (uv-pipe-connect ::UvPipe ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-pipe-pending-instances ::UvPipe ::int)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvPipe ...                                            */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvPipe)
   (with-access::UvPipe o (($pipe $builtin) loop ipc)
      (with-access::UvLoop loop (($loop $builtin))
	 (set! $pipe ($uv-pipe-create ($uv-loop-t $loop) o ipc))
	 o)))

;*---------------------------------------------------------------------*/
;*    uv-pipe-open ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-pipe-open handle fd)
   (with-access::UvPipe handle ($builtin)
      ($uv-pipe-open ($uv-pipe-t $builtin) fd)))

;*---------------------------------------------------------------------*/
;*    uv-pipe-bind ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-pipe-bind handle name)
   (with-access::UvPipe handle ($builtin)
      ($uv-pipe-bind ($uv-pipe-t $builtin) name)))

;*---------------------------------------------------------------------*/
;*    uv-pipe-ipc? ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-pipe-ipc? handle)
   (with-access::UvPipe handle ($builtin)
      ($uv-pipe-ipc? ($uv-pipe-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-pipe-connect ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-pipe-connect handle name #!key callback (loop (uv-default-loop)))
   ($uv-pipe-connect handle name callback loop)
   #t)

;*---------------------------------------------------------------------*/
;*    uv-pipe-pending-instances ...                                    */
;*---------------------------------------------------------------------*/
(define (uv-pipe-pending-instances handle count)
   (with-access::UvPipe handle ($builtin)
      ($uv-pipe-pending-instances ($uv-pipe-t $builtin) count))
   #t)
   

