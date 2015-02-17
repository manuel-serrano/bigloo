;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/process.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Sun Feb 15 09:13:19 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV process callback                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_process

   (include "uv.sch")

   (import __libuv_types
	   __libuv_loop
	   __libuv_net)
   
   (export (uv-process-spawn::int ::UvProcess ::UvProcessOptions
	      #!key callback (loop (uv-default-loop)))
	   (uv-process-kill::int ::UvProcess ::int)
	   (uv-kill::int ::int ::int)
	   (uv-process-options-stdio-container-set!
	      ::UvProcessOptions ::int)
	   (uv-process-options-stdio-container-flags-set!
	      ::UvProcessOptions ::int ::int)
	   (uv-process-options-stdio-container-stream-set!
	      ::UvProcessOptions ::int ::UvHandle)
	   (uv-process-options-stdio-container-fd-set!
	      ::UvProcessOptions ::int ::obj)
	   
	   (inline UV-PROCESS-SETUID)
	   (inline UV-PROCESS-SETGID)
	   (inline UV-PROCESS-DETACHED)
	   (inline UV-PROCESS-WINDOWS-VERBATIM-ARGUMENTS)
	   (inline UV-PROCESS-WINDOWS-HIDE)

	   (inline UV-IGNORE)
	   (inline UV-INHERIT-FD)
	   (inline UV-INHERIT-STREAM)
	   (inline UV-CREATE-PIPE)
	   (inline UV-READABLE-PIPE)
	   (inline UV-WRITABLE-PIPE)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvProcess ...                                         */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvProcess)
   (with-access::UvProcess o ($builtin)
      (set! $builtin ($uv-handle-t ($uv-process-new o))))
   #f)

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvProcessOptions ...                                  */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvProcessOptions)
   #f)

;*---------------------------------------------------------------------*/
;*    uv-process-spawn ...                                             */
;*---------------------------------------------------------------------*/
(define (uv-process-spawn o options #!key callback (loop (uv-default-loop)))
   ($uv-process-spawn loop o options callback))

;*---------------------------------------------------------------------*/
;*    uv-process-kill ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-process-kill proc sig)
   (with-access::UvProcess proc ($builtin)
      ($uv-process-kill ($uv-process-t $builtin) sig)))

;*---------------------------------------------------------------------*/
;*    uv-kill ...                                                      */
;*---------------------------------------------------------------------*/
(define (uv-kill pid sig)
   ($uv-kill pid sig))

;*---------------------------------------------------------------------*/
;*    uv-process-options-stdio-container-set! ...                      */
;*---------------------------------------------------------------------*/
(define (uv-process-options-stdio-container-set! o::UvProcessOptions len)
   ($uv-process-options-stdio-container-set!
      (with-access::UvProcessOptions o ($builtin) $builtin) len))

;*---------------------------------------------------------------------*/
;*    uv-process-options-stdio-container-stream-set! ...               */
;*---------------------------------------------------------------------*/
(define (uv-process-options-stdio-container-stream-set! o::UvProcessOptions i hdl::UvHandle)
   ($uv-process-options-stdio-container-stream-set!
      (with-access::UvProcessOptions o ($builtin) $builtin)
      i
      (with-access::UvHandle hdl ($builtin) ($uv-stream-t $builtin))))
	 
;*---------------------------------------------------------------------*/
;*    uv-process-options-stdio-container-fd-set! ...                   */
;*---------------------------------------------------------------------*/
(define (uv-process-options-stdio-container-fd-set! o::UvProcessOptions i hdl::obj)
   ($uv-process-options-stdio-container-fd-set!
      (with-access::UvProcessOptions o ($builtin) $builtin)
      i
      (cond
	 ((isa? hdl UvFile) (with-access::UvFile hdl (fd) fd))
	 ((isa? hdl UvTty) (with-access::UvTty hdl (fd) fd))
	 ((isa? hdl UvStream) (uv-stream-fd hdl))
	 (else (bigloo-type-error "uv-process-options-stdio-container-fd-set!"
		  "UvFile/UvTty/UvStream" hdl)))))
	 
;*---------------------------------------------------------------------*/
;*    uv-process-options-stdio-container-flags-set! ...                */
;*---------------------------------------------------------------------*/
(define (uv-process-options-stdio-container-flags-set! o::UvProcessOptions i flags::int)
   ($uv-process-options-stdio-container-flags-set!
      (with-access::UvProcessOptions o ($builtin) $builtin) i flags))
   
;*---------------------------------------------------------------------*/
;*    UV-PROCESS-SETUID ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (UV-PROCESS-SETUID)
   $UV_PROCESS_SETUID)

;*---------------------------------------------------------------------*/
;*    UV-PROCESS-SETGID ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (UV-PROCESS-SETGID)
   $UV_PROCESS_SETGID)

;*---------------------------------------------------------------------*/
;*    UV-PROCESS-DETACHED ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (UV-PROCESS-DETACHED)
   $UV_PROCESS_DETACHED)

;*---------------------------------------------------------------------*/
;*    UV-PROCESS-WINDOWS-VERBATIM-ARGUMENTS ...                        */
;*---------------------------------------------------------------------*/
(define-inline (UV-PROCESS-WINDOWS-VERBATIM-ARGUMENTS)
   $UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS)

;*---------------------------------------------------------------------*/
;*    UV-PROCESS-WINDOWS-HIDE ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (UV-PROCESS-WINDOWS-HIDE)
   $UV_PROCESS_WINDOWS_HIDE)

;*---------------------------------------------------------------------*/
;*    UV-IGNORE ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (UV-IGNORE)
   $UV_IGNORE)

;*---------------------------------------------------------------------*/
;*    UV-INHERIT-FD ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (UV-INHERIT-FD)
   $UV_INHERIT-FD)

;*---------------------------------------------------------------------*/
;*    UV-INHERIT-STREAM ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (UV-INHERIT-STREAM)
   $UV_INHERIT-STREAM)

;*---------------------------------------------------------------------*/
;*    UV-CREATE-PIPE ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (UV-CREATE-PIPE)
   $UV_CREATE_PIPE)

;*---------------------------------------------------------------------*/
;*    UV-READABLE-PIPE ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (UV-READABLE-PIPE)
   $UV_READABLE_PIPE)

;*---------------------------------------------------------------------*/
;*    UV-WRITABLE-PIPE ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (UV-WRITABLE-PIPE)
   $UV_WRITABLE_PIPE)
