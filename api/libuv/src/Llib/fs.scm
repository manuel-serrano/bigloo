;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/fs.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 10 11:28:07 2014                          */
;*    Last change :  Wed Jul 23 11:46:16 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV fs                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_fs
   
   (include "uv.sch")
   
   (import  __libuv_types
	    __libuv_loop)
   
   (export  #;(uv-open-input-file ::bstring #!key (bufinfo #t) callback)
	    
	    (uv-fs-rename::int ::bstring ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-open ::bstring ::obj
	       #!key (mode #o666) callback (loop (uv-default-loop)))
	    (uv-fs-close ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-fstat ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-read ::UvFile ::bstring ::int 
	       #!key
	       callback (offset 0) (position -1)
	       (loop::UvLoop (uv-default-loop)))
	    (uv-fs-flags::int ::symbol)))

;*---------------------------------------------------------------------*/
;*    Constants                                                        */
;*---------------------------------------------------------------------*/
(define O_RDONLY
   (cond-expand (bigloo-c (pragma::long "O_RDONLY")) (else 0)))
(define O_WRONLY
   (cond-expand (bigloo-c (pragma::long "O_WRONLY")) (else 1)))
(define O_RDWR
   (cond-expand (bigloo-c (pragma::long "O_RDWR")) (else 2)))
(define O_CREAT
   (cond-expand (bigloo-c (pragma::long "O_CREAT")) (else #o64)))
(define O_EXCL
   (cond-expand (bigloo-c (pragma::long "O_EXCL")) (else #o200)))
(define O_TRUNC
   (cond-expand (bigloo-c (pragma::long "O_TRUNC")) (else #o1000)))
(define O_APPEND
   (cond-expand (bigloo-c (pragma::long "O_APPEND")) (else #o2000)))
(define O_SYNC
   (cond-expand (bigloo-c (pragma::long "O_SYNC")) (else #o4010000)))


;* {*---------------------------------------------------------------------*} */
;* {*    uv-open-input-file ...                                           *} */
;* {*---------------------------------------------------------------------*} */
;* (define (uv-open-input-file name #!key (bufinfo #t) callback)       */
;*    (let ((buf (get-port-buffer "uv-open-input-file" bufinfo c-default-io-bufsiz))) */
;*       (cond                                                         */
;* 	 ((not callback)                                               */
;* 	  ($uv-open-input-file name buf callback))                     */
;* 	 ((not (procedure? callback))                                  */
;* 	  (error "uv-open-input-file" "wrong callback" callback))      */
;* 	 ((not (correct-arity? callback 1))                            */
;* 	  (error "uv-open-input-file" "wrong callback arity" callback)) */
;* 	 (else                                                         */
;* 	  ($uv-open-input-file name buf callback)))))                  */

;*---------------------------------------------------------------------*/
;*    uv-fs-rename ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-fs-rename old new #!key callback (loop (uv-default-loop)))
   ($uv-fs-rename old new callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-open ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-fs-open path flags #!key (mode #o666) callback (loop (uv-default-loop)))
   (cond
      ((integer? flags) ($uv-fs-open path flags mode callback loop))
      ((symbol? flags) ($uv-fs-open path (uv-fs-flags flags) mode callback loop))
      (else (error "uv-fs-open" "Wrong flags" flags))))

;*---------------------------------------------------------------------*/
;*    uv-fs-close ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-close fd::UvFile #!key callback (loop (uv-default-loop)))
   ($uv-fs-close fd callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fstat ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-fstat fd::UvFile #!key callback (loop (uv-default-loop)))
   ($uv-fs-fstat fd callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-read ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-fs-read fd buffer length 
	   #!key callback (offset 0) (position -1)
	   (loop::UvLoop (uv-default-loop)))
   ($uv-fs-read fd buffer offset length position callback loop))


;*---------------------------------------------------------------------*/
;*    uv-fs-flags ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-flags::int flag::symbol)
   (case flag
      ((r) O_RDONLY)
      ((r+) (bit-or O_RDONLY O_SYNC))
      ((rs+) (bit-or O_RDWR O_SYNC))
      ((w) (bit-or O_TRUNC (bit-or O_CREAT O_WRONLY)))
      ((wx xw) (bit-or O_TRUNC (bit-or O_CREAT (bit-or O_WRONLY O_EXCL))))
      ((w+) (bit-or O_TRUNC (bit-or O_CREAT O_RDWR)))
      ((xw+ wx+) (bit-or O_TRUNC (bit-or O_CREAT (bit-or O_RDWR O_EXCL))))
      ((a) (bit-or O_APPEND (bit-or O_CREAT O_WRONLY)))
      ((ax xa) (bit-or O_APPEND (bit-or O_CREAT (bit-or O_WRONLY O_EXCL))))
      ((a+) (bit-or O_APPEND (bit-or O_CREAT O_RDWR)))
      ((ax+ xa+) (bit-or O_APPEND (bit-or O_CREAT (bit-or O_RDWR O_EXCL))))
      (else (error "uv-fs-flags" "Illegal flag" flag))))
