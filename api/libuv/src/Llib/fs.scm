;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/api/libuv/src/Llib/fs.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 10 11:28:07 2014                          */
;*    Last change :  Fri Jun 29 16:22:05 2018 (serrano)                */
;*    Copyright   :  2014-18 Manuel Serrano                            */
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

            (inline uv-fs-dup::int ::int)
	    (uv-fs-rename::int ::bstring ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-ftruncate::int ::UvFile ::int64
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-truncate::int ::bstring ::long
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-chown::int ::bstring ::int ::int
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-fchown::int ::UvFile ::int ::int
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-lchown::int ::bstring ::int ::int
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-chmod::int ::bstring ::int
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-fchmod::int ::UvFile ::int
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-fstat ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-stat ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-lstat ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-link::int ::bstring ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-symlink::int ::bstring ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-readlink::obj ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-unlink::obj ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-rmdir::obj ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-mkdir::obj ::bstring ::int
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-futime ::UvFile ::double ::double
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-utime ::bstring ::double ::double
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-fsync ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-fdatasync ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-open ::bstring ::obj
	       #!key (mode #o666) callback (loop (uv-default-loop)))
	    (uv-fs-close ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-write ::UvFile ::bstring ::int 
	       #!key
	       callback (offset 0) (position -1)
	       (loop::UvLoop (uv-default-loop)))
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
;*    uv-fs-dup ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-dup old)
   ($dup old))

;*---------------------------------------------------------------------*/
;*    uv-fs-rename ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-fs-rename old new #!key callback (loop (uv-default-loop)))
   ($uv-fs-rename old new callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-ftruncate ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-fs-ftruncate file offset #!key callback (loop (uv-default-loop)))
   ($uv-fs-ftruncate file offset callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-truncate ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-fs-truncate path offset #!key callback (loop (uv-default-loop)))
   (if (procedure? callback)
       (uv-fs-open path 'a
	  :callback
	  (lambda (fd)
	     (if (isa? fd UvFile)
		 (uv-fs-ftruncate fd (fixnum->int64 offset)
		    :callback
		    (lambda (res)
		       (uv-fs-close fd
			  :callback (lambda (obj) (callback res))
			  :loop loop))
		    :loop loop)
		 (callback fd)))
	  :loop loop)
       (let ((fd (uv-fs-open path 'a)))
	  (if (isa? fd UvFile)
	      (let ((res (uv-fs-ftruncate fd offset)))
		(uv-fs-close fd)
		res)
	      fd))))

;*---------------------------------------------------------------------*/
;*    uv-fs-chown ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-chown path uid gid #!key callback (loop (uv-default-loop)))
   ($uv-fs-chown path uid gid callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fchown ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-fs-fchown fd uid gid #!key callback (loop (uv-default-loop)))
   ($uv-fs-fchown fd uid gid callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-lchown ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-fs-lchown path uid gid #!key callback (loop (uv-default-loop)))
   ($uv-fs-lchown path uid gid callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-chmod ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-chmod path mode #!key callback (loop (uv-default-loop)))
   ($uv-fs-chmod path mode callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fchmod ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-fs-fchmod fd mode #!key callback (loop (uv-default-loop)))
   ($uv-fs-fchmod fd mode callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-link ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-fs-link old new #!key callback (loop (uv-default-loop)))
   ($uv-fs-link old new callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-symlink ...                                                */
;*---------------------------------------------------------------------*/
(define (uv-fs-symlink old new #!key callback (loop (uv-default-loop)))
   ($uv-fs-symlink old new callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-readlink ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-fs-readlink path #!key callback (loop (uv-default-loop)))
   ($uv-fs-readlink path callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-unlink ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-fs-unlink path #!key callback (loop (uv-default-loop)))
   ($uv-fs-unlink path callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-rmdir ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-rmdir path #!key callback (loop (uv-default-loop)))
   ($uv-fs-rmdir path callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-mkdir ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-mkdir path mod #!key callback (loop (uv-default-loop)))
   ($uv-fs-mkdir path mod callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fsync ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-fsync fd::UvFile #!key callback (loop (uv-default-loop)))
   ($uv-fs-fsync fd callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fdatasync ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-fs-fdatasync fd::UvFile #!key callback (loop (uv-default-loop)))
   ($uv-fs-fdatasync fd callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-futime ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-fs-futime fd::UvFile atime::double mtime::double
	   #!key callback (loop (uv-default-loop)))
   ($uv-fs-futime fd atime mtime callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-utime ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-utime path::bstring atime::double mtime::double
	   #!key callback (loop (uv-default-loop)))
   ($uv-fs-utime path atime mtime callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-open ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-fs-open path flags #!key (mode #o666) callback (loop (uv-default-loop)))
   (cond
      ((integer? flags)
       ($uv-fs-open path flags mode callback loop))
      ((symbol? flags)
       ($uv-fs-open path (uv-fs-flags flags) mode callback loop))
      ((string? flags)
       ($uv-fs-open path (uv-fs-flags (string->symbol flags)) mode callback loop))
      (else
       (error "uv-fs-open" "Wrong flags" flags))))

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
;*    uv-fs-lstat ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-lstat path::bstring #!key callback (loop (uv-default-loop)))
   ($uv-fs-lstat path callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-stat ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-fs-stat path::bstring #!key callback (loop (uv-default-loop)))
   ($uv-fs-stat path callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-write ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-fs-write fd buffer length 
	   #!key callback (offset 0) (position -1)
	   (loop::UvLoop (uv-default-loop)))
   ($uv-fs-write fd buffer offset length position callback loop))

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
