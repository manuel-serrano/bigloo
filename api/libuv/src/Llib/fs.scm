;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/api/libuv/src/Llib/fs.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 10 11:28:07 2014                          */
;*    Last change :  Mon Apr 10 05:51:27 2023 (serrano)                */
;*    Copyright   :  2014-23 Manuel Serrano                            */
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
   
   (export  (inline UV_FS_COPYFILE_EXCL::int)
	    (inline UV_FS_COPYFILE_FICLONE::int)
	    (inline UV_FS_COPYFILE_FICLONE_FORCE::int)

	    (inline uv-fs-stat-cb-vector-props::vector)
            (inline uv-fs-dup::int ::int)
	    
	    (inline uv-fs-rename::int ::bstring ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-ftruncate::int ::UvFile ::int64
	       #!key callback (loop (uv-default-loop)))
	    (uv-fs-truncate::int ::bstring ::long
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-chown::int ::bstring ::int ::int
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-fchown::int ::UvFile ::int ::int
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-lchown::int ::bstring ::int ::int
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-chmod::int ::bstring ::int
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-fchmod::int ::UvFile ::int
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-fstat ::UvFile
	       #!key callback vector (loop (uv-default-loop)))
	    (inline uv-fs-stat ::bstring
	       #!key callback vector (loop (uv-default-loop)))
	    (inline uv-fs-lstat ::bstring
	       #!key callback vector (loop (uv-default-loop)))
	    (inline uv-fs-link::int ::bstring ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-symlink::int ::bstring ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-readlink::obj ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-unlink::obj ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-rmdir::obj ::bstring
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-mkdir::obj ::bstring ::int
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-futime ::UvFile ::double ::double
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-utime ::bstring ::double ::double
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-fsync ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-fdatasync ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-open ::bstring ::obj
	       #!key (mode #o666) callback (loop (uv-default-loop)))
	    (inline uv-fs-open4 ::bstring ::obj
	       #!key (mode #o666) callback arg0 arg1 arg2 arg3
	       (loop (uv-default-loop)))
	    (inline uv-fs-close ::UvFile
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-close2 ::UvFile
	       #!key callback arg0 arg1 (loop (uv-default-loop)))
	    (inline uv-fs-copyfile ::bstring ::bstring ::int
	       #!key callback (loop (uv-default-loop)))
	    (inline uv-fs-write ::UvFile ::bstring ::int 
	       #!key
	       callback (offset 0) (position -1)
	       (loop::UvLoop (uv-default-loop)))
	    (inline uv-fs-write2 ::UvFile ::bstring ::int 
	       #!key
	       callback (offset 0) (position -1) arg0 arg1
	       (loop::UvLoop (uv-default-loop)))
	    (inline uv-fs-write3 ::UvFile ::bstring ::int 
	       #!key
	       callback (offset 0) (position -1) arg0 arg1 arg2
	       (loop::UvLoop (uv-default-loop)))
	    (inline uv-fs-read ::UvFile ::bstring ::int 
	       #!key
	       callback (offset 0) (position -1)
	       (loop::UvLoop (uv-default-loop)))
	    (inline uv-fs-read2 ::UvFile ::bstring ::int 
	       #!key
	       callback (offset 0) (position -1) arg0 arg1
	       (loop::UvLoop (uv-default-loop)))
	    (inline uv-fs-read3 ::UvFile ::bstring ::int 
	       #!key
	       callback (offset 0) (position -1) arg0 arg1 arg2
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

(define-inline (UV_FS_COPYFILE_EXCL)
   (cond-expand (bigloo-c (pragma::long "UV_FS_COPYFILE_EXCL")) (else 0)))
(define-inline (UV_FS_COPYFILE_FICLONE)
   (cond-expand (bigloo-c (pragma::long "UV_FS_COPYFILE_FICLONE")) (else 0)))
(define-inline (UV_FS_COPYFILE_FICLONE_FORCE)
   (cond-expand (bigloo-c (pragma::long "UV_FS_COPYFILE_FICLONE_FORCE")) (else 0)))
 
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
;*    uv-fs-stat-cb-vector-props ...                                   */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-stat-cb-vector-props)
   '#("ctime" "mtime" "atime" "birthtime"
      "gen" "flags" "blocks" "blksize" "size"
      "ino" "rdev" "gid" "uid" "nlink"
      "mode" "dev"
      "ctime-ns" "mtime-ns" "atime-ns" "birthtime-ns"))

;*---------------------------------------------------------------------*/
;*    uv-fs-dup ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-dup old)
   ($dup old))

;*---------------------------------------------------------------------*/
;*    uv-fs-rename ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-rename old new #!key callback (loop (uv-default-loop)))
   ($uv-fs-rename old new callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-ftruncate ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-ftruncate file offset #!key callback (loop (uv-default-loop)))
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
(define-inline (uv-fs-chown path uid gid #!key callback (loop (uv-default-loop)))
   ($uv-fs-chown path uid gid callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fchown ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-fchown fd uid gid #!key callback (loop (uv-default-loop)))
   ($uv-fs-fchown fd uid gid callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-lchown ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-lchown path uid gid #!key callback (loop (uv-default-loop)))
   ($uv-fs-lchown path uid gid callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-chmod ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-chmod path mode #!key callback (loop (uv-default-loop)))
   ($uv-fs-chmod path mode callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fchmod ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-fchmod fd mode #!key callback (loop (uv-default-loop)))
   ($uv-fs-fchmod fd mode callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-link ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-link old new #!key callback (loop (uv-default-loop)))
   ($uv-fs-link old new callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-symlink ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-symlink old new #!key callback (loop (uv-default-loop)))
   ($uv-fs-symlink old new callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-readlink ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-readlink path #!key callback (loop (uv-default-loop)))
   ($uv-fs-readlink path callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-unlink ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-unlink path #!key callback (loop (uv-default-loop)))
   ($uv-fs-unlink path callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-rmdir ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-rmdir path #!key callback (loop (uv-default-loop)))
   ($uv-fs-rmdir path callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-mkdir ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-mkdir path mod #!key callback (loop (uv-default-loop)))
   ($uv-fs-mkdir path mod callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fsync ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-fsync fd::UvFile #!key callback (loop (uv-default-loop)))
   ($uv-fs-fsync fd callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fdatasync ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-fdatasync fd::UvFile #!key callback (loop (uv-default-loop)))
   ($uv-fs-fdatasync fd callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-futime ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-futime fd::UvFile atime::double mtime::double
	   #!key callback (loop (uv-default-loop)))
   ($uv-fs-futime fd atime mtime callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-utime ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-utime path::bstring atime::double mtime::double
	   #!key callback (loop (uv-default-loop)))
   ($uv-fs-utime path atime mtime callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-open ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-open path flags #!key (mode #o666) callback (loop (uv-default-loop)))
   (let ((f (cond
	       ((integer? flags) flags)
	       ((symbol? flags) (uv-fs-flags flags))
	       ((string? flags) (uv-fs-flags (string->symbol flags)))
	       (else (error "uv-fs-open" "Wrong flags" flags)))))
      ($uv-fs-open path f mode callback loop)))

;*---------------------------------------------------------------------*/
;*    uv-fs-open4 ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-open4 path flags #!key (mode #o666) callback arg0 arg1 arg2 arg3 (loop (uv-default-loop)))
   (let ((f (cond
	       ((integer? flags) flags)
	       ((symbol? flags) (uv-fs-flags flags))
	       ((string? flags) (uv-fs-flags (string->symbol flags)))
	       (else (error "uv-fs-open" "Wrong flags" flags)))))
      ($uv-fs-open4 path f mode callback arg0 arg1 arg2 arg3 loop)))

;*---------------------------------------------------------------------*/
;*    uv-fs-close ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-close fd::UvFile #!key callback (loop (uv-default-loop)))
   ($uv-fs-close fd callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-close2 ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-close2 fd::UvFile #!key callback arg0 arg1 (loop (uv-default-loop)))
   ($uv-fs-close2 fd callback arg0 arg1 loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-copyfile ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-copyfile path::bstring newpath::bstring flags::int
	   #!key callback (loop (uv-default-loop)))
   ($uv-fs-copyfile path newpath flags callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-fstat ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-fstat fd::UvFile #!key callback vector (loop (uv-default-loop)))
   ($uv-fs-fstat fd callback vector loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-lstat ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-lstat path::bstring #!key callback vector (loop (uv-default-loop)))
   ($uv-fs-lstat path callback vector loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-stat ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-stat path::bstring #!key callback vector (loop (uv-default-loop)))
   ($uv-fs-stat path callback vector loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-write ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-write fd buffer length 
	   #!key callback (offset 0) (position -1)
	   (loop::UvLoop (uv-default-loop)))
   ($uv-fs-write fd buffer offset length position callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-write2 ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-write2 fd buffer length 
	   #!key callback (offset 0) (position -1) arg0 arg1
	   (loop::UvLoop (uv-default-loop)))
   ($uv-fs-write2 fd buffer offset length position callback arg0 arg1 loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-write3 ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-write3 fd buffer length 
	   #!key callback (offset 0) (position -1) arg0 arg1 arg2
	   (loop::UvLoop (uv-default-loop)))
   ($uv-fs-write3 fd buffer offset length position callback arg0 arg1 arg2 loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-read ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-read fd buffer length 
	   #!key callback (offset 0) (position -1)
	   (loop::UvLoop (uv-default-loop)))
   ($uv-fs-read fd buffer offset length position callback loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-read2 ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-read2 fd buffer length 
	   #!key callback (offset 0) (position -1) arg0 arg1
	   (loop::UvLoop (uv-default-loop)))
   ($uv-fs-read2 fd buffer offset length position callback arg0 arg1 loop))

;*---------------------------------------------------------------------*/
;*    uv-fs-read3 ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-read3 fd buffer length 
	   #!key callback (offset 0) (position -1) arg0 arg1 arg2
	   (loop::UvLoop (uv-default-loop)))
   ($uv-fs-read3 fd buffer offset length position callback arg0 arg1 arg2 loop))

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
