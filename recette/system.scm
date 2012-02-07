;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/system.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 17 07:53:29 2002                          */
;*    Last change :  Mon Feb  6 16:00:08 2012 (serrano)                */
;*    Copyright   :  2002-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Test system features                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module system
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-system)))

;*---------------------------------------------------------------------*/
;*    fname ...                                                        */
;*---------------------------------------------------------------------*/
(define (fname s)
   (string-replace s #\/ (file-separator)))

;*---------------------------------------------------------------------*/
;*    test-system ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-system)
   (test-module "system" "system.scm")
   (test "pwd" (string? (pwd)) #t)
   (test "command-line" (pair? (command-line)) #t)
   (test "executable-name" (string? (executable-name)) #t)
   (test "file-separator" (char? (file-separator)) #t)
   (test "path-separator" (char? (path-separator)) #t)
   (test "make-file-name.1" (make-file-name "" "foo")
	 (string-append (string (file-separator)) "foo"))
   (test "make-file-name.2" (make-file-name "foo" "bar")
	 (string-append "foo" (string (file-separator)) "bar"))
   (test "make-file-path.3" (make-file-path "foo" "bar")
	 (string-append "foo" (string (file-separator)) "bar"))
   (test "make-file-path.4" (make-file-path "foo" "bar" "gee")
	 (string-append "foo" (string (file-separator))
			"bar" (string (file-separator))
			"gee"))
   (test "basename" (basename "foo") "foo")
   (test "basename" (basename (make-file-name "foo" "bar")) "bar")
   (test "basename" (basename (make-file-path "foo" "bar" "gee")) "gee")
   (test "dirname" (dirname "foo") ".")
   (test "dirname" (dirname "") ".")
   (test "dirname" (dirname (make-file-name "foo" "bar")) "foo")
   (test "dirname" (dirname (make-file-path "foo" "bar" "gee"))
	 (make-file-name "foo" "bar"))
   (test "prefix" (prefix "foo.scm") "foo")
   (test "prefix" (prefix (make-file-name "bar" "foo.scm"))
	 (make-file-name "bar" "foo"))
   (test "suffix" (suffix "foo.scm") "scm")
   (test "suffix" (suffix (make-file-name "bar" "foo.scm")) "scm")
   (test "os-class" (string? (os-class)) #t)
   (test "os-name" (string? (os-name)) #t)
   (test "os-version" (string? (os-version)) #t)
   (test "sleep" (sleep 15) 15)
   (test "file-modification-time" (elong? (file-modification-time "main.scm"))
	 #t)
   (test "unix-path->list" (list? (unix-path->list
				   (string-append "foo"
						  (string (path-separator))
						  "bar")))
	 #t)
   (test "runtime-os-name" (string? (car (list (os-name)))) #t)
   (test "runtime-os-version" (string? (car (list (os-version)))) #t)
   (test "socket" (string?
		   (with-output-to-string
		      (lambda () (display (make-server-socket 1966)))))
	 #t)
   (test "file-name-canonicalize.1" (file-name-canonicalize (fname "/tmp/bin")) (fname "/tmp/bin"))
   (test "file-name-canonicalize.2" (file-name-canonicalize! (fname "/tmp/bin")) (fname "/tmp/bin"))
   (test "file-name-canonicalize.3" (file-name-canonicalize (fname "/tmp/bin/")) (fname "/tmp/bin/"))
   (test "file-name-canonicalize.4" (file-name-canonicalize! (string-copy (fname "/tmp/bin/"))) (fname "/tmp/bin/"))
   (test "file-name-canonicalize.5" (file-name-canonicalize (fname "/tmp//bin/")) (fname "/tmp/bin/"))
   (test "file-name-canonicalize!.6" (let ((s (fname "/tmp//bin/")))
				       (file-name-canonicalize! (string-copy s)))
	 (fname "/tmp/bin/"))
   (test "file-name-canonicalize.7" (file-name-canonicalize (fname "/tmp/../bin/")) (fname "/bin/"))
   (test "file-name-canonicalize!.8" (let ((s (fname "/tmp/../bin/")))
				       (file-name-canonicalize! (string-copy (fname "/tmp/../bin/"))))
	 (fname "/bin/"))
   (test "file-name-canonicalize.9" (file-name-canonicalize (fname "/tmp/abc/../../bin/")) (fname "/bin/"))
   (test "file-name-canonicalize!.10" (let ((s (fname "/tmp/../bin/")))
				       (file-name-canonicalize! (string-copy (fname "/tmp/abc/../../bin/"))))
	 (fname "/bin/"))
   (test "file-name-canonicalize.11" (file-name-canonicalize (fname "/foo/bar/.")) (fname "/foo/bar"))
   (test "file-name-canonicalize.12" (file-name-canonicalize (fname "/../foo/bar")) (fname "/foo/bar"))
   (test "file-name-canonicalize.13" (file-name-canonicalize (fname "/foo/./bar")) (fname "/foo/bar"))
   (test "file-name-canonicalize.14" (file-name-canonicalize (fname "/foo/././bar")) (fname "/foo/bar"))
   (test "file-name-canonicalize.15" (file-name-canonicalize (fname "/foo/././////bar")) (fname "/foo/bar"))
   (test "file-name-canonicalize.16" (file-name-canonicalize (fname "/foo/././////../bar")) (fname "/bar"))
   (test "file-name-canonicalize.17" (file-name-canonicalize (fname "/foo/././////../////.//bar")) (fname "/bar"))
   (test "file-name-canonicalize.18" (file-name-canonicalize (fname "/foo/././////../////.//bar/./././.")) (fname "/bar"))
   (test "file-name-canonicalize.19" (file-name-canonicalize (fname "/foo/bar/..")) (fname "/foo"))
   (test "file-name-canonicalize.19" (file-name-canonicalize (fname "/..")) (fname "/"))
   (test "file-name-canonicalize.20" (file-name-canonicalize (fname "/foo/.bar/gee")) (fname "/foo/.bar/gee"))
   (test "file-name-canonicalize.21" (file-name-canonicalize (fname "./foo")) (fname "foo"))
   (test "file-name-canonicalize.22" (file-name-canonicalize (fname ".")) (fname "."))
   (test "file-name-canonicalize.23" (file-name-canonicalize "") "")
   (test "file-name-canonicalize.24" (file-name-canonicalize ".") ".")
   (test "file-name-unix-canonicalize.1" (file-name-unix-canonicalize "./") ".")
   (test "file-name-unix-canonicalize.2" (file-name-unix-canonicalize ".//") "")
   (test "file-name-unix-canonicalize.3" (file-name-unix-canonicalize "././") ".")
   (test "file-name-unix-canonicalize.4" (file-name-unix-canonicalize ".//a") "a")
   (test "file-name-unix-canonicalize.5" (file-name-unix-canonicalize "test/..") "")
   (test "file-name-unix-canonicalize.6" (file-name-unix-canonicalize "./test/..") "")
   (test "file-name-unix-canonicalize.7" (file-name-unix-canonicalize "../test/bar/..") "../test")
   (test "file-name-unix-canonicalize.8" (file-name-unix-canonicalize "../test/foo") "../test/foo")
   (test "file-name-unix-canonicalize.9" (file-name-unix-canonicalize "../../test/foo") "../../test/foo")
   (test "file-name-unix-canonicalize.9" (file-name-unix-canonicalize "..test/foo") "..test/foo"))
