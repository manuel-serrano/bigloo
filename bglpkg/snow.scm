;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/snow.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 12 15:24:25 2007                          */
;*    Last change :  Thu Mar  1 13:20:17 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Snow plugin for bglpkg                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_snow

   (library pkglib
	    web)

   (import  bglpkg_param
	    bglpkg_utils)
   
   (export  (snow-plugin ::bstring ::bstring ::bstring ::bstring ::obj)
	    (snow-preprocessor ::bstring)))

;*---------------------------------------------------------------------*/
;*    snow-plugin ...                                                  */
;*    -------------------------------------------------------------    */
;*    The plugin to install Snow's snowballs                           */
;*---------------------------------------------------------------------*/
(define (snow-plugin tmpdir name version path args)
   (let* ((snowball (string-append name "-v"
				   (string-replace version #\. #\_)
				   ".tgz"))
	  (cache-dir (make-file-path tmpdir name "cache"))
	  (cache-name (make-file-name cache-dir snowball)))
      (unless (file-exists? cache-name)
	 ;; download the snowball file if not present in the cache
	 (make-directories cache-dir)
	 (download (car args) cache-name))
      ;; untar the source file
      (tarball-untar-gz cache-name tmpdir snowball)
      (rename-file (make-file-path tmpdir
				   name
				   (string-append "v" version)
				   "snow"
				   (string-append name ".scm"))
		   (make-file-path tmpdir
				   name
				   (string-append name ".scm")))
      (rm-rf (make-file-path tmpdir name (string-append "v" version)))))

;*---------------------------------------------------------------------*/
;*    snow-preprocessor ...                                            */
;*---------------------------------------------------------------------*/
(define (snow-preprocessor body)
   body)
