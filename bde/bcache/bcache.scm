;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/bde/bcache/bcache.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct 19 04:04:52 2025                          */
;*    Last change :  Sun Oct 19 04:31:02 2025 (serrano)                */
;*    Copyright   :  2025 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Dump a cached module                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bache
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (for-each (lambda (path)
		(let ((p (open-input-binary-file path)))
		   (unwind-protect
		      (dump-module (input-obj p))
		      (close-binary-port p))))
      (cdr argv)))

;*---------------------------------------------------------------------*/
;*    dump-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-module mod::Module)
   
   (define (dump-decl e)
      (let* ((k (car e))
	     (d::Decl (cdr e))
	     (f::Def (-> d def)))
	 (print "    " k)
	 (print "    alias: " (-> d alias))
	 (print "    mod: " (-> d mod))
	 (print "    scope: " (-> d scope))
	 (print "    def: " (typeof f) " " (-> f kind))
	 (print "    ronly: " (-> d ronly) " " (-> f ronly))))
   
   (define (dump-class k)
      (print "      " (car k) " " (cdr k)))
   
   (print (-> mod id) ":")
   (print "  path: " (-> mod path))
   (print "  checksum: " (-> mod checksum))
   (print "  exports:")
   (for-each dump-decl (-> mod exports))
   (print "  classes:")
   (for-each dump-class (-> mod classes))
   (print "  body: " (-> mod body)))
