;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/link.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jan 15 11:16:02 1994                          */
;*    Last change :  Sun Jul 21 11:09:27 2013 (serrano)                */
;*    Copyright   :  1994-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    On link quand l'utilisateur n'a passe que des `.o'               */
;*    -------------------------------------------------------------    */
;*    Pour ce faire on essaye de trouver des `.scm' correspondants.    */
;*    On genere un petit fichier `.scm' qui les initialise puis on     */
;*    le compile normalement ou alors, on se contente d'invoquer le    */
;*    linker `*ld*'.                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module engine_link
   (export (link)
	   (unprof-src-name name)
	   (find-file-for-link ::bstring)
	   (find-src-file prefix bname)
	   (find-libraries ::pair-nil)
	   (find-main ::pair-nil)
	   (make-tmp-main ::bstring ::bool ::symbol ::pair-nil ::pair-nil))
   (import cc_ld
	   read_reader
	   backend_backend
	   engine_param
	   init_setrc
	   tools_error
	   tools_misc
	   module_module
	   expand_eps
	   expand_install
	   read_include))

;*---------------------------------------------------------------------*/
;*    link ...                                                         */
;*---------------------------------------------------------------------*/
(define (link)
   ;; register bigloo-compile srfi for library inclusion
   (register-eval-srfi! 'bigloo-compile)
   ;; we install macros for expanding module clauses
   (install-initial-expander)
   ;; we build the ad-hoc backend
   (set-backend! *target-language*)
   ;; we start by looking for the source files
   (let loop ((objects *o-files*)
	      (sources '()))
      (if (null? objects)
	  ;; and we launch the linking process
	  (backend-link-objects (the-backend) sources)
	  (let* ((object   (car objects))
		 (pref     (unprof-src-name (prefix object)))
		 (bpref    (basename pref))
		 (scm-file (find-src-file pref bpref)))
	     (if (string? scm-file)
		 (loop (cdr objects) (cons (cons scm-file object) sources))
		 (begin
		    (if (>=fx (bigloo-warning) 2)
			(warning  "link"
				  "No Bigloo module found for -- "
				  (car objects)))
		    (loop (cdr objects) sources)))))))

;*---------------------------------------------------------------------*/
;*    unprof-src-name ...                                              */
;*---------------------------------------------------------------------*/
(define (unprof-src-name name)
   (if (not *profile-mode*)
       name
       (let ((len (string-length name)))
	  (if (and (>fx len 2)
		   (char=? (string-ref name (-fx len 1)) #\p)
		   (char=? (string-ref name (-fx len 2)) #\_))
	      (substring name 0 (-fx len 2))
	      name))))

;*---------------------------------------------------------------------*/
;*    find-file-for-link ...                                           */
;*---------------------------------------------------------------------*/
(define (find-file-for-link file)
   (if (file-exists? file)
       file
       (find-file/path file *load-path*)))

;*---------------------------------------------------------------------*/
;*    find-src-file ...                                                */
;*---------------------------------------------------------------------*/
(define (find-src-file prefix bname)
   (let loop ((suffix *src-suffix*)
	      (files '()))
      (if (null? suffix)
	  (cond
	     ((null? files)
	      #f)
	     ((null? (cdr files))
	      (car files))
	     (else
	      (warning "link" "Several source files found for object `"
		       bname "'. Using file -- " (car files))
	      (car files)))
	  (let* ((suf (car suffix))
		 (f   (find-file-for-link (string-append prefix "." suf))))
	     (if (string? f)
		 (loop (cdr suffix) (cons f files))
		 (let ((f (find-file-for-link (string-append bname "." suf))))
		    (if (string? f)
			(loop (cdr suffix) (cons f files))
			(loop (cdr suffix) files))))))))

;*---------------------------------------------------------------------*/
;*    find-libraries ...                                               */
;*---------------------------------------------------------------------*/
(define (find-libraries clauses)
   (let loop ((clauses clauses)
	      (libraries '()))
      (match-case clauses
	 (()
	  (reverse! libraries))
	 (((and ?lib (library . ?libs)) . ?rest)
	  (loop rest (cons lib libraries)))
	 (((eval . ?evclauses) . ?rest)
	  (let ((evlibs (filter-map (lambda (clause)
				       (match-case clause
					  ((library . ?libs) `(eval ,clause))
					  (else #f)))
			     evclauses)))
	     (loop rest (append evlibs libraries))))
	 (((include . ?includes) . ?rest)
	  (let ((directives (append-map (lambda (include)
					   (read-directives include))
					includes)))
	     (loop (append directives rest) libraries)))
	 (((cond-expand . ?-) . ?rest)
	  (loop (list (comptime-expand/error (car clauses)))
		(loop rest libraries)))
	 (else
	  (loop (cdr clauses) libraries)))))

;*---------------------------------------------------------------------*/
;*    find-main ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-main clauses)
   (let loop ((clauses clauses))
      (match-case clauses
	 (()
	  #f)
	 (((main ?main) . ?rest)
	  main)
	 (((include . ?includes) . ?rest)
	  (or (find (lambda (include)
		       (find-main (read-directives include)))
		 includes)
	      (loop rest)))
	 (((cond-expand . ?-) . ?rest)
	  (or (find-main (list (comptime-expand/error (car clauses))))
	      (loop rest)))
	 (else
	  (loop (cdr clauses))))))

;*---------------------------------------------------------------------*/
;*    make-tmp-main ...                                                */
;*---------------------------------------------------------------------*/
(define (make-tmp-main file main module clauses libraries)
   (let ((pout (open-output-file file)))
      (if (not (output-port? pout))
	  (error "" "Can't open output file" file)
	  (begin
	     (fprint pout ";; " *bigloo-name*)
	     (fprint pout ";; !!! generated file, don't edit !!!")
	     (fprint pout ";; ==================================")
	     (newline pout)
	     (let ((mod `(module ,module
			    (import ,@(reverse clauses))
			    ,@libraries)))
		(fprint pout mod)
		(newline pout))
	     (when main
		(fprint pout "(main *the-command-line*)")
		(newline pout))
	     (close-output-port pout)))))

