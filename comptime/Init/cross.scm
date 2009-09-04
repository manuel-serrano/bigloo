;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Init/cross.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Fri Sep  4 08:39:02 2009                          */
;*    Last change :  Fri Sep  4 08:40:23 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Processes the cross-compilation parameter.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module init_cross
   (import engine_param
	   module_eval)
   (export (process-cross-parameter param)))

;*---------------------------------------------------------------------*/
;*    safe-read ...                                                    */
;*---------------------------------------------------------------------*/
(define (safe-read p type f)
   
   (define (correct-type? obj)
      (case type
	 ((string) (string? obj))
	 ((pair) (pair? obj))
	 (else (error 'cross-compilation "Internal Error" type))))
   
   (let ((tmp (read p)))
      (unless (correct-type? tmp)
	 (error 'cross-compilation "Bad bigloo_cross.sch file" f))
      tmp))

;*---------------------------------------------------------------------*/
;*    process-cross-version ...                                        */
;*---------------------------------------------------------------------*/
(define (process-cross-version p f)
   (let* ((cross-version (safe-read p 'string f))
	  (cross-specific-version (safe-read p 'string f)))
      (when (not (and (string=? *bigloo-version* cross-version)
		      (string=? *bigloo-specific-version*
				cross-specific-version)))
	 ;; Replace with error?
	 (warning "Cross compilation for different Bigloos is risky."
		  "You have been warned."))))

;*---------------------------------------------------------------------*/
;*    process-cross-config ...                                         */
;*---------------------------------------------------------------------*/
(define (process-cross-config p f)
   ;; read-config is a back-quoted list. -> we need to eval it.
   (let* ((read-config (safe-read p 'pair f))
	  (cross-config (eval read-config)))
      ;; override the existing config entries
      (for-each (lambda (c) (bigloo-configuration-add-entry! (car c) (cdr c)))
		cross-config)
      (reinitialize-bigloo-variables!)))

;*---------------------------------------------------------------------*/
;*    read-cross_sch ...                                               */
;*---------------------------------------------------------------------*/
(define (read-cross_sch f)
   (let ((port (open-input-file f)))
      (unwind-protect
	 (begin
	    (process-cross-version port f)
	    (process-cross-config port f))
	 ;; TODO: we might want to determine the cross-prefix out of 'f' for
	 ;;   -L (*lib-dir*) and -I
	 (close-input-port port))))

;*---------------------------------------------------------------------*/
;*    process-cross-parameter ...                                      */
;*---------------------------------------------------------------------*/
(define (process-cross-parameter param)
   (let ((dir (file-name-canonicalize! param)))
      (when (not (directory? dir))
	 (error 'cross-compilation  "Not a directory" dir))
      (let ((cross_sch (make-file-path dir "bigloo_cross.sch")))
	 (when (not (file-exists? cross_sch))
	    (error 'cross-compilation "File not found" cross_sch))
	 (read-cross_sch cross_sch)
	 (set! *lib-dir* (cons dir *lib-dir*)))))
