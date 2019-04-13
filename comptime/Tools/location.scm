;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/location.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 10:00:44 1996                          */
;*    Last change :  Wed Feb 10 11:21:56 2016 (serrano)                */
;*    Copyright   :  1996-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The location managment                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module tools_location
   (include "Tools/location.sch")
   (import  engine_param)
   (export  (find-location ::obj)
	    (find-location/loc ::obj ::obj)
	    (location-full-fname::bstring ::obj)
	    (location-shape ::obj ::obj)
	    (dump-location ::obj ::obj)
	    (location-skip-forward ::obj ::int)))

;*---------------------------------------------------------------------*/
;*    *file-lines-table* ...                                           */
;*---------------------------------------------------------------------*/
(define *file-lines-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    get-file-lines ...                                               */
;*---------------------------------------------------------------------*/
(define (get-file-lines file)
   (let ((lines (hashtable-get *file-lines-table* file)))
      (if (not lines)
	  (let ((lines (file-lines file)))
	     (hashtable-put! *file-lines-table* file lines)
	     lines)
	  lines)))

;*---------------------------------------------------------------------*/
;*    pos->line ...                                                    */
;*---------------------------------------------------------------------*/
(define (pos->line file pos)
   (let ((lines (get-file-lines file)))
      (or (file-position->line pos lines) 0)))

;*---------------------------------------------------------------------*/
;*    find-location ...                                                */
;*---------------------------------------------------------------------*/
(define (find-location exp)
   (define (parse-location loc)
      (if (location? loc)
	  loc
	  (match-case loc
	     ((at ?fname ?pos)
	      (location fname pos (pos->line fname pos)))
	     (else
	      #f))))
   (cond
      ((epair? exp)
       ;; easy the location has been directly produced by the reader.
       (parse-location (cer exp)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    find-location/loc ...                                            */
;*---------------------------------------------------------------------*/
(define (find-location/loc exp loc)
   (let ((new-loc (find-location exp)))
      (if (location? new-loc)
	  new-loc
	  loc)))

;*---------------------------------------------------------------------*/
;*    location-full-fname ...                                          */
;*---------------------------------------------------------------------*/
(define (location-full-fname loc)
   (let* ((file-name (location-fname loc))
	  (full-name (make-file-name (pwd) file-name)))
      (if (file-exists? full-name)
	  full-name
	  (let ((lib-name (make-file-name *lib-src-dir* file-name)))
	     (if (file-exists? lib-name)
		 lib-name
		 file-name)))))

;*---------------------------------------------------------------------*/
;*    location-shape ...                                               */
;*---------------------------------------------------------------------*/
(define (location-shape loc l)
   (if (and *location-shape?* (location? loc))
       (cons (vector (string->symbol (location-fname loc))
		     ;; we use string->symbol just to avoid the #"..." printing
		     (location-pos loc)
		     (location-lnum loc))
	     l)
       l))
       
;*---------------------------------------------------------------------*/
;*    dump-location ...                                                */
;*---------------------------------------------------------------------*/
(define (dump-location from expr)
   (print "~~ " from ": " expr " " (find-location expr))
   (let loop ((expr expr))
      (if (pair? expr)
	  (begin
	     (print "____ " from ": " expr
		    " " (find-location expr))
	     (loop (cdr expr))))))

;*---------------------------------------------------------------------*/
;*    location-skip-forward ...                                        */
;*---------------------------------------------------------------------*/
(define (location-skip-forward loc skip)
   (if (location? loc)
       (let ((fname (location-fname loc))
	     (npos (+fx (location-pos loc) skip)))
	  (location fname npos (pos->line fname npos)))
       loc))
