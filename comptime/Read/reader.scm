;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/reader.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 20 06:48:17 1998                          */
;*    Last change :  Mon Jun 22 10:04:05 2009 (serrano)                */
;*    Copyright   :  1998-2009 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The implementation of the function that physically reads on      */
;*    input ports.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module read_reader
   (import engine_param)
   (export (compiler-read . args)
	   (compiler-read-src . args)))

;*---------------------------------------------------------------------*/
;*    get-compiler-reader ...                                          */
;*---------------------------------------------------------------------*/
(define (get-compiler-reader)
   (or (bigloo-load-reader) read))
       
;*---------------------------------------------------------------------*/
;*    compiler-read ...                                                */
;*---------------------------------------------------------------------*/
(define (compiler-read . args)
   (let* ((read (get-compiler-reader))
	  (value (apply read args)))
      (if (eq? *reader* 'intern)
	  (cond
	     ((eof-object? value)
	      value)
	     ((string? value)
	      (string->obj value))
	     (else
	      (error "" "Illegal intern value" value)))
	  value)))

;*---------------------------------------------------------------------*/
;*    compiler-read-src ...                                            */
;*---------------------------------------------------------------------*/
(define (compiler-read-src . args)
   (let* ((read (get-compiler-reader))
	  (value (apply read args)))
      (if (eq? *reader* 'intern-src)
	  (cond
	     ((eof-object? value)
	      value)
	     ((string? value)
	      (string->obj value))
	     (else
	      (error "" "Illegal intern value" value)))
	  value)))
