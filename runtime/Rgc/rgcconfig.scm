;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Rgc/rgcconfig.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep  9 09:10:07 1998                          */
;*    Last change :  Sun Aug 25 09:11:51 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The configuration file for the new regular expression package.   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc_config

   (import  __error)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __param
	    __object
	    __thread
	    __bexit
	    __bignum
	    __rgc
	    __bit
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r5_control_features_6_4
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r4_vectors_6_8)

   (export *rgc-optim*
	   (rgc-max-char)
	   (rgc-char?::bool char)
	   (rgc-alphabetic?::bool char)
	   (rgc-upcase char)
	   (rgc-downcase char)
	   (rgc-env)))

;*---------------------------------------------------------------------*/
;*    Rgc optimization level (mostly used by rgc-compile).             */
;*---------------------------------------------------------------------*/
(define *rgc-optim* #t)

;*---------------------------------------------------------------------*/
;*    The config structure                                             */
;*---------------------------------------------------------------------*/
(define-struct rgc-config
   name        ;; a string, the name of the configuration
   max-char    ;; a number, the maximum number of char
   char?       ;; a procedure, the predicate that returns true for characters
   alphabetic? ;; a procedure, the predicate for letters
   upcase      ;; a procedure that processes -> upcase
   downcase    ;; a procedure that processes -> downcase
   env)        ;; the default rgc environment 

;*---------------------------------------------------------------------*/
;*    rgc-max-char ...                                                 */
;*    -------------------------------------------------------------    */
;*    The number of chars that rgc is able to handle.                  */
;*---------------------------------------------------------------------*/
(define (rgc-max-char)
   (rgc-config-max-char *rgc-config*))

;*---------------------------------------------------------------------*/
;*    rgc-char? ...                                                    */
;*---------------------------------------------------------------------*/
(define (rgc-char?::bool char)
   ((rgc-config-char? *rgc-config*) char))

;*---------------------------------------------------------------------*/
;*    rgc-alphabetic? ...                                              */
;*---------------------------------------------------------------------*/
(define (rgc-alphabetic?::bool char)
   ((rgc-config-alphabetic? *rgc-config*) char))

;*---------------------------------------------------------------------*/
;*    rgc-upcase ...                                                   */
;*---------------------------------------------------------------------*/
(define (rgc-upcase char)
   ((rgc-config-upcase *rgc-config*) char))

;*---------------------------------------------------------------------*/
;*    rgc-downcase ...                                                 */
;*---------------------------------------------------------------------*/
(define (rgc-downcase char)
   ((rgc-config-downcase *rgc-config*) char))

;*---------------------------------------------------------------------*/
;*    rgc-env ...                                                      */
;*---------------------------------------------------------------------*/
(define (rgc-env)
   (rgc-config-env *rgc-config*))

;*---------------------------------------------------------------------*/
;*    *ascii-config* ...                                               */
;*    -------------------------------------------------------------    */
;*    The configuration for ascii characters                           */
;*---------------------------------------------------------------------*/
(define *ascii-config*
   (rgc-config "ascii"
	       256
	       (lambda (x)
		  (and (> x 0) (< x 256) (char? x)))
	       (lambda (x)
		  (and (> x 0) (< x 256) (char-alphabetic? (integer->char x))))
	       (lambda (x)
		  (char->integer (char-upcase (integer->char x))))
	       (lambda (x)
		  (char->integer (char-downcase (integer->char x))))
	       '((all     (out #\Newline))
		 (lower   (in ("az")))
		 (upper   (in ("AZ")))
		 (alpha   (or lower upper))
		 (digit   (in ("09")))
		 (xdigit  (uncase (in ("af09"))))
		 (alnum   (uncase (in ("az09"))))
		 (punct   (in ".,;!?"))
		 (blank   (in #" \t\n"))
		 (space   #\Space))))

;*---------------------------------------------------------------------*/
;*    *rgc-config* ...                                                 */
;*---------------------------------------------------------------------*/
(define *rgc-config* *ascii-config*)

