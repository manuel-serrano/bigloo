;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/rgc_trap.scm                 */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 24 14:24:02 1992                          */
;*    Last change :  Mon Jun  7 16:56:56 2010 (serrano)                */
;*                                                                     */
;*    Un essai de grammaire qui trappe                                 */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module rgc-trap
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-rgc-trap)))

;*---------------------------------------------------------------------*/
;*    rgc-test ...                                                     */
;*---------------------------------------------------------------------*/
(define (rgc-test)
   (let ((ncontext      0)
	 (ntiti         0)
	 (neol          0)
	 (nbol          0)
	 (neof          0) 
	 (n             0))
      (let ((grammar (regular-grammar ((lettre  (in ("az"))))
			((+ (in #\space #\newline))
			 (set! n (+ (the-length) n))
			 (ignore))
			((when (rgc-context? 'titi) "titi")
			 (rgc-context)
			 (set! n (+ (the-length) n))
			 (set! ncontext (+ 1 ncontext))
			 (ignore))
			("titi"
			 (set! ntiti (+ 1 ntiti))
			 (set! n (+ (the-length) n)) 
			 (rgc-context 'titi)
			 (ignore))
			((eof "toto")
			 (set! n (+ (the-length) n))
			 (set! neof (+ 1 neof))
			 (ignore))
			((eol "toto")
			 (set! n (+ (the-length) n))
			 (set! neol (+ 1 neol))
			 (ignore))
			((bol "toto")
			 (set! nbol (+ 1 nbol))
			 (ignore))
			("toto"
			 (set! n (+ (the-length) n)) 
			 (ignore))
			((+ lettre)
			 (set! n (+ (the-length) n)) 
			 (ignore))
			(else
			 (the-failure)))))
	 (labels ((foo (port)
		       (let loop ((r (read/rp grammar port)))
			  (if (eof-object? r)
			      (list ncontext ntiti neol nbol neof n)
			      (loop (read/rp grammar port))))))
	    (call-with-input-file "misc/trap.txt" foo)))))
      
;*---------------------------------------------------------------------*/
;*    test-rgc-trap ...                                                */
;*---------------------------------------------------------------------*/
(define (test-rgc-trap)
   (test-module "rgc-trap" "rgc_trap.scm")
   (test "test" (rgc-test) '(3 3 3 3 1 66)))
