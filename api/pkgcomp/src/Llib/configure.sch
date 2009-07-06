;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pkgcomp/src/Llib/configure.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  7 14:06:51 2007                          */
;*    Last change :  Thu May 31 10:34:32 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Pkgcomp configuration                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    pkgcomp-default-suffix ...                                       */
;*---------------------------------------------------------------------*/
(define (pkgcomp-default-suffix)
   "scm")

;*---------------------------------------------------------------------*/
;*    pkgcomp-root-exception ...                                       */
;*---------------------------------------------------------------------*/
(define (pkgcomp-root-exception)
   '@exception)

;*---------------------------------------------------------------------*/
;*    pkgcomp-native-exceptions ...                                    */
;*---------------------------------------------------------------------*/
(define (pkgcomp-native-exceptions)
   '((@exception &error)
     (@error &error)
     (@io-error &io-error)
     (@type-error &type-error)))

;*---------------------------------------------------------------------*/
;*    pkgcomp-root-record ...                                          */
;*---------------------------------------------------------------------*/
(define (pkgcomp-root-record)
   'object)

;*---------------------------------------------------------------------*/
;*    pkgcomp-language-mark ...                                        */
;*---------------------------------------------------------------------*/
(define (pkgcomp-language-mark)
   '_)

;*---------------------------------------------------------------------*/
;*    language ...                                                     */
;*---------------------------------------------------------------------*/
(define (language lang)
   (symbol-append (pkgcomp-language-mark) lang))

;*---------------------------------------------------------------------*/
;*    pkgcomp-languages ...                                            */
;*---------------------------------------------------------------------*/
(define (pkgcomp-languages)
   `((bigloo (suffix "scm"))
     (mzscheme (import ,(language 'mzscheme)) (suffix "ss"))
     (stklos (import ,(language 'stklos)) (suffix "stk"))))

;*---------------------------------------------------------------------*/
;*    pkgcomp-native-languages ...                                     */
;*---------------------------------------------------------------------*/
(define (pkgcomp-native-languages)
   '(bigloo hop))

;*---------------------------------------------------------------------*/
;*    pkgcomp-interface-keywords ...                                   */
;*---------------------------------------------------------------------*/
(define (pkgcomp-interface-keywords)
   '(author version
	    description note keywords date license homepage maintainer
	    bigloo
	    mzscheme planet
	    chicken egg
	    stklos
	    snow))
