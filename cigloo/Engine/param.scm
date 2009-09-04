;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Engine/param.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 29 11:45:41 1995                          */
;*    Last change :  Thu Sep  3 12:19:28 2009 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Configuration variables                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_param
   (export  *cigloo-version*
	    *cigloo-name*
	    *cigloo-level*
	    *cigloo-args*
	    *cigloo-rest-args*
	    *cigloo-author*
	    *cigloo-email*
	    *cigloo-date*
	    *cigloo-tmp*
	    *verbose*
	    *src*
	    *src-dirname*
	    *dest*
	    *iname*
	    *oport*
	    *scan-include*
	    *open-include*
	    *include-path*
	    *hookfile*
	    *c-type-alist*
	    *c-unsigned-type-alist*
	    *c-signed-type-alist*
	    *default-type*
	    *no-type*
	    *opaque-type*
	    *macro-variable*
	    *macro-function*
	    *directives*
	    *include-directive*
	    *define*
	    *define-fun*
	    *eval-stub?*
	    *gcc-extensions?*
            *enum-macros*
	    *int-enum*
            *use-cpp*
            *omit-underscore*
	    *ident-style*))
 
;*---------------------------------------------------------------------*/
;*    compiler and author names ...                                    */
;*---------------------------------------------------------------------*/
(define *cigloo-version*    "1.0")
(define *cigloo-name*       (string-append "Cigloo (v" *cigloo-version* ")"))
(define *cigloo-level*      #f)
(define *cigloo-cmd-name*   'nothing-yet)
(define *cigloo-args*       'nothing-yet)
(define *cigloo-rest-args*  '())
(define *cigloo-author*     "Manuel Serrano")
(define *cigloo-email*      "Manuel.Serrano@inria.fr")
(define *cigloo-date*       " Fri Sep 6 11:11:54 PDT 1996 ")
(define *cigloo-tmp*        (let ((Venv (getenv "TMPDIR")))
			      (if (string? Venv)
				  Venv
				  "/tmp")))

;*---------------------------------------------------------------------*/
;*    Compiler controls                                                */
;*---------------------------------------------------------------------*/
(define *verbose*        0)

;*---------------------------------------------------------------------*/
;*    file names                                                       */
;*---------------------------------------------------------------------*/
(define *src*            '())
(define *src-dirname*    #f)
(define *iname*          "")
(define *dest*           #f)
(define *oport*          (current-output-port))

;*---------------------------------------------------------------------*/
;*    The includes                                                     */
;*---------------------------------------------------------------------*/
(define *include-path*   '())
(define *scan-include*   '())
(define *open-include*   '())

;*---------------------------------------------------------------------*/
;*    Hooks                                                            */
;*---------------------------------------------------------------------*/
(define *hookfile*       #f)

;*---------------------------------------------------------------------*/
;*    Idents                                                           */
;*---------------------------------------------------------------------*/
(define *ident-style*    'plain)

;*---------------------------------------------------------------------*/
;*    *c-type-alist* ...                                               */
;*    -------------------------------------------------------------    */
;*    This list defines the Bigloo name of the C type. This list       */
;*    can be enlarged by user.                                         */
;*---------------------------------------------------------------------*/
(define *c-type-alist*
   '((obj         . "obj")
     (obj         . "obj_t")
     (bchar       . "bchar")
     (blong       . "blong")
     (procedure   . "procedure")    
     (pair        . "pair")         
     (nil         . "nil")          
     (bint        . "bint")         
     (blong       . "blong")        
     (bbool       . "bbool")        
     (cnst        . "cnst")         
     (bstring     . "bstring")      
     (bchar       . "bchar")        
     (real        . "real")         
     (vector      . "vector")             
     (tvector     . "tvector")      
     (struct      . "struct")       
     (tstruct     . "tstruct")      
     (output-port . "output-port" )
     (input-port  . "input-port"  )
     (binary-port . "binary-port")  
     (unspecified . "unspecified")  
     (symbol      . "symbol")       
     (cell        . "cell")         
     (exit        . "exit")         
     (foreign     . "foreign")      
     (char        . "char")
     (char*       . "string")
     (file*       . "file")
     (short       . "short")
     (int         . "int")
     (long        . "long")
     (longlong    . "llong") 
     (signed      . "sint")
     (unsigned    . "uint")
     (float       . "float")
     (double      . "double")))

(define *c-unsigned-type-alist*
   '((char     . "uchar")
     (short    . "ushort")
     (int      . "uint")
     (long     . "ulong")))

   
(define *c-signed-type-alist*
   '((char     . "schar")
     (short    . "short")
     (int      . "int")
     (long     . "long")))

(define *default-type* "int")

(define *no-type*      '())

(define *opaque-type*  '())

;*---------------------------------------------------------------------*/
;*    *macro* ...                                                      */
;*---------------------------------------------------------------------*/
(define *macro-function* #f)
(define *macro-variable* #f)

;*---------------------------------------------------------------------*/
;*    *directives* ...                                                 */
;*---------------------------------------------------------------------*/
(define *directives*        #t)
(define *include-directive* #f)

;*---------------------------------------------------------------------*/
;*    The cpp commands ...                                             */
;*---------------------------------------------------------------------*/
(define *define*           #t)
(define *define-fun*       #t)   

;*---------------------------------------------------------------------*/
;*    Stub production                                                  */
;*---------------------------------------------------------------------*/
(define *eval-stub?*       #f)

;*---------------------------------------------------------------------*/
;*    *gcc-extensions?* ...                                            */
;*---------------------------------------------------------------------*/
(define *gcc-extensions?*  #f)

;*---------------------------------------------------------------------*/
;*    *enum-macros* ...                                                */
;*---------------------------------------------------------------------*/
(define *enum-macros* #f)

;*---------------------------------------------------------------------*/
;*    *int-enum* ...                                                   */
;*---------------------------------------------------------------------*/
(define *int-enum* #f)

;*---------------------------------------------------------------------*/
;*    *use-cpp* ...                                                    */
;*---------------------------------------------------------------------*/
(define *use-cpp* #f)

;*---------------------------------------------------------------------*/
;*    *omit-underscore* ...                                            */
;*---------------------------------------------------------------------*/
(define *omit-underscore* #f)

