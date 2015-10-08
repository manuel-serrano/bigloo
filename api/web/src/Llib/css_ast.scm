;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/css_ast.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 29 10:04:44 2009                          */
;*    Last change :  Wed Oct  7 15:30:46 2015 (serrano)                */
;*    Copyright   :  2009-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The CSS ast class hierarchy                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_css-ast
   
   (export (class css-uri
	      (value::bstring read-only))

	   (class css-ext
	      (value::obj read-only))
	   
	   (class css-stylesheet
	      (charset read-only)
	      (comment*::pair-nil read-only)
	      (import*::pair-nil read-only)
	      (rule*::pair-nil read-only (default '())))

	   (class css-charset
	      (charset::bstring read-only)
	      (spec::bstring read-only))

	   (class css-comment
	      (cdo::bstring read-only)
	      (cdc::bstring read-only)
	      (content::obj read-only))

	   (class css-import
	      (value::obj read-only)
	      (medium*::pair-nil read-only))

	   (class css-media
	      (medium+::pair read-only)
	      (ruleset*::pair-nil read-only))

	   (class css-media-query
	      (operator::obj read-only)
	      (type::bstring read-only)
	      (expr*::pair-nil read-only))

	   (class css-page
	      (ident read-only)
	      (pseudopage read-only)
	      (declaration*::pair-nil read-only))

	   (class css-fontface
	      (declaration*::pair-nil read-only))

	   (class css-keyframes
	      (operator::bstring read-only)
	      (ident::bstring read-only)
	      (keyframe*::pair-nil read-only))

	   (class css-pseudopage
	      (ident read-only))

	   (class css-ruleset
	      (stamp::int (default -1))
	      (specificity::obj (default #f))
	      (selector+::pair read-only)
	      (declaration*::pair-nil read-only))

	   (class css-keyframe
	      (selector::obj read-only)
	      (declaration*::pair-nil read-only))

	   (class css-selector
	      (element::obj read-only (default #f))
	      (attr*::pair-nil read-only (default '())))

	   (class css-selector-class
	      (name::obj read-only))

	   (class css-selector-hash
	      (name::obj read-only))

	   (class css-selector-name
	      (name::obj read-only))

	   (class css-selector-attr
	      (ident::obj read-only)
	      (op::obj read-only (default #f))
	      (arg::obj read-only (default #unspecified)))

	   (class css-selector-pseudo
	      (expr::obj read-only)
	      (fun::obj read-only (default #f)))

	   (class css-declaration
	      (property::obj read-only)
	      (expr::obj read-only)
	      (prio::obj read-only (default #f)))

	   (class css-function
	      (fun::obj read-only)
	      (expr::obj read-only))

	   (class css-hash-color
	      (value::bstring read-only))

	   (generic css-write ::obj ::output-port)))

;*---------------------------------------------------------------------*/
;*    css-write* ...                                                   */
;*---------------------------------------------------------------------*/
(define (css-write* lst::pair-nil p::output-port)
   (for-each (lambda (o) (css-write o p)) lst))
		
;*---------------------------------------------------------------------*/
;*    css-sep-write* ...                                               */
;*---------------------------------------------------------------------*/
(define (css-sep-write* lst::pair-nil p::output-port sep)
   (when (pair? lst)
      (css-write (car lst) p)
      (when (pair? (cdr lst))
	 (for-each (lambda (o)
		      (display sep p)
		      (css-write o p))
		   (cdr lst)))))
		
;*---------------------------------------------------------------------*/
;*    css-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (css-write o p::output-port)
   (cond
      ((or (string? o) (number? o))
       (display o p))
      ((pair? o)
       (for-each (lambda (o) (css-write o p)) o))
      ((or (null? o) (not o))
       #unspecified)
      (else
       (display (format "\"error:~a\"" (find-runtime-type o)) p))))

;*---------------------------------------------------------------------*/
;*    css-write ::css-uri ...                                          */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-uri p::output-port)
   (with-access::css-uri o (value)
      (display "url( " p)
      (display value p)
      (display ")" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-ext ...                                          */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-ext p::output-port)
   (with-access::css-ext o (value)
      (css-write value p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-stylesheet ...                                   */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-stylesheet p::output-port)
   (with-access::css-stylesheet o (charset comment* import* rule*)
      (when charset
	 (css-write charset p))
      (when (pair? comment*)
	 (css-write* comment* p))
      (when (pair? import*)
	 (css-write* import* p))
      (css-write* rule* p)))
      
;*---------------------------------------------------------------------*/
;*    css-write ::css-charset ...                                      */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-charset p::output-port)
   (with-access::css-charset o (charset spec)
      (display charset p)
      (display " " p)
      (display spec p)
      (display ";\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-comment ...                                      */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-comment p::output-port)
   (with-access::css-comment o (cdo content cdc)
      (display cdo p)
      (css-write content p)
      (display cdc p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-import ...                                       */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-import p::output-port)
   (with-access::css-import o (value medium*)
      (display "@import " p)
      (css-write value p)
      (display " " p)
      (css-sep-write* medium* p ", ")
      (display ";\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-media ...                                        */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-media p::output-port)
   (with-access::css-media o (medium+ ruleset*)
      (display "@media " p)
      (css-sep-write* medium+ p ", ")
      (display " { " p)
      (css-write* ruleset* p)
      (display " }\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-media-query ...                                  */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-media-query p::output-port)
   (with-access::css-media-query o (operator type expr*)
      (when operator
	 (display operator p)
	 (display " " p))
      (css-write type p)
      (for-each (lambda (expr)
		   (display " and (" p)
		   (css-write (car expr) p)
		   (when (cdr expr)
		      (display ": " p)
		      (css-write (cdr expr) p))
		   (display ")" p))
		expr*)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-page ...                                         */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-page p::output-port)
   (with-access::css-page o (ident pseudopage declaration*)
      (display "@page " p)
      (when ident
	 (css-write ident p)
	 (display " " p))
      (when pseudopage
	 (css-write pseudopage p)
	 (display " " p))
      (display " { " p)
      (css-write* declaration* p)
      (display " }\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-fontface ...                                     */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-fontface p::output-port)
   (with-access::css-fontface o (declaration*)
      (display "@font-face {" p)
      (css-write* declaration* p)
      (display "}\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-keyframes ...                                    */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-keyframes p::output-port)
   (with-access::css-keyframes o (operator ident keyframe*)
      (display operator p)
      (display " " p)
      (display ident p)
      (display " {\n" p)
      (css-write* keyframe* p)
      (display "}\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-pseudopage ...                                   */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-pseudopage p::output-port)
   (with-access::css-pseudopage o (ident)
      (display ":" p)
      (css-write ident p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-ruleset ...                                      */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-ruleset p::output-port)
   
   (define (css-write-selector selector p)
      (css-write (car selector) p)
      (when (pair? (cdr selector))
	 (case (cadr selector)
	    ((+) (display " + " p))
	    ((>) (display " > " p))
	    (else (display " " p)))
	 (css-write-selector (cddr selector) p)))

   (with-access::css-ruleset o (selector+ declaration*)
      (css-write-selector (car selector+) p)
      (let loop ((selector+ (cdr selector+)))
	 (when (pair? selector+)
	    (display ",\n" p)
	    (css-write-selector (car selector+) p)
	    (loop (cdr selector+))))
      (display " {\n" p)
      (css-write* declaration* p)
      (display "}\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-keyframe ...                                     */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-keyframe p::output-port)
   (with-access::css-keyframe o (selector declaration*)
      (display selector p)
      (display " {\n" p)
      (css-write* declaration* p)
      (display "}\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-selector ...                                     */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-selector p::output-port)
   (with-access::css-selector o (element attr*)
      (when element (css-write element p))
      (for-each (lambda (a) (css-write a p)) attr*)))
      
;*---------------------------------------------------------------------*/
;*    css-write ::css-selector-class ...                               */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-selector-class p::output-port)
   (with-access::css-selector-class o (name)
      (display "." p)
      (display name p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-selector-hash ...                                */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-selector-hash p::output-port)
   (with-access::css-selector-hash o (name)
      (display "#" p)
      (display name p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-selector-name ...                                */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-selector-name p::output-port)
   (with-access::css-selector-name o (name)
      (display name p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-selector-attr ...                                */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-selector-attr p::output-port)
   (with-access::css-selector-attr o (ident op arg)
      (display "[" p)
      (css-write ident p)
      (when op
	 (display op p)
	 (css-write arg p))
      (display "]" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-selector-pseudo ...                              */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-selector-pseudo p::output-port)
   (with-access::css-selector-pseudo o (fun expr)
      (display ":" p)
      (when fun
	 (css-write fun p)
	 (display "(" p))
      (css-write expr p)
      (when fun
	 (display ")" p))))

;*---------------------------------------------------------------------*/
;*    css-write-expr ...                                               */
;*---------------------------------------------------------------------*/
(define (css-write-expr expr p)
   (css-sep-write* expr p " "))

;*---------------------------------------------------------------------*/
;*    css-write ::css-declaration ...                                  */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-declaration p::output-port)
   (with-access::css-declaration o (property expr prio)
      (display "  " p)
      (css-write property p)
      (display ": " p)
      (css-write-expr expr p)
      (when prio (css-write prio p))
      (display ";\n" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-function ...                                     */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-function p::output-port)
   (with-access::css-function o (fun expr)
      (display fun p)
      (display "(" p)
      (css-write-expr expr p)
      (display ")" p)))

;*---------------------------------------------------------------------*/
;*    css-write ::css-hash-color ...                                   */
;*---------------------------------------------------------------------*/
(define-method (css-write o::css-hash-color p::output-port)
   (with-access::css-hash-color o (value)
      (display "#" p)
      (display value p)))
