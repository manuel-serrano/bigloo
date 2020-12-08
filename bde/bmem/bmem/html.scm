;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/html.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb  2 05:57:51 2003                          */
;*    Last change :  Fri Oct 27 18:40:56 2017 (serrano)                */
;*    Copyright   :  2003-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Html generation                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module html
   
   (include "html.sch"
	    "html-private.sch")
   
   (export  (abstract-class %html-element
	       (id read-only (default #f))
	       (class read-only (default #f))
	       (title read-only (default #f))
	       (style read-only (default #f)))
	    
	    ;; document
	    (class %html-document::%html-element
	       (attr*::pair-nil read-only (default '()))
	       (dtd read-only (default (html-dtd)))
	       (body read-only)
	       (css read-only (default (getenv "HTTP_HOP_CSS"))))
	    
	    ;; style
	    (class %html-style)
	    
	    ;; inline
	    (abstract-class %html-inline::%html-element)
	    
	    ;; phrase element
	    (class %html-pelement::%html-inline
	       (kind::symbol read-only)
	       (inline* read-only (default '())))

	    ;; linebreak
	    (class %html-br::%html-inline)
	    
	    ;; block
	    (abstract-class %html-block::%html-element)
	    
	    ;; box = block + inline*
	    (class %html-box::%html-block
	       (kind::symbol read-only)
	       (inline* read-only (default '()))
	       (align read-only (default #f)))
	    
	    ;; heading
	    (class %html-heading::%html-block
	       (level::int read-only)
	       (alignment read-only (default #f))
	       (inline* read-only (default '())))
	    
	    ;; form
	    (class %html-form::%html-block
	       (method read-only (default "POST"))
	       (action read-only)
	       (inline* read-only (default '())))
	    
	    ;; input
	    (class %html-input::%html-inline
	       (type read-only (default "text"))
	       (size read-only (default #f))
	       (cols read-only (default #f))
	       (name::bstring read-only)
	       (value read-only (default ""))
	       (checked read-only (default #unspecified)))

	    ;; textarea
	    (class %html-textarea::%html-inline
	       (rows::int read-only)
	       (cols::int read-only)
	       (name::bstring read-only)
	       (value read-only (default ""))
	       (readonly::bool read-only (default #f)))

	    ;; itemize
	    (class %html-itemize::%html-block
	       (kind::symbol read-only)
	       (li+::pair read-only))
	    (class %html-li::%html-element
	       (flow* read-only (default '())))
	    ;; table
	    (class %html-table::%html-block
	       (caption read-only (default #f))
	       (colgroup? read-only (default #f))
	       (tfoot? read-only (default #f))
	       (thead? read-only (default #f))
	       (width read-only (default #f))
	       (border read-only (default #f))
	       (frame read-only (default #f))
	       (rules read-only (default #f))
	       (cellspacing read-only (default #f))
	       (cellpadding read-only (default #f))
	       (tbody+::pair read-only (default '())))
	    (class %html-telement::%html-element
	       (kind::symbol read-only (default 'tbody))
	       (tr+::pair read-only))
	    (class %html-tr::%html-element
	       (tcell+::pair read-only))
	    (class %html-colgroup::%html-element
	       (width read-only (default #f))
	       (span read-only (default #f))
	       (align read-only (default #f))
	       (valign read-only (default #f))
	       (char read-only (default #f))
	       (col*::pair-nil read-only (default '())))
	    (class %html-tcell::%html-element
	       (kind::symbol read-only (default 'td))
	       (colspan::int read-only (default 1))
	       (rowspan::int read-only (default 1))
	       (align read-only (default #f))
	       (valign read-only (default #f))
	       (char read-only (default #f))
	       (width read-only (default #f))
	       (flow* read-only (default '())))
	    
	    ;; reference
	    (class %html-a::%html-element
	       (type read-only (default #f))
	       (href read-only (default #f))
	       (name read-only (default #f))
	       (inline* read-only (default '())))
	    
	    ;; css
	    (abstract-class %css
	       (type::bstring read-only (default "text/css")))


	    (class %css-file::%css
	       (path::bstring read-only))
	    
	    (class %css-list::%css
	       (entry+::pair read-only))
	    
	    (class %css-literal::%css
	       (entry+::bstring read-only))
	    
	    ;; cssentry
	    (class %css-entry
	       (selector::bstring read-only)
	       (value+::pair read-only)))
   
   (export  (list->css::%css ::pair)
	    
	    (html-plugin-usage ::obj ::bstring ::bstring ::%html-element)
	    (html-plugin-error ::obj ::bstring ::obj ::obj ::obj)
	    (html-plugin-version ::obj ::bstring ::bstring)

	    (html-server-error ::obj ::bstring ::obj ::obj ::obj)
	    (html-client-error ::obj ::bstring ::obj ::obj ::obj)

	    (html-string::bstring ::bstring)
	    (html-dtd::bstring)
	    (html-error ::obj ::obj ::obj)

	    (html-vbox obj . obj)
	    
	    (html->string::bstring ::%html-document)
	    (print-html ::%html-document)))

;*---------------------------------------------------------------------*/
;*    list->css ...                                                    */
;*---------------------------------------------------------------------*/
(define (list->css::%css l)
   (instantiate::%css-list
      (entry+ (map (lambda (l)
		      (instantiate::%css-entry
			 (selector (car l))
			 (value+ (let loop ((l+ (cdr l))
					    (res '()))
				    (cond
				       ((null? l+)
					(if (null? res)
					    (error 'css "Illegal css" l)
					    res))
				       ((null? (cdr l+))
					(error 'css "Illegal css" l))
				       (else
					(loop (cddr l+)
					      (cons (list (car l+) (cadr l+))
						    res))))))))
		   l))))
 
;*---------------------------------------------------------------------*/
;*    html-plugin-usage ...                                            */
;*---------------------------------------------------------------------*/
(define (html-plugin-usage style name cmdline usage)
   (html-document :title name
		  :style style
		  :body (list (html-h1 :inline* name)
			      (html-br)
			      (string-append "Usage: " cmdline)
			      (html-br)
			      usage)))

;*---------------------------------------------------------------------*/
;*    html-plugin-error ...                                            */
;*---------------------------------------------------------------------*/
(define (html-plugin-error style name obj proc msg)
   (html-document :title name
		  :style style
		  :body (list (html-h1 :inline* name)
			      (html-emergency :inline* "Plugin error !!!")
			      (html-br)
			      (html-error proc obj msg))))

;*---------------------------------------------------------------------*/
;*    html-plugin-version ...                                          */
;*---------------------------------------------------------------------*/
(define (html-plugin-version style name version)
   (html-document :title name
		  :style style
		  :body (list (html-h1 :inline* name)
			      "version: " version)))

;*---------------------------------------------------------------------*/
;*    html-server-error ...                                            */
;*---------------------------------------------------------------------*/
(define (html-server-error css name obj proc msg)
   (html-document :title name
		  :css css
		  :body (list (html-h1 :inline* name)
			      (html-emergency :inline* "Server error !!!")
			      (html-br)
			      (html-error proc obj msg))))

;*---------------------------------------------------------------------*/
;*    html-client-error ...                                            */
;*---------------------------------------------------------------------*/
(define (html-client-error style name obj proc msg)
   (html-document :title name
		  :style style
		  :body (list (html-h1 :inline* name)
			      (html-emergency :inline* "Client error !!!")
			      (html-br)
			      (html-error proc obj msg))))

;*---------------------------------------------------------------------*/
;*    html-string ...                                                  */
;*---------------------------------------------------------------------*/
(define (html-string str)
   (let ((len (string-length str)))
      (let loop ((r 0)
		 (nlen len))
	 (if (=fx r len)
	     (if (=fx nlen len)
		 str
		 (let ((res (make-string nlen)))
		    (let loop ((r 0)
			       (w 0))
		       (if (=fx w nlen)
			   res
			   (let ((c (string-ref str r)))
			      (case c
				 ((#\<)
				  (blit-string! "&lt;" 0 res w 4)
				  (loop (+fx r 1) (+fx w 4)))
				 ((#\>)
				  (blit-string! "&gt;" 0 res w 4)
				  (loop (+fx r 1) (+fx w 4)))
				 ((#\&)
				  (blit-string! "&amp;" 0 res w 5)
				  (loop (+fx r 1) (+fx w 5)))
				 ((#\")
				  (blit-string! "&quot;" 0 res w 6)
				  (loop (+fx r 1) (+fx w 6)))
				 (else
				  (string-set! res w c)
				  (loop (+fx r 1) (+fx w 1)))))))))
	     (case (string-ref str r)
		((#\< #\>)
		 (loop (+fx r 1) (+fx nlen 3)))
		((#\&)
		 (loop (+fx r 1) (+fx nlen 4)))
		((#\")
		 (loop (+fx r 1) (+fx nlen 5)))
		(else
		 (loop (+fx r 1) nlen)))))))

;*---------------------------------------------------------------------*/
;*    object-print ::%html-element ...                                 */
;*---------------------------------------------------------------------*/
(define-method (object-print v::%html-element port print-slot)
   (fprintf port "<~s>" (class-name (object-class v))))

;*---------------------------------------------------------------------*/
;*    (html-dtd) ...                                                   */
;*---------------------------------------------------------------------*/
(define (html-dtd)
   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">")

;*---------------------------------------------------------------------*/
;*    html-error  ...                                                  */
;*---------------------------------------------------------------------*/
(define (html-error msg proc obj)
   (html-internal-error :inline* (with-output-to-string
				    (lambda ()
				       (display "*** ERROR:")
				       (display-circle proc)
				       (display #":\n")
				       (display-circle msg)
				       (display " -- ")
				       (display-circle obj)))))

;*---------------------------------------------------------------------*/
;*    html->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (html->string val)
   (with-output-to-string
      (lambda ()
	 (print-html val))))

;*---------------------------------------------------------------------*/
;*    print-html ...                                                   */
;*---------------------------------------------------------------------*/
(define (print-html doc)
   (out doc 0)
   (newline))

;*---------------------------------------------------------------------*/
;*    *margins* ...                                                    */
;*---------------------------------------------------------------------*/
(define *margins* (vector "" " " "  " "   " "    " "     " "      "))

;*---------------------------------------------------------------------*/
;*    make-margin ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-margin n)
   (let ((l (vector-length *margins*)))
      (if (>=fx n l)
	  (let* ((nl (*fx l 2))
		 (nv (copy-vector *margins* (*fx l 2))))
	     (let loop ((i l))
		(if (=fx i nl)
		    (begin
		       (set! *margins* nv)
		       (make-margin n))
		    (begin
		       (vector-set! nv i (make-string i #\space))
		       (loop (+fx i 1))))))))
   (vector-ref *margins* n))
   
;*---------------------------------------------------------------------*/
;*    out-omarkup ...                                                  */
;*---------------------------------------------------------------------*/
(define (out-omarkup markup::bstring attr*::pair-nil)
   (display* "<" markup)
   (if (pair? attr*)
       (if (every string? attr*)
	   (begin
	      (display " ")
	      (let loop ((a* attr*))
		 (if (null? (cdr a*))
		     (display* (car a*) ">")
		     (begin
			(display* (car a*) " ")
			(loop (cdr a*))))))
	   (error "html" "Illegal attributes" attr*))
       (display ">")))

;*---------------------------------------------------------------------*/
;*    out-cmarkup ...                                                  */
;*---------------------------------------------------------------------*/
(define (out-cmarkup markup)
   (display*  "</" markup ">"))

;*---------------------------------------------------------------------*/
;*    out ::%html-element ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (out v m::int)
   (cond
      ((or (string? v) (number? v) (symbol? v))
       (display v))
      (else
       (warning 'html "Illegal html element (" (find-runtime-type v) ") -- " v)
       (display v))))

;*---------------------------------------------------------------------*/
;*    out* ...                                                         */
;*---------------------------------------------------------------------*/
(define (out* e* m)
   (cond
      ((pair? e*)
       (for-each (lambda (e) (out e m)) e*))
      ((null? e*)
       #unspecified)
      (else
       (out e* m))))

;*---------------------------------------------------------------------*/
;*    out+ ...                                                         */
;*---------------------------------------------------------------------*/
(define (out+ e* m)
   (for-each (lambda (e) (out e m)) e*))

;*---------------------------------------------------------------------*/
;*    out ::%html-document ...                                         */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-document m::int)
   (with-access::%html-document v (attr* dtd title body style css id class)
      ;; the dtd
      (if (string? dtd) (print (make-margin m) dtd))
      ;; the document
      (with-markup "html" attr* m
		   ;; the head
		   (if (or (string? title)
			   style
			   (string? css))
		       (with-markup "head" '() (+fx 1 m)
				    (out-html-head title style css (+fx m 1))))
		   ;; the body
		   (with-markup "body"
				(make-attributes ("class" class class)
						 ("id" id id))
				(+fx 1 m)
				(out* body (+fx 2 m))))))

;*---------------------------------------------------------------------*/
;*    out-html-head ...                                                */
;*---------------------------------------------------------------------*/
(define (out-html-head title style css m)
   ;; the head title
   (if (string? title)
       (with-oneline-markup "title" '() (+fx 1 m) (display title)))
   ;; the embedded style
   (out-css style m)
   ;; the css
   (if (string? css)
       (with-oneline-markup "link"
			    (make-attributes ("type" #t "text/css")
					     ("rel" #t "stylesheet")
					     ("href" #t css))
			    (+fx 1 m)
			    "")))

;*---------------------------------------------------------------------*/
;*    out-css ::%css ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (out-css style::obj m::int)
   (cond
      ((pair? style)
       (for-each (lambda (s) (out-css s m)) style))
      ((string? style)
       (out-css-file style m))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    out-css-file ...                                                 */
;*---------------------------------------------------------------------*/
(define (out-css-file path m)
   (let ((m2 (+fx 2 m)))
      (with-oneline-markup "link"
			   (make-attributes ("type" #t "text/css")
					    ("rel" #t "stylesheet")
					    ("href" #t path))
			   (+fx 1 m)
			   "")))

;*---------------------------------------------------------------------*/
;*    out-css :: ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (out-css style::%css-file m::int)
   (with-access::%css-file style (path)
      (out-css-file path m)))
 
;*---------------------------------------------------------------------*/
;*    out-css ::%css-literal ...                                       */
;*---------------------------------------------------------------------*/
(define-method (out-css style::%css-literal m)
   (let ((m2 (+fx 2 m)))
      (with-access::%css-literal style (type entry+)
	 (with-markup "style"
	    (make-attributes ("type" (string? type) type))
	    (+fx 1 m)
	    (display entry+)))))
   
;*---------------------------------------------------------------------*/
;*    out-css ::%css-list ...                                          */
;*---------------------------------------------------------------------*/
(define-method (out-css style::%css-list m)
   (let ((m2 (+fx 2 m)))
      (with-access::%css-list style (type entry+)
	 (with-markup "style"
	    (make-attributes ("type" (string? type) type))
	    (+fx 1 m)
	    (for-each (lambda (e)
			 (newline)
			 (out-css-entry e m2))
	       entry+)))))

;*---------------------------------------------------------------------*/
;*    out-css-entry ...                                                */
;*---------------------------------------------------------------------*/
(define (out-css-entry e::%css-entry m::int)
   (define (out-css-value v)
      (if (not (keyword? (car v)))
	  (error 'html "Illegal css value" v)
	  (print (keyword->string (car v)) ": " (cadr v) ";")))
   (with-access::%css-entry e (selector value+)
      (let ((m1 (make-margin m))
	    (m2 (make-margin (+fx 1 m))))
	 (print m1 selector " {")
	 (let loop ((v+ value+))
	    (if (null? (cdr v+))
		(begin
		   (display m2)
		   (out-css-value (car v+))
		   (display* m1 "}"))
		(begin
		   (display m2)
		   (out-css-value (car v+))
		   (loop (cdr v+))))))))

;*---------------------------------------------------------------------*/
;*    out ::%html-pelement ...                                         */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-pelement m::int)
   (with-access::%html-pelement v (id class title kind inline*)
      (with-markup (case kind
		      ((em) "em")
		      ((sup) "sup")
		      ((sub) "sub")
		      ((strong) "strong")
		      ((dfn) "dfn")
		      ((code) "code")
		      ((samp) "samp")
		      ((kbd) "kbd")
		      ((cite) "cite")
		      ((abbr) "abbr")
		      ((acronym) "acronym")
		      (else (error 'html "Illegal phrase element" kind)))
		   (make-attributes ("class" (string? class) class)
				    ("id" (string? id) id)
				    ("title" (string? title) title))
		   m
		   (out* inline* (+fx 1 m)))))

;*---------------------------------------------------------------------*/
;*    out ::%html-form ...                                             */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-form m::int)
   (with-access::%html-form v (id class title method action inline*)
      (with-oneline-markup "form"
			   (make-attributes ("class" (string? class) class)
					    ("id" (string? id) id)
					    ("title" (string? title) title)
					    ("method" (string? method) method)
					    ("action" (string? action) action))
			   m
			   (out* inline* (+fx 1 m)))))

;*---------------------------------------------------------------------*/
;*    out ::%html-input ...                                            */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-input m::int)
   (with-access::%html-input v (id class title
				   type name value checked size cols)
      (let ((c (and checked
		    (or (string=? type "radio")
			(string=? type "checkbutton")))))
	 (with-oneline-markup "input"
			      (make-attributes ("type" type type)
					       ("class" (string? class) class)
					       ("id" (string? id) id)
					       ("title" (string? title) title)
					       ("name" (string? name) name)
					       ("value" value value)
					       ("size" size size)
					       ("maxlength" cols cols)
					       ("checked" c "checked"))
			      m
			      ""))))

;*---------------------------------------------------------------------*/
;*    out ::%html-textarea ...                                         */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-textarea m::int)
   (with-access::%html-textarea v (id class title
				      name value cols rows readonly)
      (with-oneline-markup "textarea"
			   (make-attributes ("cols" cols cols)
					    ("rows" rows rows)
					    ("class" (string? class) class)
					    ("id" (string? id) id)
					    ("title" (string? title) title)
					    ("name" (string? name) name)
					    ("readonly" readonly "yes"))
			   m
			   (out value 0))))

;*---------------------------------------------------------------------*/
;*    out ::%html-itemize ...                                          */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-itemize m::int)
   (with-access::%html-itemize v (id class title kind li+)
      (with-markup (case kind
		      ((ul) "ul")
		      ((ol) "ol")
		      (else (error 'html "Illegal itemize" v)))
		   (make-attributes ("class" (string? class) class)
				    ("id" (string? id) id)
				    ("title" (string? title) title))
		   m
		   (out+ li+ (+fx 1 m)))))

;*---------------------------------------------------------------------*/
;*    out ::%html-li ...                                               */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-li m::int)
   (with-access::%html-li v (id class title flow*)
      (let ((markup "li")
	    (attr* (make-attributes ("class" (string? class) class)
				    ("id" (string? id) id)
				    ("title" (string? title) title))))
	 (match-case flow*
	    (()
	     (with-oneline-markup markup attr* m ""))
	    ((? string?)
	     (with-oneline-markup markup attr* m (out flow* (+fx m 1))))
	    (else
	     (with-markup markup attr* m (out* flow* (+fx m 1))))))))
   
;*---------------------------------------------------------------------*/
;*    out ::%html-table ...                                            */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-table m::int)
   (with-access::%html-table v (id class title
				  colgroup? thead? tfoot? tbody+ width
				  border frame rules
				  cellspacing cellpadding caption)
      (with-markup "table"
	 (make-attributes ("class" (string? class) class)
	    ("id" (string? id) id)
	    ("title" (string? title) title)
	    ("width" width width)
	    ("border" border border)
	    ("frame" frame frame)
	    ("rules" rules rules)
	    ("cellspacing" cellspacing cellspacing)
	    ("cellpadding" cellpadding cellpadding)
	    ("caption" caption caption))
	 m
	 (if caption
	     (with-oneline-markup "caption"
		'()
		(+fx m 1)
		(out caption (+fx m 2))))
	 (cond
	    ((not colgroup?)
	     #unspecified)
	    ((isa? colgroup? %html-colgroup)
	     (out colgroup? (+fx m 1)))
	    ((and (list? colgroup?)
		  (every (lambda (c) (isa? c %html-colgroup))
		     colgroup?))
	     (out+ colgroup? (+fx m 1)))
	    (else
	     (error 'html "Illegal table colgroup" colgroup?)))
	 (cond
	    ((not thead?)
	     #unspecified)
	    ((isa? thead? %html-telement)
	     (out thead? (+fx m 1)))
	    ((and (list? thead?)
		  (every (lambda (e) (isa? e %html-tr)) thead?))
	     (with-markup "thead"
		'()
		(+fx m 1)
		(out+ thead? (+fx m 2))))
	    ((isa? thead? %html-tr)
	     (with-markup "thead"
		'()
		(+fx m 1)
		(out thead? (+fx m 2))))
	    (else
	     (error 'html "Illegal table head" thead?)))
	 (cond
	    ((isa? tbody+ %html-telement)
	     (out tbody+ (+fx m 1)))
	    ((every (lambda (e) (isa? e %html-tr)) tbody+)
	     (out+ tbody+ (+fx m 1)))
	    (else
	     (error 'html "Illegal table body" tbody+)))
	 (cond
	    ((not tfoot?)
	     #unspecified)
	    ((isa? tfoot? %html-telement)
	     (out tfoot? (+fx m 1)))
	    ((and (list? tfoot?)
		  (every (lambda (e) (isa? e %html-tr)) tfoot?))
	     (out+ tfoot? (+fx m 1)))
	    ((isa? tfoot? %html-tr)
	     (out tfoot? (+fx m 1)))
	    (else
	     (error 'html "Illegal table foot" tfoot?))))))

;*---------------------------------------------------------------------*/
;*    out ::%html-telement ...                                         */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-telement m::int)
   (with-access::%html-telement v (kind id class title tr+)
      (if (not (every (lambda (e) (isa? e %html-tr)) tr+))
	  (error 'html "Illegal table element" v)
	  (with-markup (case kind
			  ((thead) "thead")
			  ((tfoot) "tfoot")
			  ((tbody) "tbody")
			  (else 'html "Illegal table element" v))
 		       (make-attributes ("class" (string? class) class)
					("id" (string? id) id)
					("title" (string? title) title))
		       m
		       (out+ tr+ (+fx 1 m))))))
    
;*---------------------------------------------------------------------*/
;*    out ::%html-tr ...                                               */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-tr m::int)
   (with-access::%html-tr v (id class title tcell+)
      (if (not (every (lambda (e) (isa? e %html-tcell)) tcell+))
	  (error 'html "Illegal tr" v)
	  (with-markup "tr"
		       (make-attributes ("class" (string? class) class)
					("id" (string? id) id)
					("title" (string? title) title))
		       m
		       (out+ tcell+ (+fx 1 m))))))

;*---------------------------------------------------------------------*/
;*    out ::%html-colgroup ...                                         */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-colgroup m::int)
   (with-access::%html-colgroup v (id class title
					 width span col* align valign char)
      (if (not (every (lambda (e) (isa? e %html-tcell)) col*))
	  (error 'html "Illegal colgroup" v)
	  (with-markup "colgroup"
		       (make-attributes ("class" (string? class) class)
					("id" (string? id) id)
					("title" (string? title) title)
					("align" align align)
					("valign" valign valign)
					("char" char char)
					("width" width width)
					("span" span span))
		       m
		       (out* col* (+fx 1 m))))))

;*---------------------------------------------------------------------*/
;*    out ::%html-tcell ...                                            */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-tcell m::int)
   (with-access::%html-tcell v (kind id class title
				     colspan rowspan align valign flow* width char)
      (let ((markup (case kind
		       ((th) "th")
		       ((td) "td")
		       ((col) "col")
		       (else 'html "Illegal table row element" v)))
	    (attr* (make-attributes ("class" (string? class) class)
				    ("id" (string? id) id)
				    ("title" (string? title) title)
				    ("rowspan" (not (=fx rowspan 1)) rowspan)
				    ("colspan" (not (=fx colspan 1)) colspan)
				    ("align" (string? align) align)
				    ("valign" (string? valign) valign)
			 	    ("char" (char? char) (string char))
				    ("width" (string? width) width))))
	 (match-case flow*
	    (()
	     (with-oneline-markup markup attr* m ""))
	    ((? string?)
	     (with-oneline-markup markup attr* m (out flow* (+fx m 1))))
	    (else
	     (with-markup markup attr* m (out* flow* (+fx m 1))))))))
	  
;*---------------------------------------------------------------------*/
;*    out ::%html-br ...                                               */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-br m::int)
   (with-access::%html-br v (id class title)
      (with-oneline-markup
       "br"
       (make-attributes ("class" (string? class) class)
			("id" (string? id) id)
			("title" (string? title) title))
       m)))
 
;*---------------------------------------------------------------------*/
;*    out ::%html-heading ...                                          */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-heading m::int)
   (with-access::%html-heading v (id class title level inline* alignment)
      (with-oneline-markup
       (case level
	  ((1) "h1")
	  ((2) "h2")
	  ((3) "h3")
	  ((4) "h4")
	  ((5) "h5")
	  ((6) "h6")
	  (else (error 'html "Illegal heading" v)))
       (make-attributes ("class" (string? class) class)
			("id" (string? id) id)
			("title" (string? title) title)
			("alignment" (string? alignment) alignment))
       m
       (out* inline* (+fx m 1)))))

;*---------------------------------------------------------------------*/
;*    out ::%html-box ...                                              */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-box m::int)
   (with-access::%html-box v (kind id class title inline* align style)
      (let ((attr (make-attributes ("class" (string? class) class)
		     ("style" (string? style) style)
		     ("align" (string? align) align)
		     ("id" (string? id) id)
		     ("title" (string? title) title))))
	 (if (eq? kind 'span)
	     (with-online-markup "span" attr m (out* inline* (+fx m 1)))
	     (with-oneline-markup (case kind
				     ((div) "div")
				     ((p) "p")
				     ((pre) "pre")
				     ((blockquote) "blockquote")
				     ((hr) "hr")
				     (else (error 'html "Illegal box" v)))
		attr m (out* inline* (+fx m 1)))))))

;*---------------------------------------------------------------------*/
;*    out ::%html-a ...                                                */
;*---------------------------------------------------------------------*/
(define-method (out v::%html-a m::int)
   (with-access::%html-a v (id class title type href inline* href name)
      (with-oneline-markup
       "a"
       (make-attributes ("class" (string? class) class)
			("id" (string? id) id)
			("title" (string? title) title)
			("href" (string? href) href)
			("name" (string? name) name)
			("type" (string? type) type))
       m
       (out* inline* (+fx m 1)))))

;*---------------------------------------------------------------------*/
;*    html-vbox ...                                                    */
;*---------------------------------------------------------------------*/
(define (html-vbox item . items)
   (html-ul :class "hop-verticalbox"
      (cons (html-li item)
	 (map (lambda (v) (html-li v)) items))))
