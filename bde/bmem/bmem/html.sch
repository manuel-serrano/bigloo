;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/html.sch               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb  8 08:57:06 2003                          */
;*    Last change :  Wed Oct 24 11:19:37 2012 (serrano)                */
;*    Copyright   :  2003-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compile-time HTML expansion. The purpose of this include file is */
;*    to avoid consing for calling HTML functions.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-html-markup                                               */
;*---------------------------------------------------------------------*/
(define-macro (define-html-markup markup . rest)
   (define (make-html-markup fident cident iident rest default)
      `(define-macro (,fident . a*)
	  (cons ',iident
		(let loop ((a* a*)
			   (res ',rest))
		   (cond
		      ((null? a*)
		       res)
		      ((not (keyword? (car a*)))
		       ,(if (symbol? default)
			   `(cons (list ',default (car a*))
				  res)
			   `(error (list 'quote ',fident)
				  "Illegal argument"
				  (car a*))))
		      ((null? (cdr a*))
		       `(error (list 'quote ',fident)
			       "Illegal argument"
			       (car a*)))
		      (else
		       (loop (cddr a*)
			     (cons (list (keyword->symbol (car a*))
					 (cadr a*))
				   res))))))))
   (match-case rest
      (()
       (let* ((fident (symbol-append 'html- markup))
	      (cident (symbol-append '% fident))
	      (iident (symbol-append (string->symbol "instantiate::") cident)))
	  (make-html-markup fident cident iident '() #f)))
      ((:default ?default)
       (let* ((fident (symbol-append 'html- markup))
	      (cident (symbol-append '% fident))
	      (iident (symbol-append (string->symbol "instantiate::") cident)))
	  (make-html-markup fident cident iident '() default)))
      (else
       (let* ((fident (symbol-append 'html- markup))
	      (cident (if (symbol? (car rest))
			  (symbol-append '%html- (car rest))
			  (symbol-append '% fident)))
	      (iident (symbol-append (string->symbol "instantiate::") cident)))
	  (let loop ((r* (if (symbol? (car rest))
			     (cdr rest)
			     rest))
		     (a* '())
		     (def #f))
	     (cond
		((null? r*)
		 (make-html-markup fident cident iident a* def))
		((eq? (car r*) ':default)
		 (if (null? (cdr r*))
		     (error 'define-html-markup "Illegal form" rest)
		     (loop (cddr r*)
			   a*
			   (cadr r*))))
		(else
		 (loop (cdr r*)
		       (cons (car r*) a*)
		       def))))))))

;*---------------------------------------------------------------------*/
;*    Regular html markup                                              */
;*---------------------------------------------------------------------*/
(define-html-markup document (id "main") :default body)
(define-html-markup style)
(define-html-markup inline)
(define-html-markup pelement)
(define-html-markup strong pelement (kind 'strong) :default inline*)
(define-html-markup em pelement (kind 'em) :default inline*)
(define-html-markup sup pelement (kind 'sup) :default inline*)
(define-html-markup sub pelement (kind 'sub) :default inline*)
(define-html-markup code pelement (kind 'code) :default inline*)
(define-html-markup br)
(define-html-markup hr box (kind 'hr) :default inline*)
(define-html-markup p box (kind 'p))
(define-html-markup dl box (kind 'dl))
(define-html-markup blockquote box (kind 'blockquote))
(define-html-markup pre box (kind 'pre) :default inline*)

;; div
(define-html-markup div box (kind 'div) :default inline*)
(define-html-markup span box (kind 'span) :default inline*)

;; pre-defined div
(define-html-markup app box (kind 'div) (class "hop-app") :default inline*)
(define-html-markup page box (kind 'div) (class "hop-page") :default inline*)
(define-html-markup title box (kind 'div) (class "hop-title") :default inline*)
(define-html-markup two-columns box (kind 'div) (class "hop-two-columns") :default inline*)
(define-html-markup leftmargin box (kind 'div) (class "hop-margin") (id "left") :default inline*)
(define-html-markup rightmargin box (kind 'div) (class "hop-margin") (id "right") :default inline*)
(define-html-markup body box (kind 'div) (class "body") :default inline*)

;; heading
(define-html-markup heading)
(define-html-markup h1 heading (level 1) :default inline*)
(define-html-markup h2 heading (level 2) :default inline*)
(define-html-markup h3 heading (level 3) :default inline*)
(define-html-markup h4 heading (level 4) :default inline*)
(define-html-markup h5 heading (level 5) :default inline*)
(define-html-markup h6 heading (level 6) :default inline*)

;; itemize
(define-html-markup ul itemize (kind 'ul) :default li+)
(define-html-markup ol itemize (kind 'ol) :default li+)
(define-html-markup li :default flow*)
(define-html-markup verticalbox itemize (kind 'ul) (class "verticalbox") :default li+)

;; tables
(define-html-markup table :default tbody+)
(define-html-markup telement)
(define-html-markup thead telement (kind 'thead))
(define-html-markup tbody telement (kind 'tbody))
(define-html-markup tfoot telement (kind 'tfoot))
(define-html-markup colgroup :default col+)
(define-html-markup tr :default tcell+)
(define-html-markup tcell)
(define-html-markup td tcell (kind 'td) :default flow*)
(define-html-markup th tcell (kind 'th) :default flow*)
(define-html-markup col tcell (kind 'col) :default flow*)

;; links
(define-html-markup a :default inline*)

;; forms
(define-html-markup form :default inline*)
(define-html-markup textarea :default value)
(define-html-markup text input (type "text") :default value)
(define-html-markup radio input (type "radio") :default value)
(define-html-markup submit input (type "submit"))
(define-html-markup reset input (type "reset"))
(define-html-markup hidden input (type "hidden"))

;; custom defined markup
(define-html-markup usage box (kind 'div) (class "hop-usage"))
(define-html-markup internal-error box (kind 'div) (class "hop-internalerror"))
(define-html-markup emergency box (kind 'span) (class "hop-emergency"))

;*---------------------------------------------------------------------*/
;*    two-columns ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander html-twocolumns
   (lambda (x e)
      (match-case x
	 ((?- :width ?w :scroll ?scr :margin ?mg :title ?title :page ?pg)
	  (let* ((page (cond
			  (pg
			   `(html-page ,pg))
			  (else
			   `(html-page '("")))))
		 (body (if title
			   `((html-title ,title) ,page)
			   page))
		 (margin `(html-leftmargin ,(if mg mg "")))
		 (% (lambda (v) (format "~a%" v))))
	     (if scr
		 (e `(html-table
		      :width "100%"
		      :frame "1px solid black"
		      :colgroup? (list (html-colgroup :width ,(% w))
				       (html-colgroup :width ,(% (-fx 100 w))))
		      (list
		       (html-tr
			(list
			 (html-td :align "left" :valign "top" ,margin)
			 (html-td :align "left" :valign "top" (list ,@body))))))
		    e)
		 (e `(html-two-columns :id ,w (list ,margin ,@body)) e))))
	 (else
	  (error "html-two-columns" "Illegal form" x)))))
				
