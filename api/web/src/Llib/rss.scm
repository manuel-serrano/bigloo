;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/api/web/src/Llib/rss.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 17 08:12:41 2005                          */
;*    Last change :  Fri Sep  7 11:11:31 2018 (serrano)                */
;*    Copyright   :  2005-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    RSS parsing                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_rss
   
   (import __web_xml
	   __web_html
	   __web_date)
   
   (export (cdata-decode ::obj)
	   (rss-1.0-parse ::pair-nil ::pair-nil
			  ::procedure ::procedure ::procedure
			  #!key (prefix #f))
	   (rss-2.0-parse ::pair-nil ::pair-nil
			  ::procedure ::procedure ::procedure
			  #!key (prefix #f))
	   (rss-parse version ::pair-nil ::pair-nil prefix 
		      ::procedure ::procedure ::procedure)))

;*---------------------------------------------------------------------*/
;*    push! ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (push! list e)
   `(set! ,list (cons ,e ,list)))

;*---------------------------------------------------------------------*/
;*    pop! ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (pop! list)
   `(let ((kar (car ,list)))
       (set! ,list (cdr ,list))
       kar))

;*---------------------------------------------------------------------*/
;*    cdata-decode ...                                                 */
;*---------------------------------------------------------------------*/
(define (cdata-decode o)
   (cond
      ((string? o)
       (html-string-decode o))
      ((pair? o)
       (if (eq? (car o) 'cdata)
	   (html-string-decode (cdr o))
	   (map cdata-decode o)))
      (else
       o)))

;*---------------------------------------------------------------------*/
;*    rss-1.0-parse ...                                                */
;*---------------------------------------------------------------------*/
(define (rss-1.0-parse xml-tree xml-ns make-rss make-channel make-item
		       #!key (prefix #f))
   (rss-parse 1.0 xml-tree xml-ns prefix make-rss make-channel make-item))

;*---------------------------------------------------------------------*/
;*    rss-2.0-parse ...                                                */
;*---------------------------------------------------------------------*/
(define (rss-2.0-parse xml-tree xml-ns make-rss make-channel make-item
		       #!key (prefix #f))
   (rss-parse 2.0 xml-tree xml-ns prefix make-rss make-channel make-item))

;*---------------------------------------------------------------------*/
;*    rss-parse ...                                                    */
;*---------------------------------------------------------------------*/
(define (rss-parse version tree namespaces prefix
		   make-rss make-channel make-item)
   
   (let ((v1.0 #f)
	 (v2.0 #t))
      
      (when (=fl version 1.0)
	 (set! v1.0 #t)
	 (set! v2.0 #f))
      
      (define (drop-prefix e::symbol)
	 (if prefix
	     (let ((s (symbol->string e))
		   (l (string-length prefix)))
		(if (substring=? s prefix l)
		    (string->symbol (substring s (+fx l 1) (string-length s)))
		    e))
	     e))

      (define (decode-image img)
	 (append-map (lambda (prop)
			(if (pair? prop)
			    (filter-map (match-lambda
					   ((?key () (?val))
					    (cons key val))
					   (else
					    #f))
			       prop)
			    '()))
	    img))

      (define (flatten l)
	 (append-map (lambda (l)
			(if (pair? l)
			    (filter pair? l)
			    '()))
	    l))
      
      (define (channel attr body)
	 (let ((title #f)
	       (desc #f)
	       (links '())
	       (cat '())
	       (rights #f)
	       (modified #f)
	       (items '())
	       (rest '()))
	    
	    ;; sub-elements
	    (for-each (lambda (e)
			 (when (pair? e)
			    (match-case e
			       (((or title dc:ditle) ?- ?t . ?-)
				(set! title (cdata-decode t)))
			       (((or description dc:description) ?- ?t . ?-)
				(set! desc (cdata-decode t)))
			       ((link () (?href) . ?-)
				(push! links
				   `(alternate
				       (href . ,(cdata-decode href))
				       (title . ,title)
				       (type . ,#f))))
			       ((link (??- (href . ?href) ??-) . ?-)
				(push! links
				   `(alternate
				       (href . ,(cdata-decode href))
				       (title . ,title)
				       (type . ,#f))))
			       (((or category dc:subject) ?- ?cat . ?-)
				(push! cat (cdata-decode cat)))
			       (((or copyright dc:rights) ?- ?r . ?-)
				(unless rights
				   (set! rights (cdata-decode r))))
			       (((or lastBuildDate pubDate) ?- (?d) . ?-)
				(let* ((d0 (cdata-decode d))
				       (d (date->w3c-datetime
					     (rfc2822-date->date d0))))
				   (when (or (not modified)
					     (>fx (string-compare3 modified d)
						0))
				      (set! modified d))))
			       ((dc:date ?- (?dt . ?-) . ?-)
				(let ((d dt))
				   (when (or (not modified)
					     (>fx (string-compare3 modified d)
						0))
				      (set! modified d))))
			       ((item ?a ?b . ?-)
				;; Only for RSS 2.0
				(push! items (item a b)))
			       ((image . ?img)
				(set! rest
				   (cons* :image (decode-image img) rest)))
			       ((language . ?lang)
				(set! rest
				   (cons* language:
				      (car (apply append lang))
				      rest)))
			       (else
				(set! rest
				   (cons* (symbol->keyword (car e))
				      (apply append (cddr e))
				      rest))))))
		      body)
	    
	    ;; attributes are read last so the title is already known
	    (for-each (lambda (e)
			 (when (pair? e)
			    (case (car e)
			       ((rdf:about)
				(when v1.0
				   (push! links
					  `(self
					    (href ,(cdata-decode (cdr e)))
					    (title ,(or title
							(cdata-decode (cdr e))))
					    (type ,"application/rss+xml")))))
			       (else #f))))
		      attr)

	    (let ((chan (apply make-channel
			       :title title
			       :links links
			       :categories cat
			       :date modified
			       :rights rights
			       :subtitle desc
			       rest)))
	       
	       ;; RSS 1.0 specification :
	       ;;   <item> elements are not children of <channel>
	       ;;   But they are in RSS 2.0
	       (if (null? items)
		   (if v1.0
		       chan
		       (error 'rss-2.0-parse
			      "No items in channel element"
			      items))
		   (if (not v1.0)
		       (make-rss chan (reverse! items))
		       (error 'rss-1.0-parse
			      "items in channel element"
			      items))))))
      
      (define (rss-enclosure attr title)
	 (let ((href #f)
	       (type #f)
	       (length #f))
	    (for-each (lambda (e)
			 (when (pair? e)
			    (case (car e)
			       ((url)
				(set! href (cdata-decode (cdr e))))
			       ((type)
				(set! type (cdata-decode (cdr e))))
			       ((length)
				(set! length (cdata-decode (cdr e)))))))
		      attr)
	    `(enclosure
		(href . ,href)
		(type . ,type)
		(length . ,length)
		(title . ,title))))
      
      
      (define (item attr body)
	 (let ((title #f)
	       (authors '())
	       (cat '())
	       (links '()) ;; ((type (href . uri) (title . text) (type . mime)) ...)
	       (summary #f)
	       (content #f)
	       (date #f)
	       (source #f) ;; pair:  (title . uri)
	       (rights #f)
	       (rest '()))
	    
	    ;; Sub-elements
	    (for-each (lambda (e)
			 (when (pair? e)
			    (case (car e)
			       ((title dc:title)
				(unless title
				   (set! title (cdata-decode (caddr e)))))
			       ((author dc:creator)
				(push! authors (cdata-decode (caddr e))))
			       ((category dc:subject)
				(push! cat (cdata-decode (caddr e))))
			       ((link)
				(when (pair? (caddr e))
				   (push! links
				      `(alternate
					  (href . ,(cdata-decode (caaddr e)))
					  (title . ,title)
					  (type . ,#f)))))
			       ((enclosure)
				(let ((lnk (rss-enclosure (cadr e) title)))
				   (push! links lnk)))
			       ((description dc:description)
				(set! summary (cdata-decode (caddr e))))
			       ((content content:encoded)
				(set! content (cdata-decode (caddr e))))
			       ((pubDate)
				(match-case e
				   ((?- ?- (?dt))
				    (let ((pd (date->w3c-datetime
						 (rfc2822-date->date
						    (cdata-decode dt)))))
				       (when (or (not date)
						 (> (string-compare3 date pd) 0))
					  (set! date pd))))))
			       ((dc:date)
				(let ((d (cdata-decode (caaddr e))))
				   (when (or (not date)
					     (>fx (string-compare3 date d) 0))
				      (set! date d))))
			       ((source)
				(let ((uri (assoc 'url (cadr e))))
				   (when uri
				      (set! source
					    (cons (cdata-decode (caddr e))
						  (car uri))))))
			       ((dc:rights copyright)
				(set! rights (cdata-decode (caddr e))))
			       ((media:content)
				(set! rest
				   (cons* content: (flatten (cdr e)) rest)))
			       (else
				(set! rest
				   (cons* (symbol->keyword (car e))
				      (apply append (cddr e))
				      rest))))))
		      body)

	    (apply make-item
		   :title title
		   :links links
		   :categories cat
		   :date date
		   :rights rights
		   :summary summary
		   :content content
		   :authors authors
		   :source source
		   rest)))
      
      (define (rss-elements body)
	 (let ((chan #f)
	       (items '()))
	    (for-each (lambda (e)
			 (match-case e
			    ((channel ?v1 ?v2)
			     (set! chan (channel v1 v2)))
			    ((item ?v1 ?v2)
			     (push! items (item v1 v2)))))
		      body)
	    (if (pair? items)
		(if v1.0
		    (make-rss chan (reverse! items))
		    (error 'rss-2.0-parse
			   "found items outside channel elements"
			   items))
		(if (not v1.0)
		    chan
		    (error 'rss-1.0-parse
			   "no item found for channel"
			   chan)))))
      
      ;; A recoder...
      (filter-map (lambda (e)
		     (when (pair? e)
			(case (drop-prefix (car e))
			   ((rss RSS)
			    (rss-elements (caddr e)))
			   ((xml-decl instruction)
			    #f)
			   (else
			    (let ((rdf (assoc 'rdf-1999 namespaces)))
			       (when (and rdf
					  (string-ci=? (symbol->string (car e))
						       (format "~a:rdf"
							       (cdr rdf))))
				  (rss-elements (caddr e))))))))
		  tree)))
