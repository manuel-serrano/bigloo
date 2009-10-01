;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/atom.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Cyprien Nicolas                                   */
;*    Creation    :  Fri Aug 29 14:56:48 2008                          */
;*    Last change :  Thu Oct  1 14:09:50 2009 (serrano)                */
;*    Copyright   :  2008-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Support for both atom-2005 (1.0) and atom 0.3                    */
;*    Even if the latter is deprecated, google still use it for        */
;*    google mail.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_atom
   
   (import __web_xml
	   __web_html
	   __web_rss)
   
   (export (atom2005-parse ::pair-nil ::pair-nil
			   ::procedure ::procedure ::procedure
			   #!key (prefix #f))
	   (atom0.3-parse  ::pair-nil ::pair-nil
			   ::procedure ::procedure ::procedure
			   #!key (prefix #f))
	   (atom-parse version ::pair-nil ::pair-nil prefix
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
;*    simple-cdata-decode ...                                          */
;*---------------------------------------------------------------------*/
(define (simple-cdata-decode o)
   (cond
      ((string? o)
       o)
      ((pair? o)
       (if (eq? (car o) 'cdata)
	   (cdr o)
	   o))
      (else
       o)))

;*---------------------------------------------------------------------*/
;*    atom2005-parse ...                                               */
;*---------------------------------------------------------------------*/
(define (atom2005-parse xml-tree xml-ns make-atom make-feed make-entry
			#!key (prefix #f))
   (atom-parse 2005.0 xml-tree xml-ns prefix make-atom make-feed make-entry))

;*---------------------------------------------------------------------*/
;*    atom0.3-parse ...                                                */
;*---------------------------------------------------------------------*/
(define (atom0.3-parse xml-tree xml-ns make-atom make-feed make-entry
		       #!key (prefix #f))
   (atom-parse 0.3 xml-tree xml-ns prefix make-atom make-feed make-entry))

;*---------------------------------------------------------------------*/
;*    atom-parse ...                                                   */
;*---------------------------------------------------------------------*/
(define (atom-parse version xml-tree xml-ns prefix make-atom make-head make-entry)
   (let ((v2005 #t)
	 (v0.3 #f))
      
      (define (drop-prefix e::symbol)
	 (if prefix
	     (let ((s (symbol->string e))
		   (l (string-length prefix)))
		(if (substring=? s prefix l)
		    (string->symbol (substring s (+fx l 1) (string-length s)))
		    e))
	     e))
      
      (define (atom-person attr body)
	 (let ((name #f) (uri #f) (email #f))
	    (for-each (lambda (elmt)
			 (when (pair? elmt)
			    (case (drop-prefix (car elmt))
			       ((name)
				(set! name (cdata-decode (caddr elmt))))
			       ((uri)
				(set! uri (cdata-decode (caddr elmt))))
			       ((email)
				(set! email (cdata-decode (caddr elmt)))))))
		      body)
	    name))
      
      (define (atom:category attr)
	 ;; No content defined for <atom:category> element
	 (let ((term #f)
	       (scheme #f)
	       (label #f))
	    (for-each (lambda (elt)
			 (when (pair? elt)
			    (case (drop-prefix (car elt))
			       ((term)
				(set! term (cdata-decode (cdr elt))))
			       ((scheme)
				(set! scheme (cdata-decode (cdr elt))))
			       ((label)
				(set! label (cdata-decode (cdr elt)))))))
		      attr)
	    (or label term scheme)))
      
      (define (atom:content attr body)
	 (if v0.3
	     (let ((t (assq 'type attr))
		   (m (assq 'mode attr)))
		(unless t (set! t "text/plain"))
		(unless m (set! m "xml"))
		(when (substring=? t "text/" 5)
		   (string-case m
		      ("xml" body)
		      ("escaped" (cdata-decode body))
		      ("base64" (base64-decode body)))))
	     
	     (let ((t (assq 'type attr))
		   (src (assq 'src attr)))
		(cond
		   ;; RFC 4287 4.1.3
		   ((string=? (cdr t) "text") body)
		   ((string=? (cdr t) "html") (cdata-decode body))
		   ((string=? (cdr t) "xhtml")(cdata-decode (caddr body)))
		   ((string-contains-ci (cdr t) "xml") body)
		   ((substring-ci=? (cdr t) "text/" 5) body)
		   (else (base64-decode body))))))
      
      (define (atom:link attr)
	 ;; atomLink =
	 ;;   element atom:link {
	 ;;      atomCommonAttributes,
	 ;;      attribute href { atomUri },
	 ;;      attribute rel { atomNCName | atomUri }?,
	 ;;      attribute type { atomMediaType }?,
	 ;;      attribute hreflang { atomLanguageTag }?,
	 ;;      attribute title { text }?,
	 ;;      attribute length { text }?,
	 ;;      undefinedContent
	 ;;  }
	 (let ((href #f)
	       (rel #f)
	       (type #f)
	       (hreflang #f)
	       (title #f)
	       (length #f))
	    
	    (for-each (lambda (elt)
			 (when (pair? elt)
			    (case (drop-prefix (car elt))
			       ((href)
				(set! href (cdata-decode (cdr elt))))
			       ((rel)
				(set! rel (string->symbol (cdr elt))))
			       ((type)
				(set! type (cdata-decode (cdr elt))))
			       ((hreflang)
				(set! hreflang (cdata-decode (cdr elt))))
			       ((title)
				(set! title (cdata-decode (cdr elt))))
			       ((length)
				(set! length (cdata-decode (cdr elt)))))))
		      attr)
	    
	    (let ((base `(,rel (href . ,href)
			       (type . ,type)
			       (title . ,title)))
		  (supp `((hreflang . ,hreflang)
			  (length . ,length))))
	       
	       (and href (if v0.3 base (append base supp))))))
      
      (define (atom:generator attr body)
	 ;; atomGenerator = element atom:generator {
	 ;;   atomCommonAttributes,
	 ;;   attribute uri { atomUri }?,
	 ;;   attribute version { text }?,
	 ;;   text
	 ;; }
	 (cdata-decode body))
      
      (define (atom:source attr body)
	 ;; atomSource =
	 ;;   element atom:source {
	 ;;      atomCommonAttributes,
	 ;;      (atomAuthor*
	 ;;       & atomCategory*
	 ;;       & atomContributor*
	 ;;       & atomGenerator?
	 ;;       & atomIcon?
	 ;;       & atomId?
	 ;;       & atomLink*
	 ;;       & atomLogo?
	 ;;       & atomRights?
	 ;;       & atomSubtitle?
	 ;;       & atomTitle?
	 ;;       & atomUpdated?
	 ;;       & extensionElement*)
	 ;;   }
	 (let ((title #f) (href #f))
	    (for-each (lambda (e)
			 (when (pair? e)
			    (case (drop-prefix (car e))
			       ((link)
				(let ((lnk (atom:link (cadr e))))
				   (when (and lnk (eq? (car lnk) 'alternate))
				      (set! href (cdr (assq 'href lnk))))))
			       ((title)
				(set! title (cdata-decode (caddr e))))
			       (else
				#unspecified))))
		      body)
	    
	    (cons title href)))
      
      (define (atom:entry entry)
	 ;; atomEntry =
	 ;;   element atom:entry {
	 ;;      atomCommonAttributes,
	 ;;      (atomAuthor*
	 ;;       & atomCategory*
	 ;;       & atomContent?
	 ;;       & atomContributor*
	 ;;	 & atomId
	 ;;       & atomLink*
	 ;;       & atomPublished?
	 ;;       & atomRights?
	 ;;       & atomSource?
	 ;;       & atomSummary?
	 ;;       & atomTitle
	 ;;       & atomUpdated
	 ;;       & extensionElement*)
	 ;;   }
	 (let ((authors '())
	       (cat '())
	       (content #f)
	       (created #f)
	       (links '())
	       (rights #f)
	       (source #f)
	       (summary #f)
	       (title #f)
	       (updated #f)
	       (rest '()))
	    (for-each (lambda (e)
			 (when (pair? e)
			    (case (drop-prefix (car e))
			       ((author)
				(let ((pers (atom-person (cadr e) (caddr e))))
				   (when pers (push! authors pers))))
			       ((category)
				;; ignore the element's body
				(push! cat (atom:category (cadr e))))
			       ((content)
				(set! content
				      (atom:content (cadr e) (caddr e))))
			       ((link)
				(let ((lnk  (atom:link (cadr e))))
				   (when lnk
				      (push! links lnk))))
			       ((published issued updated modified created)
				(when (or (not updated)
					  (>fx (string-compare3
						updated
						(caaddr e))
					       0))
				   (set! updated (caaddr e))))
			       ((rights copyright)
				(set! rights (cdata-decode (caddr e))))
			       ((source)
				(set! source (atom:source (cadr e) (caddr e))))
			       ((summary)
				(set! summary (cdata-decode (caddr e))))
			       ((title)
				(set! title (cdata-decode (caddr e))))
			       (else
				(push! rest e)))))
		      entry)
	    
	    ;;  title authors cats links summary content date source rights)
	    (apply make-entry
		   :title title
		   :links links
		   :categories cat
		   :date updated
		   :rights rights
		   :summary summary
		   :content content
		   :authors authors
		   :source source
		   rest)))
      
      
      (define (atom:feed attr body)
	 ;; atomFeed =
	 ;;   element atom:feed {
	 ;;      atomCommonAttributes,
	 ;;      (atomAuthor*
	 ;;       & atomCategory*
	 ;;       & atomContributor*
	 ;;       & atomGenerator?
	 ;;       & atomIcon?
	 ;;       & atomId
	 ;;       & atomLink*
	 ;;       & atomLogo?
	 ;;       & atomRights?
	 ;;       & atomSubtitle?
	 ;;       & atomTitle
	 ;;       & atomUpdated
	 ;;       & extensionElement*),
	 ;;      atomEntry*
	 ;;   }
	 (let ((cat '())
	       (links '())
	       (subtitle #f)
	       (logo #f)
	       (rights #f)
	       (title #f)
	       (updated #f)
	       (items '())
	       (rest '()))
	    (for-each (lambda (e)
			 (when (pair? e)
			    (case (car e)
			       ((category)
				(push! cat (atom:category (cadr e))))
			       ((link)
				(let ((lnk (atom:link (cadr e))))
				   (when lnk
				      (push! links lnk))))
			       ((logo)
				(set! logo (cdata-decode (caddr e))))
			       ((rights copyright)
				(set! rights (cdata-decode (caddr e))))
			       ((subtitle tagline)
				(set! subtitle (cdata-decode (caddr e))))
			       ((title)
				(set! title (cdata-decode (caddr e))))
			       ((updated modified)
				(let ((d (simple-cdata-decode (caaddr e))))
				   (when (or (not updated)
					     (>fx (string-compare3-ci updated d)
						  0))
				      (set! updated d))))
			       ((entry)
				(push! items
				       (atom:entry
					(simple-cdata-decode (caddr e)))))
			       (else
				(push! rest e)))))
		      body)
	    
	    (make-atom (apply make-head
			      :title title
			      :links links
			      :categories cat
			      :date updated
			      :rights rights
			      :subtitle subtitle
			      rest)
		       (reverse! items))))
      
      (cond
	 ((= version 0.3)
	  (set! v0.3 #t))
	 ((= version 2005)
	  (set! v2005 #t) (set! v0.3 #f))
	 (else (error 'atom-parse
		      "Illegal or Unsupported Atom Syndication Format version"
		      version)))
      
      (filter-map (lambda (e)
		     (when (pair? e)
			(case (drop-prefix (car e))
			   ((feed) (atom:feed (cadr e) (caddr e)))
			   (else #f))))
		  xml-tree)))
