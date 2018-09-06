;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/web/src/Llib/feeds.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Cyprien Nicolas                                   */
;*    Creation    :  Fri Aug 29 13:43:18 2008                          */
;*    Last change :  Thu Sep  6 15:48:36 2018 (serrano)                */
;*    Copyright   :  2008-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Unified support for atom and rss feeds                           */
;*=====================================================================*/


;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_feeds
   
   (import __web_xml
	   __web_html
	   __web_atom
	   __web_rss)

   (export (feed-parse ::input-port ::procedure ::procedure ::procedure
		       #!key (content-length 0) (encoding 'UTF-8))))

;*---------------------------------------------------------------------*/
;*    namespace->symbol ...                                            */
;*    -------------------------------------------------------------    */
;*    symbols to identify xml namespaces                               */
;*---------------------------------------------------------------------*/
(define (namespace->symbol ns::bstring)
   (string-case ns
      ("http://a9.com/-/spec/opensearchrss/1.0/" 'opensearch-1.0)
      ("http://purl.org/atom/ns#" 'atom)
      ("http://purl.org/rss/1.0/modules/syndication/" 'rss-1.0-syndication)
      ("http://purl.org/rss/1.0/" 'rss-1.0)
      ("http://purl.org/rss/1.0/modules/content/" 'rss-1.0-content)
      ("http://purl.org/dc/elements/1.1/" 'dc-1.1)
      ("http://radiofrance.fr/Lancelot/Podcast#" 'radiofrance)
      ("http://webns.net/mvcb/" 'metavocab)
      ("http://www.itunes.com/dtds/podcast-1.0.dtd" 'itunes-podcast-1.0)
      ("http://www.rddl.org/" 'rddl)
      ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" 'rdf-1999)
      ("http://www.w3.org/1999/xhtml" 'xhtml-1999)
      ("http://www.w3.org/1999/xlink" 'xlink-1999)
      ("http://www.w3.org/2005/Atom" 'atom-2005)
      (else ns)))

;*---------------------------------------------------------------------*/
;*    assq-cdr ...                                                     */
;*    -------------------------------------------------------------    */
;*    same as assoc, except the comparison is on cdr instead of car.   */
;*---------------------------------------------------------------------*/
(define (assq-cdr thing alist)
   (if (null? alist)
       #f
       (if (equal? (cdr (car alist)) thing)
	   (car alist)
	   (assq-cdr thing (cdr alist)))))

;*---------------------------------------------------------------------*/
;*    feed-parse ...                                                   */
;*    -------------------------------------------------------------    */
;*    reads xml from an input-port                                     */
;*---------------------------------------------------------------------*/
(define (feed-parse ip::input-port make-feed make-head make-body
		    #!key
		    (content-length 0)
		    (encoding 'UTF-8))
   ;; check the user arguments
   (unless (correct-arity? make-feed 2)
      (error "feed-parse"
	     (format "make-feed: bad arity 2 expected, ~a provided"
		     (procedure-arity make-feed))
	     make-feed))
   (unless (<fx (procedure-arity make-head) 0)
      (error "feed-parse"
	     (format "make-head: arity <0 expected, ~a provided"
		     (procedure-arity make-head))
	     make-head))
   (unless (<fx (procedure-arity make-body) 0)
      (error "feed-parse"
	     (format "make-body: arity <0 expected, ~a provided"
		     (procedure-arity make-body))
	     make-body))
   (let ((xml-tree (xml-parse ip
		      :content-length content-length
		      :encoding encoding))
	 (parser #f)
	 (prefix #f))
      (multiple-value-bind (ver enc lang root rver ns)
	 (xml-metadata xml-tree)
	 ;; Here we convert namespaces URL to symbols
	 (for-each (lambda (e)
		      (when (pair? e)
			 (set-car! e (namespace->symbol (car e)))))
		   (cons root ns))
	 ;; root is either a symbol (the first non-empty markup)
	 ;; or a pair, which car is a symbol for root ns, and cdr the markup
	 (cond
	    ((symbol? root)
	     ;; rss version 2.0 doesn't have a root namespace
	     (case root
		;; We shouldn't be too strict, so even
		;; illegal forms are allowed here.
		((rss)
		 ;; STRICT: We must check for version 2.0
		 (set! parser rss-2.0-parse))
		((channel)
		 ;; STRICT rdf:rdf needed for rss 1.0
		 (set! parser rss-1.0-parse))
		((feed)
		 ;; STRICT: Must return #f
		 (if (=fl rver 0.3)
		     (set! parser atom0.3-parse)
		     (set! parser atom2005-parse)))
		(else
		 ;; Ok, now we don't have any identifiable root namespace
		 ;; We have to check for every namespace if there is one
		 ;; which may be suitable for a feed (but maybe two are !?)
		 ;; So first we check if the root markup can be splitted
		 ;; (and it has to, or the document isn't valid...)
		 
		 ;; Here I only cares of the first markup and extract its
		 ;; prefix, which namespace may be used for feeds. Should
		 ;; it be better to start reading the namespace list and
		 ;; extract prefix for feed's namespaces ? But some people
		 ;; include atom into rss 2.0
		 (let* ((s (symbol->string root))
			(i (string-index-right s #\:)))
		    (if i 
			(let* ((pref (substring s 0 i))
			       (suff (substring s (+fx i 1) (string-length s)))
			       (my-ns (assq-cdr (string->symbol pref) ns)))
			   (if my-ns
			       ;; Yes! We found one !
			       (case (car my-ns)
				  ((rdf-1999)
				   ;; we have RDF
				   ;; RDF is used as a container for rss-1.0
				   ;; WARN: it may be used for other feeds !?
				   (for-each (lambda (e)
						(when (and (pair? e)
							   (eq? (car e)
								'rss-1.0))
						   (set! parser rss-1.0-parse)
						   (set! pref (cdr e))))
					     ns))
				  ((atom)
				   ;; Atom 0.3
				   (set! prefix pref)
				   (set! parser atom0.3-parse))
				  ((atom2005)
				   ;; Atom 1.0
				   (set! prefix pref)
				   (set! parser atom2005-parse))
				  (else
				   (error "feed-parse"
					  "Unable to use namespace"
					  my-ns)))
			       (error "feed-parse"
				      "No namespace defined to use for prefix"
				      pref)))
			(error "feed-parse"
			       "Illegal XML use of element without namespace"
			       root))))))
	    ((pair? root)
	     ;; STRICT: We must check the cdr also...
	     (case (car root) 
		((atom-2005) (set! parser atom2005-parse))
		((rss-1.0) (set! parser rss-1.0-parse))
		((atom) (and (= rver 0.3) (set! parser atom0.3-parse)))
		(else (error "feed-parse"
			     "Unknown root namespace element"
			     root)))))
	 (if parser
	     (parser xml-tree ns make-feed make-head make-body prefix: prefix)
	     (error "feed-parse" "Invalid feed" (list root rver ns))))))
