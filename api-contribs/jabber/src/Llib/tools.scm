(module tools
   	(library pthread)
	(import server-param)
	(export (list->xhtml node-list)
		(make-xhtml node)
		(send-file? id)
		(get-sid id)
		(get-file-name sid num)
		(assoc-bis obj L)
		(init-cv num)
		(free-socket? num)
		(search-node markup children)
		(send msg num)
		(clean-jid jid)
		(filename-filter path)
		(cv-get-value cv num)
		(cv-set-value cv value num)
	        (unlock cv mutex num)
		(clean-xml xml)))

(define list-markup '(div p span a))
(define list-attr '(style background))

;;;======================================================================
;;;                         list markup to xhtml string
;;;======================================================================

(define (list->xhtml node-list)  
  (cond ((null? node-list) "")
	((string? (car node-list)) (string-append (car node-list) (list->xhtml (cdr node-list))))
	((pair? (car node-list)) (string-append (make-xhtml (car node-list)) (list->xhtml (cdr node-list))))
	(else (list->xhtml (cdr node-list))))) 

;;;======================================================================
;;;                      markup to xhtml string
;;;======================================================================

(define (make-xhtml node)
  (if (not (pair? node))
      node
      (let* ((markup (car node)))
	(if (accept-markup? markup)
	    (begin (if (or (eq? markup 'div) (eq? markup 'p)) (set! markup 'span))
		   (let ((attr (cadr node))(children (caddr node))(newMarkup (string-append "<" (symbol->string markup) " ")))
		     (do ((x attr (cdr x)))
			 ((null? x)  (set! newMarkup (string-append newMarkup " >")) )
		       (if (accept-attr? (caar x))
			   (set! newMarkup (string-append newMarkup  (symbol->string  (caar x)) "=\"" (cdar x) "\""))))  
		     (do ((x children (cdr x)))
			 ((null? x))
		       (set! newMarkup (string-append newMarkup (make-xhtml (car x)))))
		     (string-append newMarkup "</" (symbol->string markup) ">")))
	    ""))))

;;;======================================================================
;;;              accepted markup and attributes 
;;;======================================================================

(define (accept-markup? markup)
  (member markup list-markup))

(define (accept-attr? attr)
  (member attr list-attr))

;;;======================================================================
;;;                      file send id ?
;;;======================================================================

(define (send-file? id)
  (pregexp-match "sendFile" id))

;;;======================================================================
;;;                         get sid 
;;;======================================================================

(define (get-sid id)
  (let ((res (pregexp-match "[^:]*$" id)))
    (if res
	(car res)
	#f)))

;;;======================================================================
;;;                        get file name for received file
;;;======================================================================
	      
(define (get-file-name sid num)
   (if (not (directory? FILE_DIR))
       (make-directory FILE_DIR))
   (let ((filename (assoc (string->symbol sid) (vector-ref list-file-name num))))
      (if filename
	  (with-lock (vector-ref  mutex-list-file-name num)
	     (lambda()
		(vector-set! list-file-name num (filter (lambda(x)(not (eq? (car x) (string->symbol sid)))) (vector-ref list-file-name num)))
		(cdr filename)))
	  #f)))

(define (assoc-bis obj L)
  (cond ((null? L)   #f)
	((or (not (pair? (car L))) (null? (car L))) (assoc-bis obj (cdr L)))
	((eq? (caar L) obj) (cdar L))
	(else (assoc-bis obj (cdr L)))))

;;;======================================================================
;;;                            init condition variable
;;;======================================================================

(define (init-cv num)
  (cv-set-value cv-connection #f num)
  (cv-set-value cv-remove #f num)
  (cv-set-value cv-send #f num))

;;;=======================================================================
;;;                free socket?
;;;=======================================================================

(define (free-socket? num)
  (let* ((cd (current-date))(cy (date-year cd))(cyd (date-year-day cd))(ch (date-hour cd))
	 (timer (Client-timer (vector-ref clients num)))(ty (date-year timer))(tyd (date-year-day timer))(th (date-hour timer)))
    (or (> cy ty) (> cyd tyd) (> ch th))))
     
;;;=======================================================================
;;;                       search node
;;;=======================================================================

(define (search-node markup children)
  (cond ((null? children) #f)
	((or (null? (car children)) (not (pair? (car children)))) (search-node markup (cdr children)))
	((eq? (caar children) markup) (car children))
	(else (search-node markup (cdr children)))))

;;;=======================================================================
;;;                       clean jid
;;;=======================================================================

(define (clean-jid jid)
  (do ((i 0 (+ i 1)))
      ((or  (>= i (string-length jid)) (char=? #\/ (string-ref jid i)))  (substring jid 0 i ))))

;;;=======================================================================
;;;                       filename filter
;;;=======================================================================

(define (filename-filter path)
  (let ((filename (pregexp-match "[^/]*$" path)))
    (if filename 
	(car filename)
	filename)))

;;;=======================================================================
;;;                         send message 
;;;=======================================================================

(define (send msg num)
  (display msg (socket-output (Client-socket (vector-ref clients num))))
  (flush-output-port  (socket-output (Client-socket (vector-ref clients num)))))

;;;=======================================================================
;;;                   condition variable tools
;;;=======================================================================

(define (cv-get-value cv num)
   (condition-variable-specific (vector-ref cv num)))

(define (cv-set-value cv value num)
   (condition-variable-specific-set! (vector-ref cv num) value))

(define (unlock cv mutex num)
  (with-lock (vector-ref mutex num)
	     (lambda()
	       (condition-variable-broadcast! (vector-ref cv num)))))

;;;=======================================================================
;;;                  replace character & < > 
;;;=======================================================================

(define (clean-xml xml)
  (let* ((tmp (pregexp-replace* "&" xml "&amp;"))
	 (tmp1 (pregexp-replace* "<" tmp "&lt;"))
	 (tmp2 (pregexp-replace* ">" tmp1 "&gt;")))
    tmp2))
