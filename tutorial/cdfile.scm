;*=====================================================================*/
;*    serrano/prgm/utils/cdda/scm/cdfile.scm                           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 30 15:31:33 1997                          */
;*    Last change :  Thu May  7 09:53:11 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We parse input file                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cdfile
   (export  (parse-file iport)))

;*---------------------------------------------------------------------*/
;*    parse-file ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-file iport)
   (let* ((exp (read iport))
	  (done (read iport)))
      (if (not (eof-object? done))
	  (warning "cdisc" "Data ignored" done))
      (match-case exp
	 ((define-cd . ?fields)
	  (parse-fields fields))
	 (else
	  (error "cdisc" "Illegal file" exp)))))

;*---------------------------------------------------------------------*/
;*    parse-fields ...                                                 */
;*---------------------------------------------------------------------*/
(define (parse-fields fields)
   (let ((author    #unspecified)
	 (title     #unspecified)
	 (editor    #unspecified)
	 (date      #unspecified)
	 (note      '())
	 (musicians #unspecified)
	 (songs     #unspecified)
	 (id        #unspecified)
	 (kind      #unspecified))
      (let loop ((fields fields))
	 (if (null? fields)
	     (if (and (string? author)
		      (string? title)
		      (string? editor)
		      (string? date)
		      (pair? musicians)
		      (pair? songs)
		      (or (not (pair? id)) (= (length id) (length songs))))
		 (values title
			 author
			 (string-append editor " " date)
			 note
			 musicians
			 songs
			 id
			 kind)
		 (error "cdisc"
			"Missing fields"
			(let ((missing '()))
			   (if (and (pair? id)
				    (not (< (length id) (length songs))))
			       (set! missing (cons 'id id)))
			   (if (and (pair? id)
				    (not (> (length id) (length songs))))
			       (set! missing (cons 'songs songs)))
			   (if (not (string? author))
			       (set! missing (cons 'author missing)))
			   (if (not (string? title))
			       (set! missing (cons 'title missing)))
			   (if (not (string? editor))
			       (set! missing (cons 'editor missing)))
			   (if (not (string? date))
			       (set! missing (cons 'date missing)))
			   (if (not (pair? musicians))
			       (set! missing (cons 'musicians missing)))
			   (if (not (pair? songs))
			       (set! missing (cons 'songs missing)))
			   missing)))
	     (begin
		(match-case (car fields)
		   ((author ?auth)
		    (set! author (string-good-case auth)))
		   ((id ?discid)
		    (let ((set-id (lambda (value)
				     (if (not (pair? id))
					 (set! id (list value))
					 (set! id (append id (list value)))))))
		       (cond
			  ((symbol? discid)
			   (set-id (string-downcase (symbol->string discid))))
			  ((number? discid)
			   (set-id (string-downcase (number->string discid))))
			  ((string? discid)
			   (set-id (string-downcase discid))))))
		   ((kind ?k)
		    (case k
		       ((classic)
			(set! kind "classic"))
		       ((jazz)
			(set! kind "jazz"))
		       ((rock)
			(set! kind "rock"))
		       (else
			(error "cdisc" "Unknown kind" k))))
		   ((title ?tit)
		    (set! title (string-good-case tit)))
		   ((editor ?edit)
		    (set! editor (string-downcase edit)))
		   ((date ?dat)
		    (set! date dat))
		   ((record ?rec)
		    (if (pair? note)
			(set! note (cons rec note))
			(set! note (list rec))))
		   ((note ?rec)
		    (if (pair? note)
			(set! note (append note (list rec)))
			(set! note (list rec))))
		   ((musicians . ?mus)
		    (set! musicians (parse-musicians mus)))
		   ((songs . ?song)
		    (if (not (pair? songs))
			(set! songs (list (parse-songs song)))
			(set! songs (append songs (list (parse-songs song))))))
		   (else
		    (warning "cdisc" "Unknown field (ignored)" (car fields))))
		(loop (cdr fields)))))))

;*---------------------------------------------------------------------*/
;*    parse-musicians ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-musicians musicians)
   (let loop ((musicians musicians)
	      (res      '()))
      (if (null? musicians)
	  (reverse! res)
	  (match-case (car musicians)
	     ((or (and (? string?) ?name) (?name))
	      (loop (cdr musicians)
		    (cons (cons (string-good-case name)
				#unspecified)
			  res)))
	     ((?name . ?tools)
	      (loop (cdr musicians)
		    (cons (cons (string-good-case name)
				(let loop ((tools tools))
				   (cond
				      ((null? tools)
				       "")
				      ((string? (car tools))
				       (string-append
					(string-downcase (car tools))
					(if (pair? (cdr tools))
					    " "
					    "")
					(loop (cdr tools))))
				      ((symbol? (car tools))
				       (string-append
					(string-downcase
					 (symbol->string (car tools)))
					(if (pair? (cdr tools))
					    " "
					    "")
					(loop (cdr tools))))
				      (else
				       (error "cdisc"
					      "Illegal musician"
					      (car musicians))))))
			  res)))
	     (else
	      (error "cdisc"
		     "Illegal musician"
		     (car musicians)))))))

;*---------------------------------------------------------------------*/
;*    string-good-case ...                                             */
;*---------------------------------------------------------------------*/
(define (string-good-case string)
   (let ((len (string-length string)))
      (let loop ((i 0)
		 (state 'up))
	 (cond
	    ((=fx i len)
	     string)
	    ((and (<fx i (- len 3))
		  (char-ci=? (string-ref string i) #\d)
		  (char=? (string-ref string (+fx i 1)) #\e)
		  (char=? (string-ref string (+fx i 2)) #\space))
	     (string-set! string i (char-downcase (string-ref string i)))
	     (loop (+fx i 1) 'down))
	    ((or (and (char=? (string-ref string i) #\space)
		      (<fx i (- len 2))
		      (not (char=? (string-ref string (+fx i 2)) #\')))
		 (and (char=? (string-ref string i) #\')
		      (and (<fx i (- len 1))
			   (not (char=? (string-ref string (+fx i 1)) #\s)))
		      (and (<fx i (- len 2))
			   (not (char=? (string-ref string (+fx i 2))
					#\space))))
		 (memq (string-ref string i) '(#\, #\( #\:)))
	     (loop (+fx i 1) 'up))
	    ((eq? state 'up)
	     (string-set! string i (char-upcase (string-ref string i)))
	     (loop (+fx i 1) 'down))
	    (else
	     (loop (+fx i 1) 'down))))))
				      
;*---------------------------------------------------------------------*/
;*    parse-songs ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-songs songs)
   (define (author-list->string auth)
      (let loop ((auth (reverse! auth)))
	 (cond
	    ((null? auth)
	     "")
	    ((pair? (cdr auth))
	     (string-append (car auth) ", " (loop (cdr auth))))
	    (else
	     (car auth)))))
   (let loop ((songs songs)
	      (res   '()))
      (if (null? songs)
	  (reverse! res)
	  (match-case (car songs)
	     ((?title (author . ?auth) (duration ?dur))
	      (loop (cdr songs)
		    (cons (list (string-good-case title)
				(author-list->string auth)
				(symbol->string dur))
			  res)))
	     ((?title (duration ?dur))
	      (loop (cdr songs)
		    (cons (list title #unspecified (symbol->string dur)) res)))
	     ((?title (author . ?auth))
	      (loop (cdr songs)
		    (cons (list (string-good-case title)
				(author-list->string auth)
				#unspecified)
			  res)))
	     ((or (and (? string?) ?title) (?title))
	      (loop (cdr songs)
		    (cons (list (string-good-case title)
				#unspecified
				#unspecified)
			  res)))
	     (else
	      (error "cdisc" "Illegal song" (car songs)))))))
	     
		
