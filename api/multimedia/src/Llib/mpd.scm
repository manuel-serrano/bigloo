;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/mpd.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  6 15:03:32 2008                          */
;*    Last change :  Sun Aug 17 07:49:08 2014 (serrano)                */
;*    Copyright   :  2008-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Music Player Deamon implementation                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-mpd
   
   (import __multimedia-music
	   __multimedia-id3)
   
   (export (class mpd-database
	      (mpd-database-init!)
	      (directories::pair-nil read-only (default '()))
	      (suffixes::pair-nil (default '("mp3" "flac" "ogg" "wav")))
	      (%base::bstring (default "music"))
	      (%artists::pair-nil (default '()))
	      (%albums::pair-nil (default '()))
	      (%genres::pair-nil (default '()))
	      (%nartists (default 0))
	      (%nalbums::int (default 0))
	      (%nsongs::int (default 0))
	      (%uptime (default #unspecified))
	      (%db-update (default #unspecified)))
	   
	   (mpd ::music ::input-port ::output-port ::mpd-database #!key log)
	   (generic mpd-database-init! ::mpd-database)

	   (generic mpd-database-file->path ::mpd-database ::bstring)
	   (generic mpd-database-stats ::mpd-database ::obj ::obj)
	   (generic mpd-database-listall ::mpd-database ::output-port)
	   (generic mpd-database-listalbum ::mpd-database ::output-port)
	   (generic mpd-database-listartistalbum ::mpd-database ::output-port ::obj)
	   (generic mpd-database-listgenreartist ::mpd-database ::output-port ::obj)
	   (generic mpd-database-listgenrealbum ::mpd-database ::output-port ::obj)
	   (generic mpd-database-listgenre ::mpd-database ::output-port)
	   (generic mpd-database-listartist ::mpd-database ::output-port)
	   (generic mpd-database-lsinfo ::mpd-database ::output-port ::bstring ::bool)
	   (generic mpd-database-find-album ::mpd-database ::output-port ::obj)
	   (generic mpd-database-find-artist ::mpd-database ::output-port ::obj)
	   (generic mpd-database-search-artist-album ::mpd-database ::output-port ::obj ::obj)
	   (generic mpd-database-search-artist-title ::mpd-database ::output-port ::obj ::obj)
	   (generic mpd-database-find-title ::mpd-database ::output-port ::obj)
	   (generic mpd-database-find-genre ::mpd-database ::output-port ::obj)

	   (generic mpd-database-getgenre::pair-nil ::mpd-database)
	   (generic mpd-database-getartist::pair-nil ::mpd-database)
	   (generic mpd-database-getartistalbum ::mpd-database ::obj)
	   (generic mpd-database-getgenreartist::pair-nil ::mpd-database ::obj)
	   (generic mpd-database-get-album::pair-nil ::mpd-database ::obj)))
						  
;*---------------------------------------------------------------------*/
;*    mpd-version ...                                                  */
;*---------------------------------------------------------------------*/
(define (mpd-version) "OK MPD 0.16.0")

;*---------------------------------------------------------------------*/
;*    *mpd-commands* ...                                               */
;*---------------------------------------------------------------------*/
(define *mpd-commands* '())

;*---------------------------------------------------------------------*/
;*    *mpd-notcommands* ...                                            */
;*---------------------------------------------------------------------*/
(define *mpd-notcommands* '())

;*---------------------------------------------------------------------*/
;*    mpd-command ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (define-mpd-command var proto . body)
   
   (define (get-arg arg)
      (case arg
	 ((int num num1 int1 integer integer1)
	  `(get-line-arg-integer-nth line 1))
	 ((num2 int2 integer2)
	  `(get-line-arg-integer-nth line 2))
	 ((uri dir string string1)
	  `(get-line-arg-string-nth line 1))
	 ((string2)
	  `(get-line-arg-string-nth line 2))
	 ((string3)
	  `(get-line-arg-string-nth line 3))
	 ((string4)
	  `(get-line-arg-string-nth line 4))
	 ((sym symbol symbol1)
	  `(get-line-arg-symbol-nth line 1))
	 ((symbol2)
	  `(get-line-arg-symbol-nth line 2))
	 ((symbol3)
	  `(get-line-arg-symbol-nth line 3))
	 ((symbol4)
	  `(get-line-arg-symbol-nth line 4))
	 (else
	  (error 'define-mpd-command "Unknown argument type" arg))))
   
   (let ((fun (match-case proto
		 ((?id)
		  `(lambda (db backend line ip op)
		      ,@body))
		 ((?id ?arg)
		  `(lambda (db backend line ip op)
		      (let ((,arg ,(get-arg arg)))
			 ,@body)))
		 ((?id ?arg1 ?arg2)
		  `(lambda (db backend line ip op)
		      (let ((,arg1 ,(get-arg arg1))
			    (,arg2 ,(get-arg arg2)))
			 ,@body)))
		 ((?id ?arg1 ?arg2 ?arg3)
		  `(lambda (db backend line ip op)
		      (let ((,arg1 ,(get-arg arg1))
			    (,arg2 ,(get-arg arg2))
			    (,arg3 ,(get-arg arg3)))
			 ,@body)))
		 ((?id ?arg1 ?arg2 ?arg3 ?arg4)
		  `(lambda (db backend line ip op)
		      (let ((,arg1 ,(get-arg arg1))
			    (,arg2 ,(get-arg arg2))
			    (,arg3 ,(get-arg arg3))
			    (,arg4 ,(get-arg arg4)))
			 ,@body)))
		 (else
		  (error 'define-mpd-command "Illegal prototype" proto))))
	 (id (symbol-append 'mpd- (car proto))))
      
      `(begin
	  (define ,id ,fun)
	  (set! ,var (append! ,var (list (cons ',(car proto) ,id)))))))

;*---------------------------------------------------------------------*/
;*    define-command ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (define-command proto . body)
   `(define-mpd-command *mpd-commands* ,proto ,@body))
       
;*---------------------------------------------------------------------*/
;*    define-notcommand ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (define-notcommand proto) 
   `(define-mpd-command *mpd-notcommands* ,proto 'ok))

;*---------------------------------------------------------------------*/
;*    string-last-index ...                                            */
;*---------------------------------------------------------------------*/
(define (string-last-index str charset i)
   (let ((j (string-index str charset i)))
      (when j (or (string-skip str charset j) j))))

;*---------------------------------------------------------------------*/
;*    get-line-command ...                                             */
;*---------------------------------------------------------------------*/
(define (get-line-command line)
   (let ((i (string-index line #\space 0)))
      (if i
	  (string->symbol (substring line 0 i))
	  (string->symbol line))))

;*---------------------------------------------------------------------*/
;*    get-line-arg-string-nth ...                                      */
;*---------------------------------------------------------------------*/
(define (get-line-arg-string-nth line num)
   (define (get-string i len)
      (when (<fx i len)
	 (if (char=? #\" (string-ref line i))
	     (let ((j (string-index line #\" (+fx i 1))))
		(if j
		    (substring line (+fx i 1) j)
		    (substring line (+fx i 1) len)))
	     (let ((j (string-index line #\space i)))
		(if j
		    (substring line i j)
		    (substring line i len))))))
   (let ((i0 (string-last-index line #\space 0))
	 (len (string-length line)))
      (when i0
	 (let loop ((n (-fx num 1))
		    (i i0))
	    (cond
	       ((not i)
		#f)
	       ((=fx n 0)
		(when i (get-string i len)))
	       ((<fx i len)
		(loop (-fx n 1) (string-last-index line #\space i))))))))

;*---------------------------------------------------------------------*/
;*    get-line-arg-symbol-nth ...                                      */
;*---------------------------------------------------------------------*/
(define (get-line-arg-symbol-nth line num)
   (define (get-symbol i len)
      (when (<fx i len)
	 (let ((j (string-index line #\space i)))
	    (if j
		(string->symbol (string-downcase! (substring line i j)))
		(string->symbol (string-downcase! (substring line i len)))))))
   (let ((i0 (string-last-index line #\space 0))
	 (len (string-length line)))
      (when i0
	 (let loop ((n (-fx num 1))
		    (i i0))
	    (cond
	       ((not i)
		#f)
	       ((=fx n 0)
		(get-symbol i len))
	       ((<fx i len)
		(loop (-fx n 1) (string-last-index line #\space i))))))))

;*---------------------------------------------------------------------*/
;*    get-line-arg-integer-nth ...                                     */
;*---------------------------------------------------------------------*/
(define (get-line-arg-integer-nth line num)
   (let ((str (get-line-arg-string-nth line num)))
      (when (string? str)
	 (string->integer str))))

(define cmdn 0)
;*---------------------------------------------------------------------*/
;*    mpd ...                                                          */
;*---------------------------------------------------------------------*/
(define (mpd backend::music
	   ip::input-port op::output-port db::mpd-database
	   #!key log)
   (display (mpd-version) op)
   (newline op)
   (flush-output-port op)
   (let loop ()
      (unless (music-closed? backend)
	 (let ((line (read-line ip)))
	    (set! cmdn (+fx 1 cmdn))
	    (when log (log line))
	    (unless (eof-object? line)
	       (let ((v (execute-command db backend ip op line)))
		  (case v
		     ((ok)
		      (mpd-ok op)
		      (flush-output-port op)
		      (loop))
		     ((close kill)
		      (flush-output-port op))
		     ((blank)
		      (loop))
		     (else
		      (mpd-err op v)
		      (loop)))))))))

;*---------------------------------------------------------------------*/
;*    execute-command ...                                              */
;*---------------------------------------------------------------------*/
(define (execute-command db backend ip op line::bstring)
   (let ((cmd (get-line-command line)))
      (cond
	 ((or (eq? cmd 'command_list_begin)
	      (eq? cmd 'command_list_ok_begin))
	  (mpd-list-begin db backend ip op cmd))
	 ((assq cmd *mpd-commands*)
	  =>
	  (lambda (c) ((cdr c) db backend line ip op)))
	 ((assq cmd *mpd-notcommands*)
	  =>
	  (lambda (c) ((cdr c) db backend line ip op)))
	 (else
	  (if (>fx (string-length line) 0)
	      (format "unknown command \"~a\"" line)
	      'blank)))))

;*---------------------------------------------------------------------*/
;*    mpd-list-begin ...                                               */
;*---------------------------------------------------------------------*/
(define (mpd-list-begin db backend ip op cmd)
   (let loop ((lines '()))
      (let ((line (read-line ip)))
	 (unless (eof-object? line)
	    (let ((c (get-line-command line)))
	       (if (eq? c 'command_list_end)
		   ;; execute the commands
		   (if (eq? cmd 'command_list_begin)
		       (let loop ((ls (reverse! lines)))
			  (if (null? ls)
			      'ok
			      (let* ((l (car ls))
				     (v (execute-command db backend ip op l)))
				 (if (eq? v 'ok)
				     (loop (cdr ls))
				     v))))
		       (if (eq? cmd 'command_list_ok_begin)
			   (let loop ((ls (reverse! lines)))
			      (if (null? ls)
				  'ok
				  (let* ((l (car ls))
					 (v (execute-command db backend ip op l)))
				     (if (eq? v 'ok)
					 (begin
					    (display "list_OK\n" op)
					    (loop (cdr ls)))
					 v))))))
		   (loop (cons line lines))))))))

;*---------------------------------------------------------------------*/
;*    mpd-ok ...                                                       */
;*---------------------------------------------------------------------*/
(define (mpd-ok op)
   (display "OK\n" op)
   (flush-output-port op))

;*---------------------------------------------------------------------*/
;*    mpd-not-implemented ...                                          */
;*---------------------------------------------------------------------*/
(define (mpd-not-implemented op)
   (mpd-ok op))

;*---------------------------------------------------------------------*/
;*    mpd-err ...                                                      */
;*---------------------------------------------------------------------*/
(define (mpd-err op msg #!key (err "5@0") (command ""))
   (display "ACK [" op)
   (display err op)
   (display "] {" op)
   (display command op)
   (display "} " op)
   (display msg op)
   (display "\n" op)
   (flush-output-port op)
   'blank)

;*---------------------------------------------------------------------*/
;*    ping ...                                                         */
;*---------------------------------------------------------------------*/
(define-command (ping)
   'ok)

;*---------------------------------------------------------------------*/
;*    urlhandlers ...                                                  */
;*---------------------------------------------------------------------*/
(define-command (urlhandlers)
   (for-each (lambda (handler)
		(display "handler: " op)
		(display handler op)
		(newline op))
      '("http://" "file://")))

;*---------------------------------------------------------------------*/
;*    tagtypes ...                                                     */
;*---------------------------------------------------------------------*/
(define-command (tagtypes)
   (for-each (lambda (tag)
		(display "tagtype: " op)
		(display tag op)
		(newline op))
      '("Artist" "Album" "Title" "Track" "Name" "Genre" "Date" "Composer"
	"Performer" "Disc")))

;*---------------------------------------------------------------------*/
;*    disp ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (disp key val)
   `(begin (display ,key op) (display ,val op) (newline op)))

;*---------------------------------------------------------------------*/
;*    mpd-status ...                                                   */
;*---------------------------------------------------------------------*/
(define-command (status)
   (let ((status (music-status backend)))
      (with-access::musicstatus status (volume song songid state xfade
					  playlistid playlistlength bitrate
					  khz songpos songlength repeat random)
	 (let ((vol (if (vector? volume) (vector-ref volume 0) volume)))
	    (disp "volume: " vol))
	 (disp "state: " state)
	 (disp "playlist: " playlistid)
	 (disp "playlistlength: " playlistlength)
	 (when (>=fx song 0)
	    (disp "song: " song)
	    (disp "songid: " songid)
	    (disp "bitrate: " bitrate)
	    (display "audio: " op)
	    (display khz op)
	    (display ":16:2\n" op))
	 (when (or (eq? state 'play) (eq? state 'pause))
	    (display "time: " op)
	    (display songpos op)
	    (display ":" op)
	    (display songlength op)
	    (newline op))
	 (if repeat
	     (display "repeat: 1\n" op)
	     (display "repeat: 0\n" op))
	 (if random
	     (display "random: 1\n" op)
	     (display "random: 0\n" op))
	 (disp "xfade: " xfade)))
   'ok)

;*---------------------------------------------------------------------*/
;*    playlist ...                                                     */
;*---------------------------------------------------------------------*/
(define-command (playlist)
   (let loop ((plist (music-playlist-get backend))
	      (num 0))
      (when (pair? plist)
	 (display num op)
	 (display ":" op)
	 (display (car plist) op)
	 (newline op)
	 (loop (cdr plist) (+fx 1 num))))
   'ok)

;*---------------------------------------------------------------------*/
;*    infofile ...                                                     */
;*---------------------------------------------------------------------*/
(define (infofile db file op #!key artist album image)
   (with-access::mpd-database db (directories)
      (for-each (lambda (k)
		   (display (keyword->string! (car k)) op)
		   (display ": " op)
		   (display (cadr k) op)
		   (newline op))
	 (getinfofile db file artist album image))))

;*---------------------------------------------------------------------*/
;*    playlistinfo ...                                                 */
;*---------------------------------------------------------------------*/
(define (playlistinfo db backend line ip op num::int)
   (with-access::mpd-database db (directories)
      (let ((plist (music-playlist-get backend)))
	 (if (and (>=fx num 0) (<fx num (length plist)))
	     (infofile db (list-ref plist num) op)
	     (let loop ((plist plist)
			(num 0))
		(when (pair? plist)
		   (infofile db (car plist) op)
		   (loop (cdr plist) (+fx 1 num))))))
      'ok))

;*---------------------------------------------------------------------*/
;*    playlistinfo ...                                                 */
;*---------------------------------------------------------------------*/
(define-command (playlistinfo num)
   (playlistinfo db backend line ip op (or num 0)))
   
;*---------------------------------------------------------------------*/
;*    playlistid ...                                                   */
;*---------------------------------------------------------------------*/
(define-command (playlistid num)
   (playlistinfo db backend line ip op (or num 0)))

;*---------------------------------------------------------------------*/
;*    plchanges ...                                                    */
;*---------------------------------------------------------------------*/
(define-command (plchanges num)
   (let ((status (music-status backend)))
      (with-access::musicstatus status (playlistid)
	 (if (<fx num playlistid)
	     (playlistinfo db backend line ip op 0)
	     'ok))))

;*---------------------------------------------------------------------*/
;*    mpd-database-uri-path ...                                        */
;*---------------------------------------------------------------------*/
(define (mpd-database-uri-path db uri op cmd)
   (if (substring-at? uri "http://" 0)
       uri
       (mpd->file uri db op cmd)))
       
;*---------------------------------------------------------------------*/
;*    add ...                                                          */
;*---------------------------------------------------------------------*/
(define-command (add uri)
   (if uri
       (let ((path (mpd-database-uri-path db uri op "add")))
	  (if (directory? path)
	      (for-each (lambda (f)
			   (when (music-file? f db)
			      (music-playlist-add!
			       backend (make-file-name path f))))
			(directory->sort-list path))
	      (music-playlist-add! backend path))
	  'ok)
       (format "illegal uri \"~a\"" uri)))

;*---------------------------------------------------------------------*/
;*    delete ...                                                       */
;*---------------------------------------------------------------------*/
(define-command (delete num)
   (music-playlist-delete! backend num)
   'ok)

;*---------------------------------------------------------------------*/
;*    deleteid ...                                                     */
;*---------------------------------------------------------------------*/
(define-command (deleteid num)
   (music-playlist-delete! backend num)
   (mpd-ok op))

;*---------------------------------------------------------------------*/
;*    clear ...                                                        */
;*---------------------------------------------------------------------*/
(define-command (clear)
   (music-playlist-clear! backend)
   'ok)

;*---------------------------------------------------------------------*/
;*    currentsong-info ...                                             */
;*---------------------------------------------------------------------*/
(define (currentsong-info db backend plist num line ip)
   
   (define (metasong op file path)
      ;; the file is not local, ask the player the meta data (if any)
      (let ((meta (music-meta backend)))
	 (if (null? meta)
	     (playlistinfo db backend line ip op (music-song backend))
	     (let ((num (music-song backend))
		   (dir (dirname file)))
		(display "file: " op)
		(display path op)
		(newline op)
		(when num
		   (display "Pos: " op)
		   (display num op)
		   (newline op)
		   (display "Id: " op)
		   (display num op)
		   (newline op)
		   (let* ((ca (assq 'artist meta))
			  (ct (assq 'title meta))
			  (cb (assq 'album meta))
			  (a (if (pair? ca)
				 (cdr ca)
				 (string-capitalize (basename (dirname dir)))))
			  (t (if (pair? ct)
				 (cdr ct)
				 (string-capitalize (basename file))))
			  (b (if (pair? cb)
				 (cdr cb)
				 (string-capitalize (basename dir)))))
		      (fprint op "Artist: " a)
		      (fprint op "Title: " t)
		      (fprint op "Album: " b)))))))
   
   (let* ((file (list-ref plist num))
	  (path (uri->mpd file db)))
      (call-with-output-string
	 (lambda (op)
	    (if (file-exists? file)
		(infofile db file op)
		(metasong op file path))))))

;*---------------------------------------------------------------------*/
;*    currentsong-cache ...                                            */
;*---------------------------------------------------------------------*/
(define currentsong-cache-plist #f)
(define currentsong-cache-num -1)
(define currentsong-cache-info "")

;*---------------------------------------------------------------------*/
;*    currentsong/cache ...                                            */
;*---------------------------------------------------------------------*/
(define (currentsong/cache db backend plist num line ip op)
   (if (and (eq? plist currentsong-cache-plist) (=fx currentsong-cache-num num))
       (display currentsong-cache-info op)
       (let ((info (currentsong-info db backend plist num line ip)))
	  (set! currentsong-cache-plist plist)
	  (set! currentsong-cache-num num)
	  (set! currentsong-cache-info info)
	  (display info op))))

;*---------------------------------------------------------------------*/
;*    currentsong ...                                                  */
;*---------------------------------------------------------------------*/
(define-command (currentsong)
   (let ((plist (music-playlist-get backend))
	 (num (music-song backend)))
      (when (and (>=fx num 0) (<fx num (length plist)))
	 (currentsong/cache db backend plist num line ip op))
      'ok))

;*---------------------------------------------------------------------*/
;*    play ...                                                         */
;*---------------------------------------------------------------------*/
(define-command (play num)
   (if (and (integer? num) (>=fx num 0))
       (music-play backend num)
       (music-play backend))
   'ok)

;*---------------------------------------------------------------------*/
;*    playid ...                                                       */
;*---------------------------------------------------------------------*/
(define-command (playid num)
   (if (and (integer? num) (>=fx num 0))
       (music-play backend num)
       (music-play backend))
   'ok)

;*---------------------------------------------------------------------*/
;*    seek ...                                                         */
;*---------------------------------------------------------------------*/
(define-command (seek num1 num2)
   (music-seek backend num2 num1)
   'ok)

;*---------------------------------------------------------------------*/
;*    seekid ...                                                       */
;*---------------------------------------------------------------------*/
(define-command (seekid num1 num2)
   (music-seek backend num2 num1)
   'ok)

;*---------------------------------------------------------------------*/
;*    stop ...                                                         */
;*---------------------------------------------------------------------*/
(define-command (stop)
   (music-stop backend)
   'ok)

;*---------------------------------------------------------------------*/
;*    pause ...                                                        */
;*---------------------------------------------------------------------*/
(define-command (pause)
   (music-pause backend)
   'ok)

;*---------------------------------------------------------------------*/
;*    next ...                                                         */
;*---------------------------------------------------------------------*/
(define-command (next)
   (with-handler
      (lambda (e)
	 (if (isa? e &io-error)
	     (mpd-err op "No more song" :err "50@0" :command "next")
	     (raise e)))
      (begin
	 (music-next backend)
	 'ok)))

;*---------------------------------------------------------------------*/
;*    previous ...                                                     */
;*---------------------------------------------------------------------*/
(define-command (previous)
   (with-handler
      (lambda (e)
	 (if (isa? e &io-error)
	     (mpd-err op "No previous song" :err "50@0" :command "previous")
	     (raise e)))
      (begin
	 (music-prev backend)
	 'ok)))

;*---------------------------------------------------------------------*/
;*    volume ...                                                       */
;*---------------------------------------------------------------------*/
(define-command (volume int)
   (if (integer? int)
       (mpd-music-volume-set! backend ip op int)
       (mpd-music-volume-get backend ip op)))

;*---------------------------------------------------------------------*/
;*    mpd-music-volume-get ...                                         */
;*---------------------------------------------------------------------*/
(define (mpd-music-volume-get backend ip op)
   (display (music-volume-get backend) op)
   (newline op)
   'ok)

;*---------------------------------------------------------------------*/
;*    mpd-music-volume-set! ...                                        */
;*---------------------------------------------------------------------*/
(define (mpd-music-volume-set! backend ip op vol)
   (music-volume-set! backend vol)
   'ok)

;*---------------------------------------------------------------------*/
;*    setvol ...                                                       */
;*---------------------------------------------------------------------*/
(define-command (setvol int)
   (mpd-music-volume-set! backend ip op int))

;*---------------------------------------------------------------------*/
;*    clearerror ...                                                   */
;*---------------------------------------------------------------------*/
(define-command (clearerror)
   'ok)

;*---------------------------------------------------------------------*/
;*    close ...                                                        */
;*---------------------------------------------------------------------*/
(define-command (close)
   'close)

;*---------------------------------------------------------------------*/
;*    stats ...                                                        */
;*---------------------------------------------------------------------*/
(define-command (stats)
   (mpd-database-stats db backend op)
   'ok)

;*---------------------------------------------------------------------*/
;*    listall ...                                                      */
;*---------------------------------------------------------------------*/
(define-command (listall)
   (mpd-database-listall db op)
   'ok)

;*---------------------------------------------------------------------*/
;*    list ...                                                         */
;*---------------------------------------------------------------------*/
(define-command (list sym string2 string3)
   (case sym
      ((album)
       (cond
	  ((equal? string2 "artist")
	   (mpd-database-listartistalbum db op string3))
	  ((equal? string2 "genre")
	   (mpd-database-listgenrealbum db op string3))
	  (else
	   (mpd-database-listartistalbum db op string2)))
       'ok)
      ((artist)
       (if (equal? string2 "genre")
	   (mpd-database-listgenreartist db op string3)
	   (mpd-database-listartist db op))
       'ok)
      ((disc)
       'ok)
      ((genre)
       (mpd-database-listgenre db op)
       'ok)
      (else
       (if (not sym)
	   "ACK [2@0] {list} too few arguments for \"list\""
	   (format "unknown list type \"~a\"" sym)))))

;*---------------------------------------------------------------------*/
;*    listallinfo ...                                                  */
;*---------------------------------------------------------------------*/
(define-command (listallinfo dir)
   (mpd-database-lsinfo db op (or dir (string (file-separator))) #t)
   'ok)

;*---------------------------------------------------------------------*/
;*    lsinfo ...                                                       */
;*---------------------------------------------------------------------*/
(define-command (lsinfo dir)
   (mpd-database-lsinfo db op (or dir (string (file-separator))) #f)
   'ok)

;*---------------------------------------------------------------------*/
;*    plconangesposid ...                                              */
;*---------------------------------------------------------------------*/
(define-command (plconangesposid int)
   (let ((num 0))
      (for-each (lambda (f)
		   (fprint op "cpos: " num)
		   (fprint op "Id: " num)
		   (set! num (+fx num 1)))
		(music-playlist-get backend)))
   'ok)

;*---------------------------------------------------------------------*/
;*    kill ...                                                         */
;*---------------------------------------------------------------------*/
(define-command (kill)
   'kill)

;*---------------------------------------------------------------------*/
;*    find ...                                                         */
;*---------------------------------------------------------------------*/
(define-command (find symbol string2)
   (case symbol
      ((album)
       (mpd-database-find-album db op string2)
       'ok)
      ((artist)
       (mpd-database-find-artist db op string2)
       'ok)
      ((title)
       (mpd-database-find-title db op string2)
       'ok)
      ((genre)
       (mpd-database-find-genre db op string2)
       'ok)
      (else
       (format "unknown find type \"~a\"" symbol))))

;*---------------------------------------------------------------------*/
;*    search ...                                                       */
;*---------------------------------------------------------------------*/
(define-command (search symbol1 string2 symbol3 string4)
   (case symbol1
      ((album)
       (mpd-database-find-album db op string2)
       'ok)
      ((artist)
       (case symbol3
	  ((album)
	   (mpd-database-search-artist-album db op string2 string4))
	  ((title)
	   (mpd-database-search-artist-title db op string2 string4))
	  (else
	   (mpd-database-find-artist db op string2)))
       'ok)
      ((title)
       (mpd-database-find-title db op string2)
       'ok)
      ((genre)
       (mpd-database-find-genre db op string2)
       'ok)
      ((any)
       (mpd-listall db backend line ip op))
      (else
       (format "unknown search type \"~a\"" symbol1))))

;*---------------------------------------------------------------------*/
;*    commands ...                                                     */
;*---------------------------------------------------------------------*/
(define-command (commands)
   (for-each (lambda (c)
		(display "command: " op)
		(display (car c) op)
		(newline op))
	     (sort (lambda (c1 c2)
		      (string<? (symbol->string (car c1))
				(symbol->string (car c2))))
		   *mpd-commands*))
   'ok)

;*---------------------------------------------------------------------*/
;*    notcommands ...                                                  */
;*---------------------------------------------------------------------*/
(define-command (notcommands)
   (for-each (lambda (c)
		(display "command: " op)
		(display (car c) op)
		(newline op))
	     (sort (lambda (c1 c2)
		      (string<? (symbol->string (car c1))
				(symbol->string (car c2))))
		   *mpd-notcommands*))
   'ok)

;*---------------------------------------------------------------------*/
;*    addid ...                                                        */
;*---------------------------------------------------------------------*/
(define-notcommand (addid))

;*---------------------------------------------------------------------*/
;*    crossfade ...                                                    */
;*---------------------------------------------------------------------*/
(define-notcommand (crossfade))
   
;*---------------------------------------------------------------------*/
;*    move ...                                                         */
;*---------------------------------------------------------------------*/
(define-notcommand (move))
   
;*---------------------------------------------------------------------*/
;*    moveid ...                                                       */
;*---------------------------------------------------------------------*/
(define-notcommand (moveid))
   
;*---------------------------------------------------------------------*/
;*    password ...                                                     */
;*---------------------------------------------------------------------*/
(define-notcommand (password))
   
;*---------------------------------------------------------------------*/
;*    random ...                                                       */
;*---------------------------------------------------------------------*/
(define-notcommand (random))
   
;*---------------------------------------------------------------------*/
;*    repeat ...                                                       */
;*---------------------------------------------------------------------*/
(define-notcommand (repeat))
   
;*---------------------------------------------------------------------*/
;*    rm ...                                                           */
;*---------------------------------------------------------------------*/
(define-notcommand (rm))
   
;*---------------------------------------------------------------------*/
;*    save ...                                                         */
;*---------------------------------------------------------------------*/
(define-notcommand (save))
   
;*---------------------------------------------------------------------*/
;*    shuffle ...                                                      */
;*---------------------------------------------------------------------*/
(define-notcommand (shuffle))
   
;*---------------------------------------------------------------------*/
;*    swap ...                                                         */
;*---------------------------------------------------------------------*/
(define-notcommand (swap))
   
;*---------------------------------------------------------------------*/
;*    swapid ...                                                       */
;*---------------------------------------------------------------------*/
(define-notcommand (swapid))
   
;*---------------------------------------------------------------------*/
;*    update ...                                                       */
;*---------------------------------------------------------------------*/
(define-notcommand (update))
   
;*---------------------------------------------------------------------*/
;*    outputs ...                                                      */
;*---------------------------------------------------------------------*/
(define-notcommand (outputs))

;*---------------------------------------------------------------------*/
;*    longest-prefix ...                                               */
;*---------------------------------------------------------------------*/
(define (longest-prefix l1 l2)
   (let loop ((l1 l1)
	      (l2 l2))
      (cond
	 ((or (null? l1) (null? l2))
	  '())
	 ((string=? (car l1) (car l2))
	  (cons (car l1) (loop (cdr l1) (cdr l2))))
	 (else
	  '()))))

;*---------------------------------------------------------------------*/
;*    file-children? ...                                               */
;*---------------------------------------------------------------------*/
(define (file-children? file dir)
   (when (string-prefix? dir file)
      (let ((blen (string-length dir))
	    (bfile (string-length file)))
	 (or (=fx blen bfile)
	     (and (>fx bfile blen)
		  (char=? (string-ref bfile blen)
		     (char=? (string-ref dir 0) (file-separator))))))))

;*---------------------------------------------------------------------*/
;*    file-system-root? ...                                            */
;*---------------------------------------------------------------------*/
(define (file-system-root? dir)
   (and (=fx (string-length dir) 1)
	(char=? (string-ref dir 0) (file-separator))))

;*---------------------------------------------------------------------*/
;*    mpd-database-init! ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-init! o::mpd-database)
   (with-access::mpd-database o (directories
				 %base %albums %artists %genres
				 %nartists %nalbums %nsongs
				 %uptime %db-update)
      ;; find a good base directory
      (when (pair? directories)
	 (if (null? (cdr directories))
	     (set! %base (car directories))
	     (let loop ((dirs (cdr directories))
			(l (file-name->list (car directories))))
		(if (null? dirs)
		    (set! %base (apply make-file-path l))
		    (let ((nl (file-name->list (dirname (car directories)))))
		       (loop (cdr dirs) (longest-prefix l nl)))))))
      ;; set the DB creation time
      (set! %db-update (- (current-seconds) (date->seconds (make-date))))
      (set! %uptime (- (current-seconds) (date->seconds (make-date))))
      ;; prepare for DB stats
      (multiple-value-bind (artists albums genres nsong)
	 (mpd-database-directories-scan o directories)
	 (set! %artists artists)
	 (set! %nartists (length artists))
	 (set! %albums albums)
	 (set! %genres genres)
	 (set! %nalbums (length albums))
	 (set! %nsongs nsong))
      o))

;*---------------------------------------------------------------------*/
;*    directory->sort-list ...                                         */
;*---------------------------------------------------------------------*/
(define (directory->sort-list dir)
   (sort (lambda (s1 s2)
	    (<fx (string-natural-compare3 s1 s2) 0))
	 (directory->list dir)))

;*---------------------------------------------------------------------*/
;*    mpd->file ...                                                    */
;*---------------------------------------------------------------------*/
(define (mpd->file file db op cmd)
   (with-access::mpd-database db (directories)
      (let ((f (any (lambda (dir)
		       (let ((p (make-file-name dir file)))
			  (when (file-exists? p)
			     p)))
		  directories)))
	 (if (string? f)
	     f
	     (mpd-err op "No such file" :err "50@0" :command cmd)))))

;*---------------------------------------------------------------------*/
;*    file->mpd ...                                                    */
;*---------------------------------------------------------------------*/
(define (file->mpd file db)
   (with-access::mpd-database db (%base)
      (let ((blen (string-length %base)))
	 (if (>fx (string-length file) blen)
	     (substring file (+fx blen 1))
	     ""))))
   
;*---------------------------------------------------------------------*/
;*    uri->mpd ...                                                     */
;*---------------------------------------------------------------------*/
(define (uri->mpd uri db)
   (if (substring-at? uri "http://" 0)
       uri
       (file->mpd uri db)))

;*---------------------------------------------------------------------*/
;*    image-file? ...                                                  */
;*---------------------------------------------------------------------*/
(define (image-file? f)
   (any (lambda (s) (string-suffix? s f)) '("jpg" "JPG" "png" "gif" "GIF")))

;*---------------------------------------------------------------------*/
;*    directory-image ...                                              */
;*---------------------------------------------------------------------*/
(define (directory-image dir)
   (let ((f (find image-file? (directory->list dir))))
      (when (string? f)
	 (make-file-name dir f))))

;*---------------------------------------------------------------------*/
;*    music-file? ...                                                  */
;*---------------------------------------------------------------------*/
(define (music-file? f db)
   (with-access::mpd-database db (suffixes)
      (any (lambda (s) (string-suffix? s f)) suffixes)))

;*---------------------------------------------------------------------*/
;*    directory-contains-music? ...                                    */
;*---------------------------------------------------------------------*/
(define (directory-contains-music? dir db)
   (any (lambda (d) (music-file? d db) (directory->list dir))))

;*---------------------------------------------------------------------*/
;*    mpd-database-directories-scan ...                                */
;*---------------------------------------------------------------------*/
(define (mpd-database-directories-scan db directories)
   
   (define artists (make-hashtable))
   (define albums (make-hashtable))
   (define genres (make-hashtable))
   (define nsongs 0)
   
   (define (directory-scan dir repo)
      (let* ((album (basename dir))
	     (dir2 (dirname dir))
	     (artist (basename dir2))
	     (dir3 (dirname dir2))
	     (genre (basename dir3))
	     (add #f))
	 (for-each (lambda (p)
		      (cond
			 ((directory? p)
			  (directory-scan p repo))
			 ((music-file? p db)
			  (set! nsongs (+fx nsongs 1))
			  (unless add
			     (set! add #t)
			     (hashtable-put! artists artist dir2)
			     (hashtable-put! albums album dir)
			     (hashtable-put! genres genre dir3)))))
		   (directory->path-list dir))))
   
   (for-each (lambda (d) (directory-scan d d)) directories)
   
   (values (sort (lambda (e1 e2) (string<? (car e1) (car e2)))
		 (hashtable-map artists cons))
	   (sort (lambda (e1 e2) (string<? (car e1) (car e2)))
		 (hashtable-map albums cons))
	   (sort (lambda (e1 e2) (string<? (car e1) (car e2)))
		 (hashtable-map genres cons))
	   nsongs))

;*---------------------------------------------------------------------*/
;*    mpd-database-file->path ::mpd-database ...                       */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-file->path o::mpd-database file)
   (with-access::mpd-database o (%base directories)
      (if (null? directories)
	  file
	  (let* ((dir (car (file-name->list file)))
		 (n (string-contains (car directories) dir)))
	     (if (integer? n)
		 (string-append (substring (car directories) 0 n) file)
		 file)))))

;*---------------------------------------------------------------------*/
;*    mpd-database-stats ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-stats o::mpd-database backend op)

   (define (playtime)
      (let ((plist (music-playlist-get backend))
	    (num (music-song backend)))
	 (when (and (>=fx num 0) (<fx num (length plist)))
	    (let* ((file (list-ref plist num))
		   (path (uri->mpd file o)))
	       (if (file-exists? file)
		   (let ((info (file-musicinfo file)))
		      (if (isa? info musicinfo)
			  (with-access::musicinfo info (duration)
			     duration)
			  1000000))
		   1000000)))))
   
   (with-access::mpd-database o (%nartists %nalbums %nsongs %uptime %db-update)
      (fprintf op "artists: ~a
albums: ~a
songs: ~a
uptime: ~a
playtime: ~a
db_playtime: 10000000
db_update: ~a\n"
	 %nartists
	 %nalbums
	 %nsongs
	 (elong->fixnum (-elong (current-seconds) %uptime))
	 (playtime)
	 (elong->fixnum (-elong (current-seconds) %db-update)))))

;*---------------------------------------------------------------------*/
;*    mpd-database-listall ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listall o::mpd-database op)
   
   (define (directory-list path)
      (let loop ((path path))
	 (if (directory? path)
	     (begin
		(display "directory: " op)
		(display (file->mpd path o) op)
		(newline op)
		(for-each (lambda (f)
			     (loop (make-file-name path f)))
		   (directory->sort-list path)))
	     (begin
		(display "file: " op)
		(display (file->mpd path o) op)
		(newline op)))))
   
   (with-access::mpd-database o (directories %base)
      (display "directory: " op)
      (display %base op)
      (newline op)
      (for-each directory-list directories)))

;*---------------------------------------------------------------------*/
;*    mpd-database-listalbum ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listalbum o::mpd-database op)
   (for-each (lambda (a)
		(display "Album: " op)
		(display (car a) op)
		(newline op))
      (with-access::mpd-database o (%albums)
	 %albums)))

;*---------------------------------------------------------------------*/
;*    mpd-database-listartistalbum ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listartistalbum o::mpd-database op artist)
   (for-each (lambda (a)
		(display "Album: " op)
		(display (cadr a) op)
		(newline op))
      (mpd-database-getartistalbum o artist)))

;*---------------------------------------------------------------------*/
;*    mpd-database-listgenrealbum ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listgenrealbum o::mpd-database op genre)
   (for-each (lambda (a)
		(display "Album: " op)
		(display (car a) op)
		(newline op))
      (filter (lambda (c)
		 (let ((dir (cdr c)))
		    (string=? (basename (dirname (dirname dir))) genre)))
	 (with-access::mpd-database o (%albums)
	    %albums))))

;*---------------------------------------------------------------------*/
;*    mpd-database-listgenreartist ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listgenreartist o::mpd-database op genre)
   (for-each (lambda (a)
		(display "Artist: " op)
		(display (car a) op)
		(newline op))
      (mpd-database-getgenreartist o genre)))

;*---------------------------------------------------------------------*/
;*    mpd-database-listgenre ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listgenre o::mpd-database op)
   (for-each (lambda (a)
		(display "Genre: " op)
		(display (car a) op)
		(newline op))
      (mpd-database-getgenre o)))

;*---------------------------------------------------------------------*/
;*    mpd-database-listartist ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listartist o::mpd-database op)
   (for-each (lambda (a)
		(display "Artist: " op)
		(display (car a) op)
		(newline op))
      (mpd-database-getartist o)))

;*---------------------------------------------------------------------*/
;*    mpd-database-lsinfo ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-lsinfo o::mpd-database op dir::bstring rec)
   (with-access::mpd-database o (%base %prefixes %roots directories suffixes)
      (if (or (file-system-root? dir) (string=? dir %base))
	  (for-each (lambda (dir)
		       (for-each (lambda (dir)
				    (let ((md (file->mpd dir o)))
				       (display "directory: " op)
				       (display md op)
				       (newline op)
				       (when rec
					  (mpd-database-lsinfo o op md rec))))
			  (directory->path-list dir)))
	     directories)
	  (let ((path (mpd->file dir o op "lsinfo")))
	     (when (directory? path)
		(let* ((files (directory->sort-list path))
		       (img (let ((f (find image-file? files)))
			       (when (string? f)
				  (make-file-name path f)))))
		   (for-each (lambda (f)
				(let ((p (make-file-name path f)))
				   (cond
				      ((directory? p)
				       (let ((md (file->mpd p o)))
					  (display "directory: " op)
					  (display md op)
					  (newline op)
					  (when rec
					     (mpd-database-lsinfo o op md rec))))
				      ((music-file? f o)
				       (infofile o p op :image img)))))
		      files)))))))

;*---------------------------------------------------------------------*/
;*    find-music-file ...                                              */
;*---------------------------------------------------------------------*/
(define (find-music-file db dir op artist album)
   (let loop ((dir dir))
      (let* ((artist (or artist (basename (dirname dir))))
	     (album (or album (basename dir)))
	     (files (sort (lambda (s1 s2)
			     (<fx (string-natural-compare3 s1 s2) 0))
		       (directory->list dir)))
	     (img (let ((f (find image-file? files)))
		     (when (string? f)
			(make-file-name dir f)))))
	 (for-each (lambda (f)
		      (let ((p (make-file-name dir f)))
			 (cond
			    ((directory? p)
			     (loop p))
			    ((music-file? p db)
			     (infofile db p op
				:artist artist
				:album album
				:image img)))))
	    files))))

;*---------------------------------------------------------------------*/
;*    mpd-database-find-album ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-find-album o::mpd-database op album)
   (with-access::mpd-database o (%albums)
      (let ((c (assoc album %albums)))
	 (when (pair? c)
	    (find-music-file o (cdr c) op #f album)))))

;*---------------------------------------------------------------------*/
;*    mpd-database-find-artist ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-find-artist o::mpd-database op artist)
   (with-access::mpd-database o (%artists)
      (let ((c (assoc artist %artists)))
	 (when (pair? c)
	    (find-music-file o (cdr c) op artist #f)))))

;*---------------------------------------------------------------------*/
;*    mpd-database-search-artist-album ...                             */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-search-artist-album o::mpd-database op artist album)
   (define (search-music-album-file db dir op)
      (let loop ((dir dir))
	 (if (string=? (basename dir) album)
	     (let* ((files (sort (lambda (s1 s2)
				    (<fx (string-natural-compare3 s1 s2) 0))
			      (directory->list dir)))
		    (img (let ((f (find image-file? files)))
			    (when (string? f)
			       (make-file-name dir f)))))
		(for-each (lambda (f)
			     (let ((p (make-file-name dir f)))
				(cond
				   ((directory? p)
				    (loop p))
				   ((music-file? p db)
				    (infofile db p op
				       :artist artist
				       :album album
				       :image img)))))
		   files))
	     (for-each (lambda (p)
			  (when (directory? p)
			     (loop p)))
		       (directory->path-list dir)))))
   (with-access::mpd-database o (%artists)
      (let ((c (assoc artist %artists)))
	 (when (pair? c)
	    (search-music-album-file o (cdr c) op)))))

;*---------------------------------------------------------------------*/
;*    mpd-database-search-artist-title ...                             */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-search-artist-title o::mpd-database op artist title)
   
   (define (search-music-album-file db dir op)
      (let loop ((dir dir))
	 (let* ((files (directory->path-list dir))
		(img (find image-file? files)))
	    (for-each (lambda (f)
			 (let ((p (make-file-name dir f)))
			    (cond
			       ((directory? p)
				(loop p))
			       ((and (music-file? p db) (string=? title (prefix f)))
				(infofile db p op
				   :artist artist
				   :album title
				   :image img)))))
	       files))))
   
   (with-access::mpd-database o (%artists)
      (let ((c (assoc artist %artists)))
	 (when (pair? c)
	    (search-music-album-file o (cdr c) op)))))

;*---------------------------------------------------------------------*/
;*    mpd-database-find-title ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-find-title o::mpd-database op title)
   
   (define (find-title dir)
      (cond
	 ((directory? dir)
	  (any find-title (directory->path-list dir)))
	 ((string=? (prefix (basename dir)) title)
	  (infofile o dir op)
	  #t)
	 (else
	  #f)))
   
   (with-access::mpd-database o (directories)
      (any find-title directories)))

;*---------------------------------------------------------------------*/
;*    mpd-database-find-genre ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-find-genre o::mpd-database op genre)
   
   (define (find-genre dir)
      (cond
	 ((directory? dir)
	  (any find-genre (directory->path-list dir)))
	 ((string=? (basename (dirname dir)) genre)
	  (infofile o dir op)
	  #t)
	 (else
	  #f)))
   
   (with-access::mpd-database o (directories)
      (any find-genre directories)))

;*---------------------------------------------------------------------*/
;*    mpd-database-getgenre ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-getgenre o::mpd-database)
   (with-access::mpd-database o (%genres)
      %genres))

;*---------------------------------------------------------------------*/
;*    mpd-database-getartist ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-getartist o::mpd-database)
   (with-access::mpd-database o (%artists)
      %artists))

;*---------------------------------------------------------------------*/
;*    mpd-database-getgenreartist ::mpd-database ...                   */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-getgenreartist o::mpd-database genre)
   (with-access::mpd-database o (%artists)
      (filter (lambda (c)
		 (let ((dir (cdr c)))
		    (string=? (basename (dirname dir)) genre)))
	 %artists)))

;*---------------------------------------------------------------------*/
;*    mpd-database-getartistalbum ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-getartistalbum o::mpd-database artist)
   (with-access::mpd-database o (%albums)
      (if (string? artist)
	  (filter-map (lambda (c)
			 (let ((dir (cdr c)))
			    (when (string=? (basename (dirname dir)) artist)
			       `(album: ,(car c)))))
	     %albums)
	  (map (lambda (a) `(album: ,(car a))) %albums))))

;*---------------------------------------------------------------------*/
;*    getinfofile ...                                                  */
;*---------------------------------------------------------------------*/
(define (getinfofile db file #!optional artist album image)
   
   (define (last-modified dt)
      (format "~a-~2,0d-~2,0dT~2,0d:~2,0d:~2,0dZ"
	 (date-year dt) (date-month dt) (date-day dt)
	 (date-hour dt) (date-minute dt) (date-second dt)))
   
   (define (file-props file)
      (let ((dir (dirname file)))
	 `((Artist: ,(or artist (string-capitalize (basename (dirname dir)))))
	   (Title: ,(prefix (basename file)))
	   (Album: ,(or album (string-capitalize (basename dir)))))))
   
   (define (tag-props file tag)
      (with-access::musictag tag ((ar artist)
				  (al album)
				  title track genre year)
	 (let* ((dir (dirname file))
		(at (or artist
			(if (string-ci=? ar "unknown")
			    (string-capitalize (basename (dirname dir)))
			    ar)))
		(al (or album
			(if (string-ci=? al "Unknown Disc")
			    (string-capitalize (basename dir))
			    al))))
	    `((Artist: ,at)
	      (Title: ,title)
	      (Album: ,al)
	      (Track: ,track)
	      (Date: ,year)
	      (Genre: ,genre)))))
   
   (if (or (not (file-exists? file)) (directory? file))
       `((file: ,(uri->mpd file db)))
       (let ((tag (file-musictag file))
	     (info (file-musicinfo file))
	     (dt (seconds->date (file-modification-time file)))
	     (img (if (string? image) (directory-image (dirname file)))))
	  `((file: ,(uri->mpd file db))
	    (Last-Modified: ,(last-modified dt))
	    ,@(if (isa? info musicinfo)
		  (with-access::musicinfo info (duration)
		     `((Time: ,duration)))
		  '())
	    ,@(if (isa? tag musictag)
		  (tag-props file tag)
		  (file-props file))
	    ,@(if (string? img)
		  `((Image: ,img))
		  '())))))

;*---------------------------------------------------------------------*/
;*    get-music-file ...                                               */
;*---------------------------------------------------------------------*/
(define (get-music-file db dir artist album)
   (let loop ((dir dir))
      (let ((artist (or artist (basename (dirname dir))))
	    (album (or album (basename dir))))
	 (map (lambda (f)
		 (let ((p (make-file-name dir f)))
		    (cond
		       ((directory? p)
			(loop p))
		       ((music-file? p db)
			(getinfofile db p artist album)))))
	    (sort (lambda (s1 s2)
		     (<fx (string-natural-compare3 s1 s2) 0))
	       (directory->list dir))))))

;*---------------------------------------------------------------------*/
;*    mpd-database-get-album ::mpd-database ...                        */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-get-album o::mpd-database album)
   (with-access::mpd-database o (%albums)
      (let ((c (assoc album %albums)))
	 (when (pair? c)
	    (get-music-file o (cdr c) #f album)))))
