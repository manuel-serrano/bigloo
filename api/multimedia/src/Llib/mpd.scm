;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/mpd.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  6 15:03:32 2008                          */
;*    Last change :  Thu Mar 18 06:33:48 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
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
	      (directories::pair-nil read-only)
	      (%base::bstring (default "music"))
	      (%roots read-only (default (make-hashtable 10)))
	      (%prefixes read-only (default (make-hashtable 10)))
	      (%artists::pair-nil (default '()))
	      (%albums::pair-nil (default '()))
	      (%genres::pair-nil (default '()))
	      (%nartists (default 0))
	      (%nalbums::int (default 0))
	      (%nsongs::int (default 0))
	      (%uptime (default #unspecified))
	      (%db-update (default #unspecified)))
	   
	   (mpd ::music ::input-port ::output-port ::mpd-database #!key log)
	   (mpd-database-init! ::mpd-database)

	   (generic mpd-database-file->path ::mpd-database ::bstring)
	   (generic mpd-database-stats ::mpd-database ::obj)
	   (generic mpd-database-listall ::mpd-database ::output-port)
	   (generic mpd-database-listalbum ::mpd-database ::output-port)
	   (generic mpd-database-listartistalbum ::mpd-database ::output-port ::obj)
	   (generic mpd-database-listgenrealbum ::mpd-database ::output-port ::obj)
	   (generic mpd-database-listgenre ::mpd-database ::output-port)
	   (generic mpd-database-listartist ::mpd-database ::output-port)
	   (generic mpd-database-lsinfo ::mpd-database ::output-port ::obj)
	   (generic mpd-database-find-album ::mpd-database ::output-port ::obj)
	   (generic mpd-database-find-artist ::mpd-database ::output-port ::obj)
	   (generic mpd-database-search-artist-album ::mpd-database ::output-port ::obj ::obj)
	   (generic mpd-database-search-artist-title ::mpd-database ::output-port ::obj ::obj)
	   (generic mpd-database-find-title ::mpd-database ::output-port ::obj)
	   (generic mpd-database-find-genre ::mpd-database ::output-port ::obj)))
						  
;*---------------------------------------------------------------------*/
;*    mpd-version ...                                                  */
;*---------------------------------------------------------------------*/
(define (mpd-version) "OK MPD 0.13.0")

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
;*    get-line-command ...                                             */
;*---------------------------------------------------------------------*/
(define (get-line-command line)
   (let ((i (string-index line #\space)))
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
   (let ((i0 (string-index line #\space))
	 (len (string-length line)))
      (when i0
	 (let loop ((n (-fx num 1))
		    (i i0))
	    (cond
	       ((not i)
		#f)
	       ((=fx n 0)
		(when i (get-string (+fx i 1) len)))
	       ((<fx i len)
		(loop (-fx n 1) (string-index line #\space (+fx i 1)))))))))

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
   (let ((i0 (string-index line #\space))
	 (len (string-length line)))
      (when i0
	 (let loop ((n (-fx num 1))
		    (i i0))
	    (cond
	       ((not i)
		#f)
	       ((=fx n 0)
		(get-symbol (+fx i 1) len))
	       ((<fx i len)
		(loop (-fx n 1) (string-index line #\space (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    get-line-arg-integer-nth ...                                     */
;*---------------------------------------------------------------------*/
(define (get-line-arg-integer-nth line num)
   (let ((str (get-line-arg-string-nth line num)))
      (when (string? str)
	 (string->integer str))))

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
;*    disp ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (disp key val)
   `(begin (display ,key op) (display ,val op) (newline op)))

;*---------------------------------------------------------------------*/
;*    mpd-status ...                                                   */
;*---------------------------------------------------------------------*/
(define-command (status)
   (let ((status (music-status backend)))
      (with-access::musicstatus status (volume song songid state)
	 (let ((vol (if (vector? volume) (vector-ref volume 0) volume)))
	    (disp "volume: " vol))
	 (disp "state: " state)
	 (disp "playlist: " (musicstatus-playlistid status))
	 (disp "playlistlength: " (musicstatus-playlistlength status))
	 (when (>=fx song 0)
	    (disp "song: " song)
	    (disp "songid: " songid)
	    (disp "bitrate: " (musicstatus-bitrate status))
	    (display "audio: " op)
	    (display (musicstatus-khz status) op)
	    (display ":16:2\n" op))
	 (when (or (eq? state 'play) (eq? state 'pause))
	    (display "time: " op)
	    (display (musicstatus-songpos status) op)
	    (display ":" op)
	    (display (musicstatus-songlength status) op)
	    (newline op))
	 (if (musicstatus-repeat status)
	     (display "repeat: 1\n" op)
	     (display "repeat: 0\n" op))
	 (if (musicstatus-random status)
	     (display "random: 1\n" op)
	     (display "random: 0\n" op))
	 (disp "xfade: " (musicstatus-xfade status))))
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
(define (infofile db file num op #!optional artist album)
   (with-access::mpd-database db (directories)
      (display "file: " op)
      (let ((path (uri->mpd file db)))
	 (display path op)
	 (newline op)
	 (when num
	    (display "Pos: " op)
	    (display num op)
	    (newline op)
	    (display "Id: " op)
	    (display num op)
	    (newline op))
	 (let ((tag (and (file-exists? file)
			 (not (directory? file))
			 (file-musictag file)))
	       (dir (dirname file)))
	    (if (musictag? tag)
		(with-access::musictag tag ((ar artist) title (al album) track)
		   (let ((a (or artist
				(if (string-ci=? ar "unknown")
				    (string-capitalize (basename (dirname dir)))
				    ar))))
		      (fprint op "Artist: " a))
		   (fprint op "Title: " title)
		   (let ((a (or album
				(if (string-ci=? al "Unknown Disc")
				    (string-capitalize (basename dir))
				    al))))
		      (fprint op "Album: " a))
		   (when (>=fx track 0)
		      (fprint op "Track: " track)))
		(let ((dir (dirname file)))
		   (fprint op "Artist: " (or artist
					     (string-capitalize
					      (basename (dirname dir)))))
		   (fprint op "Title: " (prefix (basename file)))
		   (fprint op "Album: " (or album
					    (string-capitalize
					     (basename dir))))))))))

;*---------------------------------------------------------------------*/
;*    playlistinfo ...                                                 */
;*---------------------------------------------------------------------*/
(define (playlistinfo db backend line ip op num::int)
   (with-access::mpd-database db (directories)
      (let ((plist (music-playlist-get backend)))
	 (if (and (>=fx num 0) (<fx num (length plist)))
	     (infofile db (list-ref plist num) num op)
	     (let loop ((plist plist)
			(num 0))
		(when (pair? plist)
		   (infofile db (car plist) num op)
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
(define (mpd-database-uri-path db uri)
   (if (substring-at? uri "http://" 0)
       uri
       (mpd->file uri db)))
       
;*---------------------------------------------------------------------*/
;*    add ...                                                          */
;*---------------------------------------------------------------------*/
(define-command (add uri)
   (if uri
       (let ((path (mpd-database-uri-path db uri)))
	  (if (directory? path)
	      (for-each (lambda (f)
			   (when (music-file? f)
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
;*    currentsong ...                                                  */
;*---------------------------------------------------------------------*/
(define-command (currentsong)
   
   (define (metasong file path)
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
   
   (let ((plist (music-playlist-get backend))
	 (num (music-song backend)))
      (when (and (>=fx num 0) (<fx num (length plist)))
	 (let* ((file (list-ref plist num))
		(path (uri->mpd file db)))
	    (if (file-exists? path)
		(infofile db file num op)
		(metasong file path))))
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
	 (if (&io-error? e)
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
	 (if (&io-error? e)
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
   (mpd-database-stats db op)
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
       (mpd-database-listartist db op)
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
;*    lsinfo ...                                                       */
;*---------------------------------------------------------------------*/
(define-command (lsinfo dir)
   (mpd-database-lsinfo db op dir)
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
;*    mpd-database-init! ...                                           */
;*---------------------------------------------------------------------*/
(define (mpd-database-init! o::mpd-database)
   (with-access::mpd-database o (directories
				 %base %roots %prefixes
				 %albums %artists %genres
				 %nartists %nalbums %nsongs
				 %uptime %db-update)
      ;; find a good base directory
      (when (pair? directories)
	 (set! %base (basename (dirname (car directories)))))
      ;; set the DB roots prefix
      (for-each (lambda (d)
		   (let ((l (reverse (file-name->list d))))
		      (let loop ((f (make-file-name %base (car l)))
				 (b (cdr l)))
			 (let ((o (hashtable-get %prefixes f)))
			    (if o
				(when (pair? b)
				   (loop (string-append (car b) "-" f) (cdr b)))
				(begin
				   (hashtable-put! %prefixes f d)
				   (hashtable-put! %roots d f)))))))
		directories)
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
(define (mpd->file file db)
   (with-access::mpd-database db (%prefixes)
      (match-case (file-name->list file)
	 ((?base ?dir . ?rest)
	  (let* ((prefix (make-file-name base dir))
		 (root (hashtable-get %prefixes prefix)))
	     (if (not (string? root))
		 (error 'mpd "Illegal file" file)
		 (let ((len (string-length root)))
		    (if (null? rest)
			(values root len)
			(values (apply make-file-path root rest) len))))))
	 (else
	  (error 'mpd "Illegal file" file)))))

;*---------------------------------------------------------------------*/
;*    file->mpd ...                                                    */
;*---------------------------------------------------------------------*/
(define (file->mpd file db offset)
   (with-access::mpd-database db (%roots)
      (if (=fx offset 0)
	  (let ((root (hashtable-get %roots file)))
	     (if (string? root)
		 root
		 (error 'file->mpd "Illegal file" file)))
	  (let* ((dir (substring file 0 offset))
		 (root (hashtable-get %roots dir)))
	     (if (string? root)
		 (let ((len (string-length file)))
		    (if (=fx len offset)
			root
			(let ((s (substring file (+fx offset 1) len)))
			   (make-file-name root s))))
		 (error 'file->mpd "Illegal file" file))))))
   
;*---------------------------------------------------------------------*/
;*    find-mpd-file-offset ...                                         */
;*---------------------------------------------------------------------*/
(define (find-mpd-file-offset file db)
   (with-access::mpd-database db (directories)
      (let loop ((dirs directories))
	 (if (null? dirs)
	     0
	     (if (substring-at? file (car dirs) 0)
		 (string-length (car dirs))
		 (loop (cdr dirs)))))))

;*---------------------------------------------------------------------*/
;*    uri->mpd ...                                                     */
;*---------------------------------------------------------------------*/
(define (uri->mpd uri db)
   (if (substring-at? uri "http://" 0)
       uri
       (file->mpd uri db (find-mpd-file-offset uri db))))

;*---------------------------------------------------------------------*/
;*    music-file? ...                                                  */
;*---------------------------------------------------------------------*/
(define (music-file? f)
   (or (string-suffix? ".mp3" f)
       (string-suffix? ".ogg" f)
       (string-suffix? ".wav" f)
       (string-suffix? ".flac" f)))

;*---------------------------------------------------------------------*/
;*    directory-contains-music? ...                                    */
;*---------------------------------------------------------------------*/
(define (directory-contains-music? dir)
   (any? music-file? (directory->list dir)))

;*---------------------------------------------------------------------*/
;*    directories-listalbum ...                                        */
;*---------------------------------------------------------------------*/
(define (directories-listalbum directories)

   (define count 0)
   
   (define (directory-listalbum dir)
      (let ((len (+fx 1 (string-length (dirname dir))))
	    (op (current-output-port)))
	 (let loop ((dir dir))
	    (for-each (lambda (d)
			 (let ((p (make-file-name dir d)))
			    (when (directory? p)
			       (when (directory-contains-music? p)
				  (set! count (+fx 1 count))
				  (display "Album: " op)
				  (display (basename p) op)
				  (newline op))
			       (loop p))))
		      (sort (lambda (s1 s2)
			       (<fx (string-natural-compare3 s1 s2) 0))
			    (directory->list dir))))))

   (let ((s (with-output-to-string
	       (lambda ()
		  (for-each directory-listalbum directories)))))
      (values s count)))

;*---------------------------------------------------------------------*/
;*    directories-listartist ...                                       */
;*---------------------------------------------------------------------*/
(define (directories-listartist directories)
   
   (define count 0)
   
   (define (directory-listartist dir)
      (let ((len (+fx 1 (string-length (dirname dir))))
	    (op (current-output-port)))
	 (let loop ((dir dir))
	    (for-each (lambda (p)
			 (when (directory? p)
			    (when (any? directory-contains-music?
					(directory->path-list p))
			       (set! count (+fx count 1))
			       (display "Artist: " op)
			       (display (string-capitalize (basename p)) op)
			       (newline op))
			    (loop p)))
		      (sort string<? (directory->path-list dir))))))
   
   (let ((s (with-output-to-string
	       (lambda ()
		  (for-each directory-listartist directories)))))
      (values s count)))

;*---------------------------------------------------------------------*/
;*    directories-song-count ...                                       */
;*---------------------------------------------------------------------*/
(define (directories-song-count directories)
   
   (define count 0)
   
   (define (directory-song-count dir)
      (for-each (lambda (p)
		   (cond
		      ((directory? p)
		       (directory-song-count p))
		      ((music-file? p)
		       (set! count (+fx count 1)))))
		(directory->path-list dir)))
   
   (for-each directory-song-count directories)

   count)

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
			 ((music-file? p)
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
(define-generic (mpd-database-stats o::mpd-database op)
   (with-access::mpd-database o (%nartists %nalbums %nsongs %uptime %db-update)
      (fprintf op "artists: ~a
albums: ~a
songs: ~a
uptime: ~a
playtime: 1000000
db_playtime: 10000000
db_update: ~a\n"
	       %nartists
	       %nalbums
	       %nsongs
	       (elong->fixnum (-elong (current-seconds) %uptime))
	       (elong->fixnum (-elong (current-seconds) %db-update)))))

;*---------------------------------------------------------------------*/
;*    mpd-database-listall ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listall o::mpd-database op)
   
   (define (directory-list path)
      (let ((offset (string-length path)))
	 (let loop ((path path))
	    (if (directory? path)
		(begin
		   (display "directory: " op)
		   (display (file->mpd path o offset) op)
		   (newline op)
		   (for-each (lambda (f)
				(loop (make-file-name path f)))
			     (directory->sort-list path)))
		(begin
		   (display "file: " op)
		   (display (file->mpd path o offset) op)
		   (newline op))))))
   
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
	     (mpd-database-%albums o)))

;*---------------------------------------------------------------------*/
;*    mpd-database-listartistalbum ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listartistalbum o::mpd-database op artist)
   (for-each (lambda (a)
		(display "Album: " op)
		(display (car a) op)
		(newline op))
	     (if (string? artist)
		 (filter (lambda (c)
			    (let ((dir (cdr c)))
			       (string=? (basename (dirname dir)) artist)))
			 (mpd-database-%albums o))
		 (mpd-database-%albums o))))

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
		     (mpd-database-%albums o))))

;*---------------------------------------------------------------------*/
;*    mpd-database-listgenre ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listgenre o::mpd-database op)
   (for-each (lambda (a)
		(display "Genre: " op)
		(display (car a) op)
		(newline op))
	     (mpd-database-%genres o)))

;*---------------------------------------------------------------------*/
;*    mpd-database-listartist ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-listartist o::mpd-database op)
   (for-each (lambda (a)
		(display "Artist: " op)
		(display (car a) op)
		(newline op))
	     (mpd-database-%artists o)))

;*---------------------------------------------------------------------*/
;*    mpd-database-lsinfo ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (mpd-database-lsinfo o::mpd-database op dir)
   (with-access::mpd-database o (%base %prefixes %roots directories)
      (cond
	 ((or (not (string? dir))
	      (=fx (string-length dir) 0)
	      (and (=fx (string-length dir) 1)
		   (char=? (string-ref dir 0) (file-separator))))
	  (display "directory: " op)
	  (display %base op)
	  (newline op))
	 ((string=? dir %base)
	  (for-each (lambda (dir)
		       (display "directory: " op)
		       (display (file->mpd dir o 0) op)
		       (newline op))
		    directories))
	 (else
	  (multiple-value-bind (path offset)
	     (mpd->file dir o)
	     (when (directory? path)
		(for-each (lambda (f)
			     (let ((p (make-file-name path f)))
				(if (directory? p)
				    (display "directory: " op)
				    (display "file: " op))
				(display (file->mpd p o offset) op)
				(newline op)))
			  (directory->sort-list path))))))))

;*---------------------------------------------------------------------*/
;*    find-music-file ...                                              */
;*---------------------------------------------------------------------*/
(define (find-music-file db dir op artist album)
   (let loop ((dir dir))
      (let ((artist (or artist (basename (dirname dir))))
	    (album (or album (basename dir))))
	 (for-each (lambda (f)
		      (let ((p (make-file-name dir f)))
			 (cond
			    ((directory? p)
			     (loop p))
			    ((music-file? p)
			     (infofile db p #f op artist album)))))
		   (sort (lambda (s1 s2)
			    (<fx (string-natural-compare3 s1 s2) 0))
			 (directory->list dir))))))

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
	     (for-each (lambda (f)
			  (let ((p (make-file-name dir f)))
			     (cond
				((directory? p)
				 (loop p))
				((music-file? p)
				 (infofile db p #f op artist album)))))
		       (sort (lambda (s1 s2)
				(<fx (string-natural-compare3 s1 s2) 0))
			     (directory->list dir)))
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
	 (for-each (lambda (f)
		      (let ((p (make-file-name dir f)))
			 (cond
			    ((directory? p)
			     (loop p))
			    ((and (music-file? p) (string=? title (prefix f)))
			     (infofile db p #f op artist title)))))
		   (directory->path-list dir))))
   
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
	  (infofile o dir #f op)
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
	  (infofile o dir #f op)
	  #t)
	 (else
	  #f)))
   
   (with-access::mpd-database o (directories)
      (any find-genre directories)))
