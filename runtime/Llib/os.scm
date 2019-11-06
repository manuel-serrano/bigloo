;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/os.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Tue Aug  5 10:57:59 1997                          */
;*    Last change :  Thu Mar 14 14:53:53 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Os dependant variables (setup by configure).                     */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Operating System Interface@                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __os
   
   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    __object
	    __thread
	    __rgc
	    __bit
	    __custom
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_numbers_6_5
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __foreign
	    __evenv
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r4_input_6_10_2)
   
   (extern  ($signal::obj (::int ::obj) "bgl_signal")
	    ($get-signal-handler::obj (::int) "bgl_get_signal_handler")
	    ($restore-signal-handlers::void () "bgl_restore_signal_handlers")
	    (*the-command-line*::obj "command_line")
	    (*the-executable-name*::string "executable_name")
	    (macro $getenv?::bool (::string) "(long)getenv")
	    (macro $getenv::string (::string) "(char *)getenv")
	    ($getenv-all::pair () "bgl_getenv_all")
	    (c-setenv::int (::string ::string) "bgl_setenv")
	    (macro c-system::int  (::string) "system")
	    (c-date::string () "c_date")
	    (macro c-chdir::bool (::string) "chdir")
	    (macro c-getcwd::string (::string ::int) "(char *)(long)getcwd")
	    (macro $chdir::int (::string) "chdir")
	    (c-chmod::bool (::string ::bool ::bool ::bool) "bgl_chmod")
	    (macro c-chmod-int::bool (::string ::int) "chmod")
	    
            (macro runtime-default-executable-name::string "BGL_DEFAULT_A_OUT")
            (macro runtime-default-script-name::string "BGL_DEFAULT_A_BAT")
	    (macro runtime-os-class::string "OS_CLASS")
	    (macro runtime-os-name::string "OS_NAME")
	    (macro runtime-os-arch::string "OS_ARCH")
	    (macro runtime-os-version::string "OS_VERSION")
	    (macro runtime-os-tmp::string "OS_TMP")
	    (macro runtime-file-separator::char "FILE_SEPARATOR")
	    (macro runtime-path-separator::char "PATH_SEPARATOR")
	    (macro runtime-static-library-suffix::string "STATIC_LIB_SUFFIX")
	    (macro runtime-shared-library-suffix::string "SHARED_LIB_SUFFIX")
	    (macro runtime-os-charset::string "OS_CHARSET")
	    (c-sleep::void (::long) "bgl_sleep")
	    (macro %dload-init-sym::string "BGL_DYNAMIC_LOAD_INIT")
	    (%dload::obj (::string ::string ::string) "bgl_dload")
	    (%dunload::int (::bstring) "bgl_dunload")
	    (%dload-error::string () "bgl_dload_error")
	    ($getuid::int () "bgl_getuid")
	    ($getgid::int () "bgl_getgid")
	    ($setuid::int (::int) "bgl_setuid")
	    ($setgid::int (::int) "bgl_setgid")
	    ($getpwnam::obj (::string) "bgl_getpwnam")
	    ($getpwuid::obj (::int) "bgl_getpwuid")
	    (macro $umask::long (::long) "umask")
	    (macro $getpid::int () "getpid")
	    (macro $getppid::int () "getppid")
	    ($getgroups::vector () "bgl_getgroups")
	    ($ioctl::bool (::obj ::elong ::elong) "bgl_ioctl")
	    (macro $openlog::void (::string ::int ::int) "openlog")
	    (macro $syslog::void (::int ::string ::string) "syslog")
	    (macro $closelog::void () "closelog")
	    (macro $syslog-log-cons::int "LOG_CONS")
	    (macro $syslog-log-ndelay::int "LOG_NDELAY")
	    (macro $syslog-log-nowait::int "LOG_NOWAIT")
	    (macro $syslog-log-odelay::int "LOG_ODELAY")
	    ;; (macro $syslog-log-perror::int "LOG_PERROR")
	    (macro $syslog-log-pid::int "LOG_PID")
	    (macro $syslog-log-auth::int "LOG_AUTH")
	    (macro $syslog-log-authpriv::int "LOG_AUTHPRIV")
	    (macro $syslog-log-cron::int "LOG_CRON")
	    (macro $syslog-log-daemon::int "LOG_DAEMON")
	    (macro $syslog-log-ftp::int "LOG_FTP")
	    (macro $syslog-log-kern::int "LOG_KERN")
	    (macro $syslog-log-local0::int "LOG_LOCAL0")
	    (macro $syslog-log-local1::int "LOG_LOCAL1")
	    (macro $syslog-log-local2::int "LOG_LOCAL2")
	    (macro $syslog-log-local3::int "LOG_LOCAL3")
	    (macro $syslog-log-local4::int "LOG_LOCAL4")
	    (macro $syslog-log-local5::int "LOG_LOCAL5")
	    (macro $syslog-log-local6::int "LOG_LOCAL6")
	    (macro $syslog-log-local7::int "LOG_LOCAL7")
	    (macro $syslog-log-lpr::int "LOG_LPR")
	    (macro $syslog-log-mail::int "LOG_MAIL")
	    (macro $syslog-log-news::int "LOG_NEWS")
	    (macro $syslog-log-syslog::int "LOG_SYSLOG")
	    (macro $syslog-log-user::int "LOG_USER")
	    (macro $syslog-log-uucp::int "LOG_UUCP")
	    (macro $syslog-log-emerg::int "LOG_EMERG")
	    (macro $syslog-log-alert::int "LOG_ALERT")
	    (macro $syslog-log-crit::int "LOG_CRIT")
	    (macro $syslog-log-err::int "LOG_ERR")
	    (macro $syslog-log-warning::int "LOG_WARNING")
	    (macro $syslog-log-notice::int "LOG_NOTICE")
	    (macro $syslog-log-info::int "LOG_INFO")
	    (macro $syslog-log-debug::int "LOG_DEBUG")

	    ($bgl-dlsym::custom (::bstring ::bstring ::bstring) "bgl_dlsym")
	    ($bgl-dlsym-get::obj (::custom) "bgl_dlsym_get")
	    ($bgl-dlsym-set::obj (::custom ::obj) "bgl_dlsym_set"))

   (java    (class foreign
	       (field static *the-command-line*::obj
		  "command_line")
	       (field static *the-executable-name*::string
		  "executable_name")
	       (method static $signal::obj (::int ::obj)
		  "bgl_signal")
	       (method static $get-signal-handler::obj (::int)
		  "bgl_get_signal_handler")
	       (method static $restore-signal-handlers::void ()
		  "bgl_restore_signal_handlers")
	       (method static $getenv?::bool (::string)
		  "getenv_exists")
	       (method static $getenv::string (::string)
		  "getenv")
	       (method static $getenv-all::obj ()
		  "getenv_all")
	       (method static c-setenv::int (::string ::string)
		  "bgl_setenv")
	       (method static c-system::int  (::string)
		  "system")
	       (method static c-date::string ()
		  "c_date")
	       (method static c-chdir::bool (::string)
		  "chdir")
	       (method static c-getcwd::string (::string ::int)
		  "getcwd")
	       (method static c-chmod::bool (::string ::bool ::bool ::bool)
		  "bgl_chmod")
	       (method static c-chmod-int::bool (::string ::int)
		  "bgl_chmod")
	       (method static c-sleep::void (::long)
		  "bgl_sleep")
	       (field static %dload-init-sym::string
		  "BGL_DYNAMIC_LOAD_INIT")
	       (method static %dload::obj (::string ::string ::string)
		  "bgl_dload")
	       (method static %dunload::int (::string)
		  "bgl_dunload")
	       (method static %dload-error::string ()
		  "bgl_dload_error")
	       (method static $umask::int (::int)
		  "bgl_umask")
	       (method static $bgl-dlsym::custom (::bstring ::bstring ::bstring)
		  "bgl_dlsym")
	       (method static $bgl-dlsym-get::obj (::custom)
		  "bgl_dlsym_get")
	       (method static $bgl-dlsym-set::obj (::custom ::obj)
		  "bgl_dlsym_set"))
      
      (class runtime
	 (field static default-executable-name::string
	    "BGL_DEFAULT_A_OUT")
	 (field static default-script-name::string
	    "BGL_DEFAULT_A_BAT")
	 (field static os-class::string
	    "OS_CLASS")
	 (field static os-name::string
	    "OS_NAME")
	 (field static os-arch::string
	    "OS_ARCH")
	 (field static os-version::string
	    "OS_VERSION")
	 (field static os-tmp::string
	    "OS_TMP")
	 (field static os-charset::string
	    "OS_CHARSET")
	 (field static file-separator::char
	    "FILE_SEPARATOR")
	 (field static path-separator::char
	    "PATH_SEPARATOR")
	 (field static static-library-suffix::string
	    "STATIC_LIB_SUFFIX")
	 (field static shared-library-suffix::string
	    "SHARED_LIB_SUFFIX")
	 "bigloo.os"))
   
   (export  (signal num::int ::obj)
	    (get-signal-handler::obj ::int)
	    
	    (getenv #!optional name)
	    (putenv ::string ::string)
	    (date::string)
	    (inline chdir::bool string::string)
	    (system . strings)
	    (system->string . strings)
	    (pwd)
	    (command-line)
	    (executable-name::string)
	    (basename::bstring ::bstring)
	    (dirname::bstring ::bstring)
	    (prefix::bstring ::bstring)
	    (suffix::bstring ::bstring)
	    (chmod::bool ::bstring . opts)
	    (make-file-name::bstring ::bstring ::bstring)
	    (make-file-path::bstring ::bstring ::bstring . obj)
	    (make-static-lib-name ::bstring ::symbol)
	    (make-shared-lib-name ::bstring ::symbol)
	    (file-name->list::pair-nil ::bstring)
	    (file-name-canonicalize::bstring ::bstring)
	    (file-name-canonicalize!::bstring ::bstring)
	    (file-name-unix-canonicalize::bstring ::bstring)
	    (file-name-unix-canonicalize!::bstring ::bstring)
	    (relative-file-name::bstring ::bstring ::bstring)
	    (find-file/path ::bstring ::obj)
	    (make-static-library-name::bstring ::bstring)
	    (make-shared-library-name::bstring ::bstring)
            (inline default-executable-name)
            (inline default-script-name)
	    (inline os-class)
            (inline os-name)
	    (inline os-arch)
	    (inline os-version)
	    (inline os-tmp)
	    (inline file-separator)
	    (inline path-separator)
	    (inline static-library-suffix)
	    (inline shared-library-suffix)
	    (os-charset)
	    *dynamic-load-path*
	    *default-java-package*
	    (inline sleep ::long)
	    (dynamic-load ::bstring
			  #!optional
			  (init %dload-init-sym)
			  (module #f))
	    (dynamic-unload ::bstring)
	    (dynamic-load-symbol::obj ::bstring ::bstring #!optional module)
	    (inline dynamic-load-symbol-get::obj ::custom)
	    (inline dynamic-load-symbol-set::obj ::custom ::obj)
	    (unix-path->list::pair-nil ::bstring)
	    (getuid::int)
	    (setuid ::int)
	    (getgid::int)
	    (setgid ::int)
	    (getpwnam ::bstring)
	    (getpwuid ::int)
	    (inline getpid::int)
	    (inline getppid::int)
	    (inline getgroups::vector)
	    (ioctl-register-request! ::bstring ::uint64)
	    (ioctl::bool ::obj ::obj ::obj)
	    (umask::int #!optional mask)
	    (inline openlog ::bstring ::int ::int)
	    (syslog ::int . args)
	    (inline closelog)
	    (syslog-option::int . opts)
	    (syslog-facility::int ::symbol)
	    (syslog-level::int ::symbol)))

;*---------------------------------------------------------------------*/
;*    Variables setup ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (default-executable-name) runtime-default-executable-name)
(define-inline (default-script-name) runtime-default-script-name)
(define-inline (os-class) runtime-os-class)
(define-inline (os-name) runtime-os-name)
(define-inline (os-arch) runtime-os-arch)
(define-inline (os-version) runtime-os-version)
(define-inline (os-tmp) runtime-os-tmp)
(define-inline (file-separator) runtime-file-separator)
(define-inline (path-separator) runtime-path-separator)
(define-inline (static-library-suffix) runtime-static-library-suffix)
(define-inline (shared-library-suffix) runtime-shared-library-suffix)

;*---------------------------------------------------------------------*/
;*    os-charset ...                                                   */
;*---------------------------------------------------------------------*/
(define (os-charset)
   (cond
      ((getenv "LANG") => (lambda (x) x))
      ((getenv "LC_CTYPE") => (lambda (x) x))
      ((getenv "LC_ALL") => (lambda (x) x))
      (else runtime-os-charset)))

;*---------------------------------------------------------------------*/
;*    command-line ...                                                 */
;*---------------------------------------------------------------------*/
(define (command-line)
   *the-command-line*)

;*---------------------------------------------------------------------*/
;*    executable-name ...                                              */
;*---------------------------------------------------------------------*/
(define (executable-name)
   *the-executable-name*)

;*---------------------------------------------------------------------*/
;*    signal ...                                                       */
;*---------------------------------------------------------------------*/
(define (signal num proc)
   (cond
      ((eq? proc 'ignore)
       ($signal num #t))
      ((eq? proc 'default)
       ($signal num #f))
      ((not (=fx (procedure-arity proc) 1))
       (error "signal" "Wrong number of arguments" proc))
      ((<fx num 0)
       #unspecified)
      ((>fx num 31)
       (error "signal" "Illegal signal" num))
      (else
       ($signal num proc))))

;*---------------------------------------------------------------------*/
;*    get-signal-handler ...                                           */
;*---------------------------------------------------------------------*/
(define (get-signal-handler num)
   (let ((v ($get-signal-handler num)))
      (cond
	 ((eq? v #t) 'ignore)
	 ((eq? v #f) 'default)
	 (else v))))

;*---------------------------------------------------------------------*/
;*    getenv ...                                                       */
;*---------------------------------------------------------------------*/
(define (getenv #!optional name)
   (if (string? name)
       (begin
	  (when (and (string=? (os-class) "win32") (string=? name "HOME"))
	     (set! name "USERPROFILE"))
	  (if ($getenv? name)
	      (let ((result ($getenv name)))
		 (if (string-ptr-null? result)
		     #f
		     result))
	      #f))
       ($getenv-all)))

;*---------------------------------------------------------------------*/
;*    putenv ...                                                       */
;*---------------------------------------------------------------------*/
(define (putenv string val)
   (if (and (string=? (os-class) "win32")
            (string=? string "HOME"))
       (set! string "USERPROFILE"))
   (=fx (c-setenv string val) 0))

;*---------------------------------------------------------------------*/
;*    system ...                                                       */
;*---------------------------------------------------------------------*/
(define (system . strings)
   (cond
      ((null? strings)
       #f)
      ((null? (cdr strings))
       (c-system (car strings)))
      (else
       (c-system (apply string-append strings)))))
   
;*---------------------------------------------------------------------*/
;*    system->string ...                                               */
;*---------------------------------------------------------------------*/
(define (system->string . strings)
   (let ((p (open-input-file (apply string-append "| " strings))))
      (unwind-protect
	 (read-string p)
	 (close-input-port p))))

;*---------------------------------------------------------------------*/
;*    date ...                                                         */
;*---------------------------------------------------------------------*/
(define (date)
   (let* ((dt (c-date))
	  (len (string-length dt)))
      (if (char=? (string-ref dt (-fx len 1)) #\Newline)
	  (substring dt 0 (-fx len 1))
	  dt)))

;*---------------------------------------------------------------------*/
;*    chdir ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (chdir dirname)
   (if (c-chdir dirname) #f #t))

;*---------------------------------------------------------------------*/
;*    pwd ...                                                          */
;*---------------------------------------------------------------------*/
(define (pwd)
   (let ((string (make-string 1024)))
      (c-getcwd string 1024)))
	  
;*---------------------------------------------------------------------*/
;*    basename ...                                                     */
;*---------------------------------------------------------------------*/
(define (basename string)
   (if (string=? (os-class) "mingw")
       (mingw-basename string)
       (default-basename string)))

;*---------------------------------------------------------------------*/
;*    mingw-basename ...                                               */
;*---------------------------------------------------------------------*/
(define (mingw-basename string)
   (let ((n (string-length string))
	 (stop #f))
      (do ((i (-fx n 1) (-fx i 1)))
	  ((eq? stop #t)
	   (substring string (+fx i 2) n))
	  (set! stop (if (<fx i 0)
			 #t
			 (or (char=? (string-ref string i) #\\)
			     (char=? (string-ref string i) #\/)))))))

;*---------------------------------------------------------------------*/
;*    default-basename ...                                             */
;*---------------------------------------------------------------------*/
(define (default-basename string)
   (let* ((len   (-fx (string-length string) 1))
	  (start (if (and (>fx len 0)
			  (char=? (string-ref string len)
				  runtime-file-separator))
		     (-fx len 1)
		     len)))
      (let loop ((index start))
	 (cond
	    ((=fx index -1)
	     string)
	    ((char=? (string-ref string index) runtime-file-separator)
	     (substring string (+fx index 1) (+fx start 1)))
	    (else
	     (loop (-fx index 1)))))))

;*---------------------------------------------------------------------*/
;*    prefix ...                                                       */
;*---------------------------------------------------------------------*/
(define (prefix string)
   (let ((len (-fx (string-length string) 1)))
      (let loop ((e len)
                 (s len))
         (cond
            ((<=fx s 0)
             (substring string 0 (+fx 1 e)))
            (else
             (if (and (eq? (string-ref string s) #\.)
                      (=fx e len))
                 (loop (-fx s 1) (-fx s 1))
                 (loop e (-fx s 1))))))))

;*---------------------------------------------------------------------*/
;*    dirname ...                                                      */
;*---------------------------------------------------------------------*/
(define (dirname string)
  (if (string=? (os-class) "mingw")
      (mingw-dirname string)
      (default-dirname string)))

;*---------------------------------------------------------------------*/
;*    mingw-dirname ...                                                */
;*---------------------------------------------------------------------*/
(define (mingw-dirname string)
   (let ((n (string-length string))
	 (stop #f))
      (do ((i (-fx n 1) (-fx i 1)))
	  ((eq? stop #t)
	   (if (<fx i 0) 
	       "."
	       (substring string 0 (+fx i 1))))
	  (set! stop (if (<fx i 0)
			 #t
			 (or (char=? (string-ref string i) #\\)
			     (char=? (string-ref string i) #\/)))))))

;*---------------------------------------------------------------------*/
;*    default-dirname ...                                              */
;*---------------------------------------------------------------------*/
(define (default-dirname string)
   (let ((len (-fx (string-length string) 1)))
      (if (=fx len -1)
	  "."
	  (let loop ((read len))
	     (cond
		((=fx read 0)
		 (if (char=? (string-ref string read) runtime-file-separator)
		     (make-string 1 runtime-file-separator)
		     "."))
		((char=? (string-ref string read) runtime-file-separator)
		 (substring string 0 read))
		(else
		 (loop (-fx read 1))))))))

;*---------------------------------------------------------------------*/
;*    suffix ...                                                       */
;*---------------------------------------------------------------------*/
(define (suffix string)
   (let* ((len (string-length string))
          (len-1 (-fx len 1)))
      (let loop ((read len-1))
         (cond
            ((<fx read 0)
             "")
            ((char=? (string-ref string read) runtime-file-separator)
	     "")
            ((char=? (string-ref string read) #\.)
             (cond
                ((=fx read len-1)
                 "")
                (else
                 (substring string (+fx read 1) len))))
            (else
             (loop (-fx read 1)))))))

;*---------------------------------------------------------------------*/
;*    chmod ...                                                        */
;*---------------------------------------------------------------------*/
(define (chmod file::bstring . mode)
   (let loop ((mode mode)
	      (read? #f)
	      (write? #f)
	      (exec? #f))
      (cond
	 ((null? mode)
	  (c-chmod file read? write? exec?))
	 ((fixnum? (car mode))
	  (c-chmod-int file (car mode)))
	 ((eq? (car mode) 'read)
	  (loop (cdr mode)
		#t
		write?
		exec?))
	 ((eq? (car mode) 'write)
	  (loop (cdr mode)
		read?
		#t
		exec?))
	 ((eq? (car mode) 'execute)
	  (loop (cdr mode)
		read?
		write?
		#t))
	 (else
	  (error "chmod" "Unknown mode" mode)))))
	     
;*---------------------------------------------------------------------*/
;*    @deffn make-file-name@ ...                                       */
;*    -------------------------------------------------------------    */
;*    This function build a file name from a path and a                */
;*    relative file-name.                                              */
;*---------------------------------------------------------------------*/
(define (make-file-name directory::bstring file::bstring)
   (define (default ldir)
      (let* ((lfile (string-length file))
	     (len (+fx ldir (+fx lfile 1)))
	     (str (make-string len runtime-file-separator)))
	 (blit-string-ur! directory 0 str 0 ldir)
	 (blit-string-ur! file 0 str (+fx 1 ldir) lfile)
	 str))
   (let ((ldir (string-length directory)))
      (cond
	 ((and (=fx ldir 1) (char=? (string-ref directory 0) #\.))
	  file)
	 ((=fx ldir 0)
	  (let* ((lfile (string-length file))
		 (len (+fx 1 lfile))
		 (str (make-string len runtime-file-separator)))
	     (blit-string-ur! file 0 str 1 lfile)
	     str))
	 ((char=? (string-ref directory (-fx ldir 1)) runtime-file-separator)
	  (let* ((lfile (string-length file))
		 (len (+fx ldir lfile))
		 (str (make-string len runtime-file-separator)))
	     (blit-string-ur! directory 0 str 0 ldir)
	     (blit-string-ur! file 0 str ldir lfile)
	     str))
	 (else
	  (default ldir)))))

;*---------------------------------------------------------------------*/
;*    @deffn make-file-path@ ...                                       */
;*    -------------------------------------------------------------    */
;*    This function build a absolute file name from a path and a       */
;*    relative file-name.                                              */
;*---------------------------------------------------------------------*/
(define (make-file-path directory::bstring file::bstring . obj)
   (if (and (=fx (string-length directory) 0) (null? obj))
       file
       (let* ((ldir  (string-length directory))
	      (lfile (string-length file))
	      (len (let loop ((obj obj)
			      (l (+fx ldir (+fx 1 lfile))))
		      (cond
			 ((null? obj)
			  l)
			 ((not (string? (car obj)))
			  (bigloo-type-error "make-file-path"
			     "string" (car obj)))
			 (else
			  (loop (cdr obj) (+fx 1
					       (+fx (string-length (car obj))
						    l)))))))
	      (str  (make-string len runtime-file-separator)))
	  (blit-string-ur! directory 0 str 0 ldir)
	  (blit-string-ur! file 0 str (+fx 1 ldir) lfile)
	  (let loop ((obj obj)
		     (w (+fx 1 (+fx ldir lfile))))
	     (if (null? obj)
		 str
		 (let ((lo (string-length (car obj))))
		    (blit-string-ur! (car obj) 0 str (+fx 1 w) lo)
		    (loop (cdr obj) (+fx w (+fx lo 1)))))))))

;*---------------------------------------------------------------------*/
;*    make-static-lib-name ...                                         */
;*---------------------------------------------------------------------*/
(define (make-static-lib-name lib backend)
   (case backend
      ((bigloo-c)
       (if (string=? (os-class) "win32")
	   (string-append lib "." runtime-static-library-suffix)
	   (string-append "lib" lib "." runtime-static-library-suffix)))
      ((bigloo-jvm)
       (string-append lib ".zip"))
      ((bigloo-.net)
       (string-append lib ".dll"))
      (else
       (error "make-static-lib-name" "Unknown backend" backend))))

;*---------------------------------------------------------------------*/
;*    make-shared-lib-name ...                                         */
;*---------------------------------------------------------------------*/
(define (make-shared-lib-name lib backend)
   (case backend
      ((bigloo-c)
       (if (string=? (os-class) "win32")
	   (string-append lib "." runtime-static-library-suffix)
	   (string-append "lib" lib "." runtime-shared-library-suffix)))
      ((bigloo-jvm)
       (string-append lib ".zip"))
      ((bigloo-.net)
       (string-append lib ".dll"))
      (else
       (error "make-shared-lib-name" "Unknown backend" backend))))

;*---------------------------------------------------------------------*/
;*    @deffn find-file/path@ ...                                       */
;*---------------------------------------------------------------------*/
(define (find-file/path file-name path)
   (define (mingw-full-qualified-path? file-name)
      (if (string=? (os-class) "mingw")
	  (or (char=? (string-ref file-name 0) #\/)
	      (char=? (string-ref file-name 0) #\\)
	      (if (>fx (string-length file-name) 2)
		  (and (char=? (string-ref file-name 1) #\:)
		       (or (char=? (string-ref file-name 2) #\/)
			   (char=? (string-ref file-name 2) #\\)))
		  #f))
	  #f))
   (cond
      ((=fx (string-length file-name) 0)
       #f)
      ((or (char=? (string-ref file-name 0) runtime-file-separator)
	   (mingw-full-qualified-path? file-name))
       (if (file-exists? file-name)
           file-name
           #f))
      (else
       (let loop ((path path))
	  (if (null? path)
	      #f
	      (let ((fname (make-file-name (car path) file-name)))
		 (if (file-exists? fname)
		     fname
		     (loop (cdr path)))))))))

;*---------------------------------------------------------------------*/
;*    @deffn file-name->list@ ...                                      */
;*---------------------------------------------------------------------*/
(define (file-name->list name)
   (let ((len (string-length name)))
      (if (and (=fx len 1) (char=? (string-ref name 0) (file-separator)))
	  (list "")
	  (let loop ((start 0)
		     (stop 0)
		     (res '()))
	     (cond
		((=fx stop len)
		 (reverse! (cons (substring name start stop) res)))
		((char=? (string-ref name stop) (file-separator))
		 (loop (+fx stop 1)
		       (+fx stop 1)
		       (cons (substring name start stop) res)))
		(else
		 (loop start (+fx stop 1) res)))))))

;*---------------------------------------------------------------------*/
;*    file-name-canonicalize-inner ...                                 */
;*---------------------------------------------------------------------*/
(define (file-name-canonicalize-inner::bstring src::bstring res::bstring index)
   
   (let ((sep (file-separator))
	 (len (string-length src))
	 (i index)
	 (j index))
      
      (define (skip-separators)
	 (if (=fx i len)
	     (string-shrink! res j)
	     (if (char=? (string-ref src i) sep)
		 (begin
		    (set! i (+fx i 1))
		    (skip-separators))
		 (main-loop))))
      
      (define (copy-path)
	 (if (=fx i len)
	     (string-shrink! res j)
	     (let ((c (string-ref src i)))
		(cond
		   ((char=? c sep)
		    (string-set! res j c)
		    (set! i (+fx i 1))
		    (set! j (+fx j 1))
		    (skip-separators))
		   (else
		    (string-set! res j c)
		    (set! i (+fx i 1))
		    (set! j (+fx j 1))
		    (copy-path))))))
      
      (define (pop-directory j)
	 (let ((nj (string-index-right res sep (-fx j 1))))
	    (if (fixnum? nj)
		(+fx nj 1)
		0)))
	    
      (define (main-loop)
	 (if (=fx i len)
	     (string-shrink! res j)
	     (let ((c (string-ref src i)))
		(cond
		   ((char=? c sep)
		    ;; a path separator
		    (string-set! res j c)
		    (set! i (+fx i 1))
		    (set! j (+fx j 1))
		    (skip-separators))
		   ((char=? c #\.)
		    (cond
		       ((=fx i (-fx len 1))
			;; a tailing "."
			(if (=fx j 0)
			    (begin
			       (string-set! res j #\.)
			       (string-shrink! res (+fx j 1)))
			    (string-shrink! res (-fx j 1))))
		       ((char=? (string-ref src (+fx i 1)) sep)
			(if (=fx i (-fx len 2))
			    ;; a trailing "./" sequence
			    (begin
			       (string-set! res j #\.)
			       (string-shrink! res (+fx j 1)))
			    (begin
			       ;; a sequence "./" that is skipped
			       (set! i (+fx i 2))
			       (skip-separators))))
		       ((char=? (string-ref src (+fx i 1)) #\.)
			;; a ".." sequence
			(cond
			   ((=fx i (-fx len 2))
			    ;; a final ".."
			    (cond
			       ((=fx j 0)
				(string-shrink! res 0))
			       ((=fx j 1)
				(string-shrink! res 1))
			       (else
				(set! j (pop-directory j))
				(if (>fx j 1)
				    (string-shrink! res (-fx j 1))
				    (string-shrink! res j)))))
			   ((char=? (string-ref src (+fx i 2)) sep)
			    ;; a "../" sequence
			    (when (>=fx j 2)
			       (set! j (pop-directory j)))
			    (set! i (+fx i 3))
			    (skip-separators))
			   (else
			    (string-set! res j c)
			    (set! i (+fx i 1))
			    (set! j (+fx j 1))
			    (string-set! res j (string-ref src i))
			    (set! i (+fx i 1))
			    (set! j (+fx j 1))
			    (string-set! res j (string-ref src i))
			    (copy-path))))
		       (else
			;; a sequence ".[^.]"
			(string-set! res j c)
			(set! i (+fx i 1))
			(set! j (+fx j 1))
			(string-set! res j (string-ref src i))
			(set! i (+fx i 1))
			(set! j (+fx j 1))
			(copy-path))))
		   (else
		    ;; a regular path
		    (string-set! res j c)
		    (set! i (+fx i 1))
		    (set! j (+fx j 1))
		    (copy-path))))))

      ;; start copy all leading sequence of ".."
      (define (head-loop)
	 (if (and (<fx i (-fx len 3))
		  (char=? (string-ref src i) #\.)
		  (char=? (string-ref src (+fx i 1)) #\.)
		  (char=? (string-ref src (+fx i 2)) sep))
	     (begin
		(string-set! res j #\.)
		(string-set! res (+fx j 1) #\.)
		(string-set! res (+fx j 2) sep)
		(set! i (+fx i 3))
		(set! j (+fx j 3))
		(let loop ()
		   (if (=fx i len)
		       (string-shrink! res j)
		       (if (char=? (string-ref src i) sep)
			   (begin
			      (set! i (+fx i 1))
			      (loop))
			   (head-loop)))))
	     (main-loop)))
      
      (head-loop)))
		    
;*---------------------------------------------------------------------*/
;*    file-name-canonicalize ...                                       */
;*---------------------------------------------------------------------*/
(define (file-name-canonicalize name)
   (file-name-canonicalize-inner name (make-string (string-length name)) 0))

;*---------------------------------------------------------------------*/
;*    file-name-canonicalize! ...                                      */
;*---------------------------------------------------------------------*/
(define (file-name-canonicalize! name)
   (let ((len (string-length name))
	 (sep (file-separator)))
      (let loop ((i 0)
		 (s 0))
	 (if (=fx i len)
	     name
	     (let ((c (string-ref-ur name i)))
		(cond
		   ((char=? c sep)
		    (if (=fx s (-fx i 1))
			(let ((res (make-string len)))
			   (blit-string! name 0 res 0 i)
			   (file-name-canonicalize-inner name res s))
			(loop (+fx i 1) i)))
		   ((and (char=? c #\.) (>=fx s 0))
		    (let ((res (make-string len)))
		       (blit-string! name 0 res 0 i)
		       (file-name-canonicalize-inner name res s)))
		   (else
		    (loop (+fx i 1) -1))))))))
      
;*---------------------------------------------------------------------*/
;*    file-name-unix-canonicalize ...                                  */
;*---------------------------------------------------------------------*/
(define (file-name-unix-canonicalize name)
   (let ((len (string-length name)))
      (cond
	 ((=fx len 0)
	  name)
	 ((char=? (string-ref name 0) #\~)
	  (cond
	     ((=fx len 1)
	      ;; complete with the $HOME
	      (file-name-canonicalize! (getenv "HOME")))
	     ((char=? (string-ref name 1) (file-separator))
	      ;; complete with the $HOME
	      (file-name-canonicalize!
	       (string-append (getenv "HOME") (substring name 1 len))))
	     (else
	      ;; complete with a user home
	      (file-name-canonicalize!
	       (make-file-path (getenv "HOME") ".." (substring name 1 len))))))
	 (else
	  (file-name-canonicalize name)))))
      
;*---------------------------------------------------------------------*/
;*    file-name-unix-canonicalize! ...                                 */
;*---------------------------------------------------------------------*/
(define (file-name-unix-canonicalize! name)
   (let ((len (string-length name)))
      (cond
	 ((=fx len 0)
	  name)
	 ((char=? (string-ref name 0) #\~)
	  (file-name-unix-canonicalize name))
	 (else
	  (file-name-canonicalize! name)))))

;*---------------------------------------------------------------------*/
;*    relative-file-name ...                                           */
;*---------------------------------------------------------------------*/
(define (relative-file-name name base)
   (define (make-file f)
      (cond
	 ((null? (cdr f))
	  (car f))
	 ((null? (cddr f))
	  (make-file-name (car f) (cadr f)))
	 (else
	  (apply make-file-path f))))
   (let ((f (file-name->list name)))
      (if (not (string=? (car f) ""))
	  name
	  (let loop ((f f)
		     (b (file-name->list base)))
	     (cond
		((null? f)
		 "")
		((null? b)
		 (make-file f))
		((not (string=? (car f) (car b)))
		 (make-file (append (make-list (length b) "..") f)))
		(else
		 (loop (cdr f) (cdr b))))))))

;*---------------------------------------------------------------------*/
;*    make-static-library-name ...                                     */
;*    -------------------------------------------------------------    */
;*    This function, adds the proper static library extension.         */
;*---------------------------------------------------------------------*/
(define (make-static-library-name libname::bstring)
   (string-append libname "." runtime-static-library-suffix))

;*---------------------------------------------------------------------*/
;*    make-shared-library-name ...                                     */
;*    -------------------------------------------------------------    */
;*    This function, adds the proper shared library extension.         */
;*---------------------------------------------------------------------*/
(define (make-shared-library-name libname::bstring)
   (string-append libname "." runtime-shared-library-suffix))

;*---------------------------------------------------------------------*/
;*    sleep ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (sleep ms)
   (c-sleep ms)
   ms)

;*---------------------------------------------------------------------*/
;*    *dynamic-load-path* ...                                          */
;*---------------------------------------------------------------------*/
(define *dynamic-load-path* '("."))

;*---------------------------------------------------------------------*/
;*    *default-java-package* ...                                       */
;*    -------------------------------------------------------------    */
;*    The default package for non qualified imported Java definitions. */
;*    -------------------------------------------------------------    */
;*    See the -pckg-java compiler option.                              */
;*---------------------------------------------------------------------*/
(define *default-java-package* "bigloo.foreign")

;*---------------------------------------------------------------------*/
;*    dynamic-load ...                                                 */
;*---------------------------------------------------------------------*/
(define (dynamic-load lib #!optional (init %dload-init-sym) (module #f))
   
   (define (proc-err proc msg obj)
      (error (string-append "dynamic-load:" proc) msg obj))
   
   (define (err msg obj)
      (error "dynamic-load:" msg obj))
   
   (let ((flib (cond-expand
		  (bigloo-c (find-file/path lib *dynamic-load-path*))
		  (bigloo-jvm lib)
		  (else (find-file/path lib *dynamic-load-path*))))
	 (mod (if module
		  (cond-expand
		     (bigloo-c
		      (bigloo-module-mangle
			 "module-initialization" (symbol->string! module)))
		     (else
		      module))
		  "")))
      (if (not (string? flib))
	  (err "Can't find library" lib)
	  (let* ((ini (if (not init) "" init))
		 (val (%dload flib ini mod)))
	     (case val
		((__dload_noarch)
		 (err "Not supported on this architecture" flib))
		((__dload_error)
		 (proc-err flib (%dload-error) flib))
		((__dload_noinit)
		 (cond
		    ((and (equal? init %dload-init-sym) (not module))
		     (warning (string-append "dynamic-load: " flib)
			"Cannot find library init entry point -- "
			init))
		    ((not init)
		     #unspecified)
		    (else
		     (proc-err flib
			"Cannot find library init entry point"
			init))))
		(else
		 val))))))

;*---------------------------------------------------------------------*/
;*    dynamic-unload ...                                               */
;*---------------------------------------------------------------------*/
(define (dynamic-unload lib)
   (let ((flib (cond-expand
		  (bigloo-c
		   (find-file/path lib *dynamic-load-path*))
		  (bigloo-jvm
		   lib)
		  (else
		   (find-file/path lib *dynamic-load-path*)))))
      (if (not (string? flib))
	  (error "dynamic-unload" "Can't find library" lib)
	  (=fx (%dunload flib) 0))))

;*---------------------------------------------------------------------*/
;*    dynamic-load-symbol ...                                          */
;*---------------------------------------------------------------------*/
(define (dynamic-load-symbol lib name #!optional module)
   (let ((sym (if (string? module) (bigloo-module-mangle name module) name)))
      ($bgl-dlsym lib name sym)))

;*---------------------------------------------------------------------*/
;*    dynamic-load-symbol-get ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (dynamic-load-symbol-get sym::custom)
   ($bgl-dlsym-get sym))

;*---------------------------------------------------------------------*/
;*    dynamic-load-symbol-set ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (dynamic-load-symbol-set sym::custom val::obj)
   ($bgl-dlsym-set sym val))

;*---------------------------------------------------------------------*/
;*    unix-path->list ...                                              */
;*---------------------------------------------------------------------*/
(define (unix-path->list str)
   (let ((stop (string-length str))
	 (sep (path-separator)))
      (let loop ((mark 0)
		 (r 0)
		 (res '()))
	 (cond
	    ((=fx stop r)
	     (let ((res (if (<fx mark r)
			    (cons (substring str mark r) res)
			    res)))
		(reverse! res)))
	    ((char=? (string-ref str r) sep)
	     (if (<fx mark r)
		 (loop (+fx 1 r) (+fx 1 r) (cons (substring str mark r) res))
		 (loop (+fx 1 r) (+fx 1 r) res)))
	    (else
	     (loop mark (+fx 1 r) res))))))
	     
;*---------------------------------------------------------------------*/
;*    getuid ...                                                       */
;*---------------------------------------------------------------------*/
(define (getuid)
   (cond-expand
      (bigloo-c
       ($getuid))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    setuid ...                                                       */
;*---------------------------------------------------------------------*/
(define (setuid uid)
   (cond-expand
      (bigloo-c
       ($setuid uid))
      (else
       (error "setuid" "operation not supported" uid))))

;*---------------------------------------------------------------------*/
;*    getgid ...                                                       */
;*---------------------------------------------------------------------*/
(define (getgid)
   (cond-expand
      (bigloo-c
       ($getgid))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    setgid ...                                                       */
;*---------------------------------------------------------------------*/
(define (setgid uid)
   (cond-expand
      (bigloo-c
       ($setgid uid))
      (else
       (error "setgid" "operation not supported" uid))))

;*---------------------------------------------------------------------*/
;*    getpwnam ...                                                     */
;*---------------------------------------------------------------------*/
(define (getpwnam name)
   (cond-expand
      (bigloo-c
       ($getpwnam name))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    getpwuid ...                                                     */
;*---------------------------------------------------------------------*/
(define (getpwuid uid)
   (cond-expand
      (bigloo-c
       ($getpwuid uid))
      (else
       #f)))
;*---------------------------------------------------------------------*/
;*    getpid ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (getpid)
   (cond-expand
      (bigloo-c ($getpid))
      (else 0)))

;*---------------------------------------------------------------------*/
;*    getppid ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (getppid)
   (cond-expand
      (bigloo-c ($getppid))
      (else 0)))

;*---------------------------------------------------------------------*/
;*    getpgroups ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (getgroups)
   (cond-expand
      (bigloo-c ($getgroups))
      (else '#())))

;*---------------------------------------------------------------------*/
;*    ioctl-requests-table ...                                         */
;*---------------------------------------------------------------------*/
(define ioctl-requests-table
   '())

;*---------------------------------------------------------------------*/
;*    ioctl-register-request! ...                                      */
;*---------------------------------------------------------------------*/
(define (ioctl-register-request! name::bstring val::uint64)
   (set! ioctl-requests-table (cons (cons name val) ioctl-requests-table)))

;*---------------------------------------------------------------------*/
;*    request->elong ...                                               */
;*---------------------------------------------------------------------*/
(define (request->elong::elong req)
   (let loop ((obj req))
      (cond
	 ((elong? obj)
	  obj)
	 ((fixnum? obj)
	  (fixnum->elong obj))
	 ((real? obj)
	  (flonum->elong obj))
	 ((string? obj)
	  (let ((cell (assoc obj ioctl-requests-table)))
	     (if (pair? cell)
		 (cdr cell)
		 (loop (string->number obj)))))
	 ((bignum? obj)
	  (bignum->elong obj))
	 (else
	  (bigloo-type-error "ioctl" "number of string" req)))))
       
;*---------------------------------------------------------------------*/
;*    ioctl ...                                                        */
;*---------------------------------------------------------------------*/
(define (ioctl dev request val)
   
   (define (->elong::elong n)
      (cond
	 ((elong? n) n)
	 ((fixnum? n) (fixnum->elong n))
	 ((bignum? n) (bignum->elong n))
	 ((string? n) (string->elong n))
	 ((real? n) (->elong (flonum->fixnum n)))
	 (else (bigloo-type-error "ioctl" "elong pair" n))))
   
   (cond-expand
      (bigloo-c ($ioctl dev (request->elong request) (->elong val)))
      (else (error "ioctl" "not supported by backend" dev))))

;*---------------------------------------------------------------------*/
;*    umask ...                                                        */
;*---------------------------------------------------------------------*/
(define (umask #!optional mask)
   (if (integer? mask)
       ($umask mask)
       (let ((old ($umask 0)))
	  ($umask old)
	  old)))

;*---------------------------------------------------------------------*/
;*    openlog ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (openlog ident::bstring option::int facility::int)
   (cond-expand
      ((and bigloo-c (config have-syslog #t))
       ($openlog ident option facility)))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    syslog ...                                                       */
;*---------------------------------------------------------------------*/
(define (syslog priority::int . args)
   (cond-expand
      ((and bigloo-c (config have-syslog #t))
       ($syslog priority "%s"
	  (call-with-output-string
	     (lambda (op)
		(for-each (lambda (a) (display a op)) args))))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    closelog ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (closelog)
   (cond-expand
      ((and bigloo-c (config have-syslog #t)) ($closelog)))
    #unspecified)

;*---------------------------------------------------------------------*/
;*    syslog-option ...                                                */
;*---------------------------------------------------------------------*/
(define (syslog-option . opts)
   (cond-expand
      ((and bigloo-c (config have-syslog #t))
       (let loop ((opts opts)
		  (o 0))
	  (if (null? opts)
	      o
	      (loop (cdr opts)
		 (bit-or o
		    (case (car opts)
		       ((LOG_CONS) $syslog-log-cons)
		       ((LOG_NDELAY) $syslog-log-ndelay)
		       ((LOG_NOWAIT) $syslog-log-nowait)
		       ((LOG_ODELAY) $syslog-log-odelay)
		       ;; ((LOG_PERROR) $syslog-log-perror)
		       ((LOG_PID) $syslog-log-pid)
		       (else (error "syslog-option"
				"unknown option" (car opts)))))))))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    syslog-facility ...                                              */
;*---------------------------------------------------------------------*/
(define (syslog-facility facility)
   (cond-expand
      ((and bigloo-c (config have-syslog #t))
       (case facility
	  ((LOG_AUTH) $syslog-log-auth)
	  ((LOG_AUTHPRIV) $syslog-log-authpriv)
	  ((LOG_CRON) $syslog-log-cron)
	  ((LOG_DAEMON) $syslog-log-daemon)
	  ((LOG_FTP) $syslog-log-ftp)
	  ((LOG_KERN) $syslog-log-kern)
	  ((LOG_LOCAL0) $syslog-log-local0)
	  ((LOG_LOCAL1) $syslog-log-local1)
	  ((LOG_LOCAL2) $syslog-log-local2)
	  ((LOG_LOCAL3) $syslog-log-local3)
	  ((LOG_LOCAL4) $syslog-log-local4)
	  ((LOG_LOCAL5) $syslog-log-local5)
	  ((LOG_LOCAL6) $syslog-log-local6)
	  ((LOG_LOCAL7) $syslog-log-local7)
	  ((LOG_LPR) $syslog-log-lpr)
	  ((LOG_MAIL) $syslog-log-mail)
	  ((LOG_NEWS) $syslog-log-news)
	  ((LOG_SYSLOG) $syslog-log-syslog)
	  ((LOG_USER) $syslog-log-user)
	  ((LOG_UUCP) $syslog-log-uucp)
	  (else (error "syslog-facility" "unknown facility" facility))))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    syslog-level ...                                                 */
;*---------------------------------------------------------------------*/
(define (syslog-level lvl)
   (cond-expand
      ((and bigloo-c (config have-syslog #t))
       (case lvl
	  ((LOG_EMERG) $syslog-log-emerg)
	  ((LOG_ALERT) $syslog-log-alert)
	  ((LOG_CRIT) $syslog-log-crit)
	  ((LOG_ERR) $syslog-log-err)
	  ((LOG_WARNING) $syslog-log-warning)
	  ((LOG_NOTICE) $syslog-log-notice)
	  ((LOG_INFO) $syslog-log-info)
	  ((LOG_DEBUG) $syslog-log-debug)
	  (else (error "syslog-level" "unknown level" lvl))))
      (else
       0)))
