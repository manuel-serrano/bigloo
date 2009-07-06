(module pnet2ms
  (main main)
  (use (win32-base))
  (use (win32-registry))
  (use (win32-helpers))
  (extern
   (macro BGL_RELEASE_NUMBER::string "BGL_RELEASE_NUMBER")))


; Current Bigloo version in form x:x:x:0
(define *bigloo-version-doubledots*
  (let ((matches (pregexp-match (pregexp "(\\d)\\.(\\d)([a-z])") BGL_RELEASE_NUMBER)))
    (if (not matches)
        (error *bigloo-version-doubledots* "Could not parse bigloo version:" BGL_RELEASE_NUMBER))
    (string-append (cadr matches)
                   ":"
                   (caddr matches)
                   ":"
                   (integer->string (-fx (char->integer (string-ref (cadddr matches) 0))
                                         (-fx (char->integer #\a) 1)))
                   ":0")))


; The list of Bigloo .NET runtime DLLs
(define *bigloo-dlls*
  (map (lambda (prefix)
         (string-append prefix BGL_RELEASE_NUMBER))
       '("bigloo_u-" "bigloo_s-" "bigloopth_u-" "bigloopth_s-" "bigloofth_u-" "bigloofth_s-")))


; ********************************************* pathes *********************************************

; Temporary directory (not Cygwin /tmp)
(define *temp-dir*::bstring (GetTemporaryPath))

; MS .NET Framework path
(define *dotnet-framework*
  (let ((attempt (make-file-path (getenv "SYSTEMROOT") "Microsoft.NET" "Framework" "v1.1.4322")))
    (if (directory? attempt)
        attempt
        (let ((attempt (make-file-path (getenv "SYSTEMROOT") "Microsoft.NET" "Framework" "v1.0.3705")))
          (if (not (directory? attempt))
              (error *dotnet-framework* "Could not determine path to MS .NET Framework" attempt)
              attempt)))))

; MS .Net Framework SDK path
(define *dotnet-framework-sdk*
  (try (GetRegistryValue HKEY_LOCAL_MACHINE "SOFTWARE\\Microsoft\\.NETFramework" "sdkInstallRootv1.1")
       (lambda (cont proc msg obj)
         (try (cont (GetRegistryValue HKEY_LOCAL_MACHINE "SOFTWARE\\Microsoft\\.NETFramework" "sdkInstallRoot"))
              (lambda (cont proc msg obj)
                (try (let ((attempt (getenv "FrameworkSDKDir")))
                       (if (directory? attempt)
                           (cont attempt)
                           (error *dotnet-framework-sdk* "Could not find the path to the Microsoft .NET Framework SDK" #f)))
                     (lambda (cont proc msg obj)
                       (error *dotnet-framework-sdk* "Could not find the path to the Microsoft .NET Framework SDK" #f))))))))

; MS .NET assembler
(define *ilasm*
  (let ((attempt (make-file-path *dotnet-framework* "ilasm.exe")))
    (if (not (file-exists? attempt))
        (error *ilasm* "Could not determine path to MS .NET assembler" attempt)
        attempt)))

; MS .NET Global Assembly Cache Utility
(define *gacutil*
  (let ((attempt1 (make-file-path *dotnet-framework* "gacutil.exe")))
    (if (not (file-exists? attempt1))
        (let ((attempt2 (make-file-path *dotnet-framework-sdk* "Bin" "gacutil.exe")))
          (if (not (file-exists? attempt2))
              (error *gacutil* (string-append "Could not find the MS .NET Global Assembly Cache Utility neither at " attempt1 " nor at " attempt2) #f)
              attempt2))
        attempt1)))

; MS .NET disassembler
(define *ildasm*
  (let ((attempt (make-file-path *dotnet-framework-sdk* "Bin" "ildasm.exe")))
    (if (not (file-exists? attempt))
        (error *ildasm* "Could not determine path to MS .NET disassembler: " attempt)
        attempt)))


; **************************************** regular expressions ****************************************

; Regular expression for global cache entry
(define *global-cache-entry-regexp*
  (pregexp "\t([A-Za-z0-9\\-_.]+), Version=(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+), Culture=neutral, PublicKeyToken=([0-9a-fA-F]+).*"))

; Regular expression for external assembly reference
(define *assembly-extern-regexp*
  (pregexp "\\.assembly extern '?([A-Za-z0-9\\-_.]+)'?"))

; Regular expression for external assembly reference
(define *assembly-regexp*
  (pregexp "\\.assembly '?([A-Za-z0-9\\-_.]+)'?"))

; Hash algorithm code
(define *hash-algorithm-regexp*
  (pregexp "\\.hash algorithm .+"))

; Regular expression for local variable declaration
(define *locals-regexp*
  (pregexp "(\\s*\\.locals)(\\s*\\(.*)"))


; ************************************** command-line options **************************************

; Indicates whether local variables should be set as initialized by the VM
(define *initlocals* #f)

; Indicates whether to keep intermediate .il files
(define *keep-intermediate-files* #f)

; Indicates whether to register the DLL in the global assembly cache
(define *register-DLL-in-cache* #f)

; Indicates whether verbose mode is enabled
(define *verbose-level* 0)


; *********************************** verbose and usage messages ***********************************

; The current verbose gap
(define *verbose-current-gap* 0)

; Increases the current verbose gap
(define (verbose-increase-gap!)
  (set! *verbose-current-gap* (+ *verbose-current-gap* 1)))

; Decreases the current verbose gap
(define (verbose-decrease-gap!)
  (set! *verbose-current-gap* (- *verbose-current-gap* 1)))

; Prints the specified items if the specified verbose level is lower than the current one
(define (verbose-print level::int . L)
  (if (<= level *verbose-level*)
      (apply print (make-string (* 2 *verbose-current-gap*) #\space) L)))

; Prints the usage help line
(define (usage)
  (print "usage: pnet2ms [options] filename ...")
  (print "  -h, --help    Displays this help message")
  (print "  -initlocals   Add 'init' to local variable definitions")
  (print "  -k            Keep intermediate files")
  (print "  -register     Register DLLs in the Global Assembly Cache")
  (print "  -v            Enable verbose mode")
  (print "  -v2           Enable very verbose mode")
  (print "  -v3           Enable very very verbose mode")
  (exit 69))


; **************************************************************************************************

; Returns the specified value in the Win32 registry
(define (GetRegistryValue::string open_key::HKEY subkey_name::string value_name::string)
  (let ((phkey (make-PHKEY 1)))
    (if (not (= (RegOpenKeyEx open_key subkey_name 0 KEY_QUERY_VALUE phkey) ERROR_SUCCESS))
        (error GetRegistryValue "Failed to open key" subkey_name))
    (let ((value_length (make-LPDWORD 1)))
      (if (not (= (RegQueryValueEx (PHKEY-ref phkey 0) value_name (make-null-LPDWORD) (make-null-LPDWORD) (pragma::LPSTR "NULL") value_length) ERROR_SUCCESS))
          (error GetRegistryValue "Failed to query value length" value_name))
      (let ((result (make-string (LPDWORD-ref value_length 0))))
        (if (not (= (RegQueryValueEx (PHKEY-ref phkey 0) value_name (make-null-LPDWORD) (make-null-LPDWORD) result value_length) ERROR_SUCCESS))
            (error GetRegistryValue "Failed to query value" value_name))
        (RegCloseKey (PHKEY-ref phkey 0))
        result))))

; Compares two lists of numbers
; Returns
;   <0  if L1 older than L2
;   0   if L1 equals L2
;   >1  if L1 greater than L2
(define (compare-number-lists L1 L2)
  (if (null? L1)
      (if (null? L2)
          0
          (error compare-number-lists "L2 not null while L1 is" L2))
      (if (null? L2)
          (error compare-number-lists "L1 not null while L2 is" L1)
          (if (= (car L1) (car L2))
              (compare-number-lists (cdr L1) (cdr L2))
              (- (car L1) (car L2))))))

; Returns the list (version-as-list version-as-string publickeytoken) of the specified assembly
(define (get-assembly-info assembly-name)
  (verbose-increase-gap!)
  (let ((command-line (list *gacutil* "/l" assembly-name output: pipe:)))
    (verbose-print 3 "> " command-line)
    (let* ((gacutil-process (apply run-process command-line))
           (gacutil-output-port (process-output-port gacutil-process))
           (result #f))
      (let loop ((line (read-line gacutil-output-port)))
        (if (eof-object? line)
            (close-input-port gacutil-output-port)
            (begin
              (verbose-print 3 "> " line)
              (let ((match (pregexp-match *global-cache-entry-regexp* line)))
                (if (and match (string=? (cadr match) assembly-name))
                    (let* ((version (cddr match))
                           (Lversion (list (string->number (car version))
                                           (string->number (cadr version))
                                           (string->number (caddr version))
                                           (string->number (cadddr version)))))
                      (verbose-print 2 "Found assembly " assembly-name " version " Lversion)
                      (if (or (not result)
                              (< (compare-number-lists (car result) Lversion)
                                 0))
                          (set! result (list Lversion
                                             (string-append (car version) ":"
                                                            (cadr version) ":"
                                                            (caddr version) ":"
                                                            (cadddr version))
                                             (list-ref match 6))))))
                (loop (read-line gacutil-output-port))))))
      (if result
          (verbose-print 2 "Most recent assembly found: " (cadr result)))
      (verbose-decrease-gap!)
      (if (not result)
          (error get-assembly-info "Could not find an entry in the global assembly cache for assembly" assembly-name)
          result))))

;***** Go! *****
(define (go filename)
  ;***** checking for file existence *****
  (if (not (file-exists? filename))
      (error go "File does not exist" filename))
  (let* ((filename-basename (basename filename))
         (ildasm-output (string-append *temp-dir* filename-basename ".il"))
         (ilasm-input (string-append *temp-dir* filename-basename ".corrected.il"))
         (ilasm-output (string-append filename-basename ".corrected." (suffix filename))))
    ;***** disassembling *****
    (verbose-print 1 "Disassembling " filename " to " ildasm-output)
    (let ((command-line (list *ildasm*
                              "/NOBAR"
                              (string-append "/OUT=" ildasm-output)
                              filename
                              wait: #t)))
      (verbose-print 3 "> " command-line)
      (let ((ildasm-go (apply run-process command-line)))
        (let ((ildasm-exit-status (process-exit-status ildasm-go)))
          (if (not (zero? ildasm-exit-status))
              (error go "ildasm failed: " ildasm-exit-status)))))
    ;***** correcting file *****
    (verbose-print 1 "Correcting " ildasm-output " to " ilasm-input)
    (call-with-input-file ildasm-output
      (lambda (input)
        (call-with-output-file ilasm-input
          (lambda (output)
            (let loop ((line (read-line input)))
              (if (not (eof-object? line))
                  (begin
                    (if (not (let ((line-length (string-length line)))
                               (and (> line-length 0)
                                    (let loop ((i 0))
                                      (let ((current-char (string-ref line i)))
                                        (or (char=? current-char #\.)
                                            (and (char=? current-char #\space)
                                                 (< i line-length)
                                                 (loop (+ i 1)))))))))
                        (fprint output line)
                        (cond ((pregexp-match *locals-regexp* line)
                               => (lambda (match)
                                    (if *initlocals*
                                        (fprint output (cadr match) " init" (caddr match))
                                        (fprint output line))))
                              ((pregexp-match *assembly-extern-regexp* line)
                               => (lambda (match)
                                    ;***** external assembly ref *****
                                    (let ((assembly-name (cadr match)))
                                      (verbose-print 1 "Correcting external assembly definition " assembly-name)
                                      (let ((correction (get-assembly-info assembly-name)))
                                        (fprint output line)
                                        (let ((line (read-line input)))
                                          (if (not (string=? line "{"))
                                              (error go "{ expected, read: " line)
                                              (fprint output line)))
                                        (let ((line (read-line input)))
                                          (if (substring=? line "  .publickeytoken" 17)
                                              (begin
                                                (fprint output line)
                                                (let ((line (read-line input)))
                                                  (if (not (substring=? line "  .ver" 6))
                                                      (error go ".ver expected, read: " line)
                                                      (fprint output line))))
                                              (if (not (substring=? line "  .ver" 6))
                                                  (error go ".ver expected, read: " line)
                                                  (begin (fprint output "  .ver " (cadr correction))
                                                         (fprint output "  .publickeytoken = (" (caddr correction) ")")))))
                                        (let ((line (read-line input)))
                                          (if (not (string=? line "}"))
                                              (error go "} expected, read: " line)
                                              (fprint output line)))))))
                              ((pregexp-match *assembly-regexp* line)
                               => (lambda (match)
                                    ;***** assembly declaration *****
                                    (if (not (member (cadr match) *bigloo-dlls*))
                                        (fprint output line)
                                        (begin
                                          ;***** correcting Bigloo assembly declaration *****
                                          (verbose-print 1 "Correcting assembly definition " (cadr match))
                                          (fprint output line)
                                          (let ((line (read-line input)))
                                            (if (not (string=? line "{"))
                                                (error go "{ expected, read: " line)
                                                (fprint output line)))
                                          (let ((line (read-line input)))
                                            (if (pregexp-match *hash-algorithm-regexp* line)
                                                (begin (fprint output line)
                                                       (set! line (read-line input))))
                                            (if (not (substring=? line "  .ver" 6))
                                                (error go ".ver expected, read: " line)
                                                (fprint output "  .ver " *bigloo-version-doubledots*)))
                                          (let ((line (read-line input)))
                                            (if (not (string=? line "}"))
                                                (error go "} expected, read: " line)
                                                (fprint output line)))))))
                              (else (fprint output line))))
                    (loop (read-line input)))))))))
    ;***** reassembling *****
    (verbose-print 1 "Reassembling " ilasm-input " to " ilasm-output)
    (let* ((ilasm-common-args (list *ilasm* "/QUIET" "/NOLOGO" (string-append "/OUTPUT=" ilasm-output) ilasm-input output: null: wait: #t))
           (command-line (if (string-ci=? (suffix ilasm-output) "dll")
                             (append ilasm-common-args (list "/DLL" "/KEY=@Bigloo"))
                             ilasm-common-args)))
      (verbose-print 3 "> " command-line)
      (let* ((ilasm-go (apply run-process command-line))
             (ilasm-exit-status (process-exit-status ilasm-go)))
        (if (not (zero? ilasm-exit-status))
            (error go "ilasm failed: " ilasm-exit-status))))
    ;***** replacing input file *****
    (verbose-print 1 "Replacing " filename " by " ilasm-output)
    (if (not (delete-file filename))
        (error go "Could not delete input file " filename))
    (if (not (rename-file ilasm-output filename))
        (error go "Could not replace input file by " ilasm-output))
    ;***** registering the DLL *****
    (if *register-DLL-in-cache*
        (begin
          (verbose-print 1 "Registering " filename)
          (let* ((gacutil-process (run-process *gacutil*
                                               "/if"
                                               filename
                                               output: pipe:))
                 (gacutil-output-port (process-output-port gacutil-process))
                 (gacutil-output (open-output-string)))
            (let loop ((line (read-line gacutil-output-port)))
              (if (eof-object? line)
                  (close-input-port gacutil-output-port)
                  (begin
                    (fprint gacutil-output line)
                    (loop (read-line gacutil-output-port)))))
            (process-wait gacutil-process)
            (let ((gacutil-exit-status (process-exit-status gacutil-process)))
              (if (zero? gacutil-exit-status)
                  (close-output-port gacutil-output)
                  (begin
                    (warning (string-append "gacutil failed to register " filename))
                    (print (close-output-port gacutil-output))))))))
    ;***** removing temporary files *****
    (if (not *keep-intermediate-files*)
        (begin
          (if (not (delete-file ildasm-output))
              (warning "Could not delete temporary file " ildasm-output))
          (if (not (delete-file ilasm-input))
              (warning "Could not delete temporary file " ilasm-input))))))

;***** Entry-point function *****
(define (main Largs)
  (let ((Lfiles '()))
    ; ***** parsing the command-line *****
    (let loop ((Largs (cdr Largs)))
      (if (not (null? Largs))
          (let ((current-arg (car Largs)))
            (if (substring=? current-arg "-" 1)
                (string-case current-arg
                             ("-h"          (usage))
                             ("--help"      (usage))
                             ("-initlocals" (set! *initlocals* #t))
                             ("-k"          (set! *keep-intermediate-files* #t))
                             ("-register"   (set! *register-DLL-in-cache* #t))
                             ("-v"          (set! *verbose-level* 1))
                             ("-v2"         (set! *verbose-level* 2))
                             ("-v3"         (set! *verbose-level* 3))
                             (else          (usage)))
                (if (and (not (string-ci=? (suffix current-arg) "dll"))
                         (not (string-ci=? (suffix current-arg) "exe")))
                    (error main "Input file must have extension .exe or .dll" current-arg)
                    (set! Lfiles (append Lfiles (list current-arg)))))
            (loop (cdr Largs)))))
    ; ***** verifying command-line *****
    (if (null? Lfiles)
        (usage))
    ; ***** displaying Bigloo version *****
    (verbose-print 2 "Bigloo version: " BGL_RELEASE_NUMBER " -> " *bigloo-version-doubledots*)
    (verbose-print 2 "Temporary path: " *temp-dir*)
    ; ***** dumping tool pathes *****
    (verbose-print 2 "Tools used:")
    (verbose-increase-gap!)
    (verbose-print 2 "ildasm:   " *ildasm*)
    (verbose-print 2 "ilasm:    " *ilasm*)
    (verbose-print 2 "gacutil:  " *gacutil*)
    (verbose-decrease-gap!)
    ; ***** processing files *****
    (let loop ((Lfiles Lfiles))
      (if (not (null? Lfiles))
          (begin
            (verbose-print 1 "Processing file " (car Lfiles) "...")
            (verbose-increase-gap!)
            (go (car Lfiles))
            (verbose-decrease-gap!)
            (loop (cdr Lfiles)))))
    (exit 0)))

