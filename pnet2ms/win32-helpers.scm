(module win32-helpers
  (use (win32-base))
  (export (GetErrorMessage::bstring error-code::LONG)
          (GetTemporaryPath::bstring)))

;; Returns the error message of specified code
(define (GetErrorMessage::bstring error-code::LONG)
  (let* ((buffer-length 1024)
         (result (make-string buffer-length)))
    (if (zero? (FormatMessage (+ FORMAT_MESSAGE_FROM_SYSTEM FORMAT_MESSAGE_IGNORE_INSERTS)
                              (make-null-LPVOID)
                              error-code
                              0
                              result
                              buffer-length
                              (make-null-LPVOIDARRAY)))
        "[Could not retrieve error message]"
        result)))

;; Returns the temporary path, ending with a backslash
(define (GetTemporaryPath::bstring)
  (let* ((buffer-length 1024)
         (result (make-string buffer-length))
         (len (GetTempPath buffer-length result)))
    (if (zero? len)
        (error GetTempPath (string-append "Could not retrieve temporary path (" (GetErrorMessage (GetLastError)) ")") #f)
        (string-shrink! result len))))
