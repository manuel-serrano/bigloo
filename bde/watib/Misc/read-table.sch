(define-macro (read-table name f)
   (call-with-input-file f
      (lambda (p)
         `(define ,name
             (let ((h (make-hashtable)))
                (for-each
                 (match-lambda
                   ((?sym . ?cont) (hashtable-put! h sym cont)))
                 ,(read p))
                h)))))
