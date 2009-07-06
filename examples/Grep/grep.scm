;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Grep/grep.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 10 11:13:41 2006                          */
;*    Last change :  Thu Aug 16 10:20:00 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple grep implementation in Scheme                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module grep
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (cond
      ((null? (cdr args))
       (fprintf (current-error-port) "Usage: grep STRING [FILE]...")
       (exit 0))
      (else
       (let ((t (kmp-table (cadr args))))
	  (for-each (lambda (f) (kmp-file t (cadr args) f)) (cddr args))))))

;*---------------------------------------------------------------------*/
;*    kmp-table ...                                                    */
;*---------------------------------------------------------------------*/
(define (kmp-table p)
   (let* ((lp (string-length p))
	  (t (make-vector (+fx lp 2) 0))
	  (i 0)
	  (j -1)
	  (c #a000))
      (vector-set! t 0 j)
      (let while ()
	 (when (<fx i lp)
	    (cond
	       ((char=? (string-ref p i) c)
		(vector-set! t (+fx i 1) (+fx j 1))
		(set! j (+fx j 1))
		(set! i (+fx i 1)))
	       ((>fx j 0)
		(set! j (vector-ref t j)))
	       (else
		(vector-set! t (+fx i 1) 0)
		(set! i (+fx i 1))
		(set! j 0)))
	    (set! c (string-ref p j))
	    (while)))
      t))

;*---------------------------------------------------------------------*/
;*    kmp-mmap ...                                                     */
;*---------------------------------------------------------------------*/
(define (kmp-mmap t p mm ls lp m)
   (let while ((i 0))
      (cond
	 ((=fx i lp)
	  m)
	 ((>=fx (+fx i m) ls)
	  -1)
	 (else
	  (if (char=? ($mmap-ref mm (+fx i m)) (string-ref p i))
	      (while (+fx i 1))
	      (let ((ti (vector-ref t i)))
		 (set! m (+fx m (-fx i ti)))
		 (if (>fx i 0)
		     (while ti)
		     (while i))))))))

;*---------------------------------------------------------------------*/
;*    kmp-file ...                                                     */
;*---------------------------------------------------------------------*/
(define (kmp-file t p file)
   (let* ((mm (open-mmap file read: #t write: #f))
	  (ls (mmap-length mm))
	  (lp (string-length p)))
      (let loop ((o 0))
	 (unless (>=fx o ls)
	    (let ((n (kmp-mmap t p mm ls lp o)))
	       (when (>fx n 0)
		  (print file ":" (mmap-line mm ls n))
		  (loop (+fx n 1))))))
      (close-mmap mm)))

;*---------------------------------------------------------------------*/
;*    mmap-line ...                                                    */
;*---------------------------------------------------------------------*/
(define (mmap-line mm ls n)
   (let ((b 0)
	 (e (elong->fixnum ls)))
      ;; beginning
      (let loop ((i n))
	 (when (>fx i 0)
	    (if (char=? ($mmap-ref mm i) #\Newline)
		(set! b (+fx i 1))
		(loop (-fx i 1)))))
      ;; end
      (let loop ((i n))
	 (when (<fx i ls)
	    (if (char=? ($mmap-ref mm i) #\Newline)
		(set! e i)
		(loop (+fx i 1)))))
      (mmap-substring mm b e)))


