(module jas_as
   (import jas_lib jas_classfile jas_produce jas_profile
	   jas_opcode jas_peep jas_wide jas_labels jas_stack )
   (export (jvm-as ::obj ::binary-port)
	   (jvm-asfile ::obj ::obj)
	   *jas-warning*
	   *jvm-char-info*) )

;;
;; set to #t to enable large function warning
;;
(define *jas-warning* #f)

;;
;; Generate char info for the debugger (in addition to line info)
;;
(define *jvm-char-info* #f)

;;
;; Goodies for brAun 
;;
;; - SourceDebugExtension
(define **sdefile** #f)
(define **sdefullfile** #f)
(define **maxpos** 0)
(define **maxchar** 0)
(define **maxline** 0)
(define *profile* #f)
(define **all-lines** '())
(define **pos->line** '())

;;
;; Main function
;;
(define (jvm-asfile filein fileout)
   (let ( (port (open-output-binary-file fileout)) )
      (if (not (binary-port? port))
	  (error "Jas" "Can't open file for output" fileout))
      (with-handler
	 (lambda (e)
	    (delete-file fileout)
	    (raise e))
	 (unwind-protect
	    (jvm-as (call-with-input-file filein read) port)
	    (close-binary-port port)))))
 
(define (jvm-as l outchan::binary-port)
   (let ( (classfile (as (jas-profile l))) )
      ;; If compiling a module, reorder methods so that closures are
      ;; produced in the order they appear in the tableswitch of "apply".
      (reorder-classfile classfile l)
      (produce outchan classfile)
      (for-each (lambda (s) (remprop! s 'jas-global-value))
		(classfile-globals classfile) )))


;; Fix the order in which closures appear into the classfile. This
;; way, the index of a closure object can be directly mapped to a
;; method in the classfile (no need to add debug infos in the classfile)
(define (reorder-classfile classe::classfile l)
   (define defs (cddr (cdddr l)))

   (define (get-procedure-code l procname)
      (match-case l
	 (() #f)
	 (( (method ?gname ?params ?locals . ?code) . ?rest)
	  (let ( (m (declared-method classe gname)) )
	     (if (equal? (method-name m) procname)
		 code
		 (get-procedure-code (cdr l) procname))))
	 (else (get-procedure-code (cdr l) procname)) ))

   (define (get-closures code acc)
      (cond
	 ((null? code) acc) ; put closure at the end, so no reverse
	 ((and (pair? (car code)) (eq? (caar code) 'invokestatic)
	       (not (eq? (cadar code) 'list_to_vector)) )
	  (get-closures (cdr code)
			(cons (closure->method (cadar code)) acc)))
	 (else (get-closures (cdr code) acc))))

   ;; get the ::method associated to this closure. If the closure is
   ;; a stub, get the ::method of the real closure in the source code
   (define (closure->method name)
      (let* ((m (declared-method classe name))
	     (stub (method-name m)))
	 (if (or (substring=? stub "_" 1)
		 (substring=? stub "BgL__" 5))
	     ;; check if a user procedure exists without an _
	     (let* ((owner (symbol->string (method-owner m)))
		    (name2 (symbol->string name))
		    (nameproc (string-append
			       (substring name2 0 (string-length owner))
			       (substring name2
					  (+fx 1 (string-length owner))
					  (string-length name2))))
		    (proc-in-code (filter
				   (lambda (x)
				      (and (pair? x)
					   (eq? (car x) 'invokestatic)
					   (eq? (cadr x)
						(string->symbol nameproc))))
				   (get-procedure-code l (method-name m)))))
		;; if it exists and it is used, return this user procedure
		;; otherwise return the stub procedure...
		(if (not (null? proc-in-code))
		    (declared-method classe (cadr (car proc-in-code)))
		    (declared-method classe name)))
	     (declared-method classe name))))

   (let ( (code (get-procedure-code defs "apply")) )
      (if code
	  (let ( (closures (get-closures code '()))
		 (methods (classfile-methods classe)) )
	     ;; wipe out closures of the current list of methods (slow ?)
	     (for-each (lambda (x) (set! methods (remq! x methods))) closures)
	     ;; and put closures at the end of the list
	     (classfile-methods-set! classe (append methods closures)) ))))

;;
;; ast -> classfile
;;
(define (as l)
   (match-case l
      ((?key (and ?this (? symbol?))
	     (and ?extend (? symbol?))
	     (and ?implements ((? symbol?) ...))
	     (declare . ?decls) . ?infos )
       (let ( (classfile (instantiate::classfile)) )
	  (map (lambda (decl) (as-declare classfile decl)) decls)
	  (set-field-method-type classfile)
	  (with-access::classfile classfile (flags me super interfaces)
	     (define (get-classe name) (pool-class-by-name classfile name))
	     (let ( (cthis (declared-class classfile this))
		    (cextend (declared-class classfile extend)) )
		(set! flags (classe-flags cthis))
		(set! me (pool-class classfile cthis)) )
	     (set! super (get-classe extend))
	     (set! interfaces (map get-classe implements))
	     (scan-infos classfile infos)
	     (if (string? **sdefullfile**)
		 (with-access::classfile classfile (attributes)
		    (set! attributes (cons (smap classfile) attributes)) )))
	  classfile ))
       (else (error "jas" "bad module definition" l)) ))

(define (set-field-method-type classfile)
   (for-each
    (lambda (slot)
       ;let ( (value (cdr slot)) )
       (let ( (value (getprop slot 'jas-global-value)) )
	  (if (field-or-method? value)
	      (with-access::field-or-method value (usertype type)
		 (set! type (as-type classfile usertype)) ))))
    (classfile-globals classfile) ))

(define (scan-infos classfile infos)
   (cond
      ((null? infos) classfile)
      ((eq? (caar infos) 'fields)
       (with-access::classfile classfile (fields)
	  (set! fields (map (lambda (f) (as-field classfile f)) (cdar infos)))
	  (scan-infos classfile (cdr infos)) ))
      ((eq? (caar infos) 'sourcefile)
       (with-access::classfile classfile (attributes)
	  (set! attributes (cons (srcfile classfile (cadar infos)) attributes))
	  (scan-infos classfile (cdr infos)) ))
      ((eq? (caar infos) 'sde)
       (set! **sdefile** (cadar infos))
       (set! **sdefullfile** (caddar infos))
       (scan-infos classfile (cdr infos)) )
      (else
       (with-access::classfile classfile (methods)
	  (set! methods (map (lambda (m) (as-method classfile m)) infos))
	  ;; Damien: empty char info crash the JVM
	  (if (null? **all-lines**) (set! *jvm-char-info* #f))
	  (if *jvm-char-info* (pos-info->line-info!)) ))))

;;
;; Sourcefile
;;
(define (srcfile classfile name)
   (instantiate::attribute
      (type   'srcfile)
      (name   (pool-name classfile "SourceFile"))
      (size   2)
      (info   (pool-name classfile name)) ))

;;
;; SMAP
;;
(define (smap classfile)
   (let ( (smapfile (string->utf8 (smapfile))) )
      (instantiate::attribute
	 (type 'bytevector)
	 (name (pool-name classfile "SourceDebugExtension"))
	 (size (string-length smapfile))
	 (info smapfile) )))

(define (smapfile)
   (string-append
    "SMAP\n"
    **sdefile** "\n"
    ;; default stratum
    "Bigloo" "\n"
    ;; fisrt stratum: full path for source file
    "*S " (if *jvm-char-info*
	      "BiglooChar"
	      "Bigloo") "\n"
    "*F\n"
    "+ 1 " **sdefile** "\n"
    **sdefullfile** "\n"
    "*L\n"
    "1#1," (integer->string (if *jvm-char-info*
				**maxchar**
				**maxline**)) ":1\n"
    ;; second stratum: mapping character->line and full path for source file
    (if *jvm-char-info*
	(string-append
	 "*S Bigloo\n"
	 "*F\n"
	 "+ 1 " **sdefile** "\n"
	 **sdefullfile** "\n"
	 "*L\n"
	 (apply string-append
		(map (lambda (x)
			(format "~a#1,1:~a,~a\n"
				(car x) (cadr x)
				(+fx 1 (-fx (caddr x) (cadr x)))))
		     **pos->line**)))
	"")
    "*E\n" ))

;;
;; Declaration
;;
(define (as-declare classfile decl)
   (match-case decl
      ((?gname ?value)
       (as-assign classfile gname
	  (match-case value
	     ((class ?modifiers (and (? string?) ?name))
	      (let ( (name/ (pathname name)) )
		 (instantiate::classe
		    (code  (string-append "L" name/ ";"))
		    (flags (as-class-modifiers modifiers))
		    (name name/) )))
	     ((field ?class ?modifiers ?type (and (? string?) ?name))
	      (instantiate::field
		 (flags (as-field-modifiers modifiers))
		 (name name)
		 (owner class)
		 (usertype type) ))
	     ((method ?class ?modifiers ?tret (and (? string?) ?name) . ?targs)
	      (instantiate::method
		 (flags (as-method-modifiers modifiers))
		 (name name)
		 (owner class)
		 (usertype `(function ,tret ,@targs)) ))
	     (else (jas-error classfile "bad declaration" decl)) )))
      (else (jas-error classfile "bad declaration" decl)) ))

(define (pathname str)
   (let* ((len (string-length str))
	  (res (make-string len)))
      (let loop ((i (-fx len 1)))
	 (cond
	    ((=fx i -1)
	     res)
	    ((char=? (string-ref str i) #\.)
	     (string-set! res i #\/)
	     (loop (-fx i 1)))
	    (else
	     (string-set! res i (string-ref str i))
	     (loop (-fx i 1)))))))

;; modifiers
(define (as-class-modifiers modifiers)
   (let ( (r 0) )
      (for-each
       (lambda (name)
	  (case name
	     ((public)       (set! r (bit-or r #x0001)))
	     ((final)        (set! r (bit-or r #x0010)))
	     ((super)        (set! r (bit-or r #x0020)))
     ((interface)    (set! r (bit-or r #x0200)))
	     ((abstract)     (set! r (bit-or r #x0400)))
	     (else (error "as" "bad method modifier" name)) ))
       modifiers )
      r ))

(define (as-field-modifiers modifiers)
   (let ( (r 0) )
      (for-each
       (lambda (name)
	  (case name
	     ((public)       (set! r (bit-or r #x0001)))
	     ((private)      (set! r (bit-or r #x0002)))
	     ((protected)    (set! r (bit-or r #x0004)))
	     ((static)       (set! r (bit-or r #x0008)))
	     ((final)        (set! r (bit-or r #x0010)))
	     ((volatile)     (set! r (bit-or r #x0040)))
	     ((transient)    (set! r (bit-or r #x0080)))
	     (else (error "as" "bad field modifier" name)) ))
       modifiers )
      r ))

(define (as-method-modifiers modifiers)
   (let ( (r 0) )
      (for-each
       (lambda (name)
	  (case name
	     ((public)       (set! r (bit-or r #x0001)))
	     ((private)      (set! r (bit-or r #x0002)))
	     ((protected)    (set! r (bit-or r #x0004)))
	     ((static)       (set! r (bit-or r #x0008)))
	     ((final)        (set! r (bit-or r #x0010)))
	     ((synchronized) (set! r (bit-or r #x0020)))
	     ((native)       (set! r (bit-or r #x0100)))
	     ((abstract)     (set! r (bit-or r #x0400)))
	     (else (error "as" "bad method modifier" name)) ))
       modifiers )
      r ))

;;;
;;; FIELDS
;;;
(define (as-field classfile fieldname)
   (let ( (field (declared-field classfile fieldname)) )
      ;(if (not (field-pool field))
      ;   (jas-warning classfile "unused field" (field-name field)) )
      (pool-field classfile field)
      field ))

;;;
;;; METHODS
;;;
(define (as-method classfile decl)
   (match-case decl
      ((method ?gname ?params ?locals . ?code)
       (classfile-current-method-set! classfile gname)
       (let ( (m (declared-method classfile gname)) )
	  (pool-local-method classfile m)	  
	  (with-access::method m (attributes)
	     (set! attributes (cons (as-code classfile params locals code)
				    attributes )))
	  m ))
      (else (error "as" "bad method definition" decl)) ))

(define (as-code classfile param locals code)
   (let* ( (l1 (resolve-opcodes classfile param locals code))
	   (lp (peep classfile param locals l1))
	   (lw (resolve-wide classfile lp))
	   (l3 (resolve-labels classfile lw))
	   (handlers (get-handlers l3))
	   (lines0 (get-lines l3 0))
	   (lines (if *jvm-char-info*
		      lines0
		      (line-compress lines0 -1)))
	   (localvars (get-localvars l3))
	   (bytecode (get-bytecode l3)) )
      (set! **all-lines** (append! **all-lines** lines0))
      (let ( (n (length bytecode)) )
	 (if (and (>=fx n 8000) (not *jas-warning*))
	     (warning (classfile-current-method classfile) "Method too large. This may cause some troubles to Jvm jits (current size: " n
		      #", limit size: 8000).\n"
		      "You should consider splitting this function in small pieces.") ))
      ;; No line attribute makes JDI crazy!!
      (if (null? lines) (set! lines (cons (list 0 0 0) '())))
      (instantiate::attribute
	 (type   'code)
	 (name   (pool-name classfile "Code"))
	 (size   (+ 12 (length bytecode) (* 8 (length handlers))
		    (if (null? lines) 0 (+ 8 (* 4 (length lines))))
		    (if (null? localvars) 0 (+ 8 (* 10 (length localvars)))) ))
	 (info   `(,(stk-analysis classfile lp) 
		   ,**last-number-of-locals**
		   ,bytecode
		   ,handlers
		   ,@(if (null? lines) '()
			 (cons (make-line-attribute classfile lines) '()) )
		   ,@(if (null? localvars) '()
			 (cons (make-localvars classfile localvars) '()) ))))))

(define (make-line-attribute classfile lines)
   (instantiate::attribute
      (type   'linenumber)
      (name   (pool-name classfile "LineNumberTable"))
      (size   (+ 2 (* 4 (length lines))))
      (info   (map (if *jvm-char-info*
		       (lambda (x) (cons (car x) (caddr x)))
		       (lambda (x) (cons (car x) (cadr x)))) lines)) ))

(define (make-localvars classfile localvars)
   (instantiate::attribute
      (type   'localvariable)
      (name   (pool-name classfile "LocalVariableTable"))
      (size   (+ 2 (* 10 (length localvars))))
      (info   localvars) ))


(define (get-handlers l)
   (cond ((null? l) l)
	 ((eq? (caar l) 202) (cons (cdar l) (get-handlers (cdr l))))
	 (else               (get-handlers (cdr l))) ))


;; Damien: find the biggest char info to check wether -jvm-char-info is ok
(define (get-lines l pc)
   (cond ((null? l)
	  (when (and *jvm-char-info* (>fx **maxchar** 65535))
	     (warning "*jvm-char-info*" "Source file too large for generating character information (current size: " **maxchar**
		      #", limit size: 65535).\n"
		      "You should consider splitting this source file in small pieces.")
	     (set! *jvm-char-info* #f))
	  l)
	 ((eq? (caar l) 202) (get-lines (cdr l) pc))
	 ((eq? (caar l) 203)
	  (if (>fx (cadar l) **maxline**) (set! **maxline** (cadar l)))
	  (if (>fx (caddar l) **maxchar**) (set! **maxchar** (caddar l)))
	  (cons (list pc (cadar l) (caddar l)) (get-lines (cdr l) pc)))
	 ((eq? (caar l) 204) (get-lines (cdr l) pc))
	 ((eq? (caar l) 205) (get-lines (cdr l) pc))
	 (else (get-lines (cdr l) (+fx pc (length (car l))))) ))

(define (line-compress l line)
   (cond
      ((null? l) l)
      ((eq? (cadar l) line) (line-compress (cdr l) line))
      (else
	  (cons (car l) (line-compress (cdr l) (cadar l)))) ))

(define (pos-info->line-info!)
   (define (info l line mini maxi)
      (cond
	 ((null? l)
	  (if line
	      (cons (list line mini maxi) '())
	      '()))
	 ((not line)
	  (let ((pos (caddr (car l))))
	     (info l (cadr (car l)) pos pos)))
	 ((=fx line (cadr (car l)))
	  (let ((pos (caddr (car l))))
	     (info (cdr l) line (min mini pos) (max maxi pos))))
	 (else
	  (cons (list line mini maxi)
		(info l #f 0 0)))))
   (let ((x (sort **all-lines** (lambda (x y) (<=fx (cadr x) (cadr y))))))
      ;; compute the min char and the max char for every line info
      (set! **pos->line** (info x #f 0 0)) ))

(define (get-localvars l)
   (cond
      ((null? l) l)
      ((eq? (caar l) 205)
       (cons (cdar l) (get-localvars (cdr l))) )
      (else (get-localvars (cdr l))) ))
   

(define (get-bytecode l)
   (cond ((null? l) l)
	 ((eq? (caar l) 202) (get-bytecode (cdr l)))
	 ((eq? (caar l) 203) (get-bytecode (cdr l)))
	 ((eq? (caar l) 204) (get-bytecode (cdr l)))
	 ((eq? (caar l) 205) (get-bytecode (cdr l)))
	 (else (append (car l) (get-bytecode (cdr l)))) ))

