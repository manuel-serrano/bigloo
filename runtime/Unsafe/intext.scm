;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/intext.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano & Pierre Weis                      */
;*    Creation    :  Tue Jan 18 08:11:58 1994                          */
;*    Last change :  Sun Nov 28 18:16:38 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The serialization process does not make hypothesis on word's     */
;*    size. Since 2.8b, the serialization/deserialization is thread    */
;*    safe.                                                            */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Object Dumping@                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __intext
   
   (import  __error
	    __hash
	    __object
	    __thread
	    __r4_symbols_6_4
	    __bexit
	    __param)

   (use     __type
	    __bigloo
	    __structure
	    __tvector
            __weakptr
	    __bit
	    __dsssl
	    __ucs2
	    __unicode
	    __bignum
	    
	    __process
	    __custom
	    __date
	    __srfi4
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3

	    __r5_control_features_6_4
	    
	    __evenv)

   (extern  (macro cnst->integer::long (::obj) "CCNST")
	    (macro integer->cnst::obj (::long) "BCNST")
	    (macro pointer?::bool (::obj) "POINTERP")
	    (macro size-of-long::long "sizeof( long )")
	    
	    (export string->obj "string_to_obj")
	    (export obj->string "obj_to_string"))
   
   (java    (class foreign
	       (method static cnst->integer::long (::obj) "CCNST")
	       (method static integer->cnst::obj (::long) "BCNST")
	       (method static pointer?::bool (::obj) "POINTERP")
	       (field static size-of-long::long "SIZEOFLONG"))
	    
	    (export string->obj "string_to_obj")
	    (export obj->string "obj_to_string"))
   
   (export  (set-obj-string-mode! ::obj)
	    
	    (string->obj ::bstring)
	    (obj->string::bstring ::obj)
	    
	    (register-custom-serialization! ::bstring ::procedure ::procedure)
	    (get-custom-serialization ::bstring)
	    
	    (register-procedure-serialization! ::procedure ::procedure)
	    (get-procedure-serialization)
	    
	    (register-process-serialization! ::procedure ::procedure)
	    (get-procedure-serialization)
	    
	    (register-opaque-serialization! ::procedure ::procedure)
	    (get-opaque-serialization)
	    
	    (register-class-serialization! ::obj ::procedure ::procedure)
	    (get-class-serialization ::obj))
   
   (option  (set! *unsafe-type*   #t)
	    (set! *unsafe-arity*  #t)
	    (set! *unsafe-range*  #t)
	    (set! *unsafe-struct* #t))
   
   (pragma  (string->obj side-effect-free)
	    (obj->string side-effect-free)
	    (cnst->integer nesting)
	    (integer->cnst nesting)
	    (pointer? nesting)))

;*---------------------------------------------------------------------*/
;*    for  ....                                                        */
;*---------------------------------------------------------------------*/
(define-macro (for var min max . body)
   (let ((loop (gensym 'for)))
      `(let ,loop ((,var ,min))
	    (when (<fx ,var ,max)
	       ,@body
	       (,loop (+fx ,var 1))))))

;*---------------------------------------------------------------------*/
;*    Les variables de controle de `string->obj'                       */
;*---------------------------------------------------------------------*/
(define *epair?* #t)

;*---------------------------------------------------------------------*/
;*    set-obj-string-mode! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function controls whenever extended pair must be outputed   */
;*    as usual pair (loosing the cer slot) or as specific extended     */
;*    pairs.                                                           */
;*---------------------------------------------------------------------*/
(define (set-obj-string-mode! mode)
   (case mode
      ((epair)
       (set! *epair?* #t))
      ((pair)
       (set! *epair?* #f))))

;*---------------------------------------------------------------------*/
;*    @deffn string->obj@ ...                                          */
;*---------------------------------------------------------------------*/
(define (string->obj s)
   
   ;; *pointer*
   (define *pointer* 0)

   ;; *definitions*
   (define *definitions* '#())
   
   ;; *defining*
   (define *defining* #f)
   
   ;; read-integer
   (define (read-integer s)
      (read-size s))
   
   ;; read-float
   (define (read-float s)
      (let* ((sz (read-size s))
	     (res (string->real (substring s *pointer* (+fx *pointer* sz)))))
	 (set! *pointer* (+fx *pointer* sz))
	 res))
   
   ;; read-bignum
   (define (read-bignum s)
      (let* ((sz (read-size s))
	     (res (string->bignum (substring s *pointer* (+fx *pointer* sz)))))
	 (set! *pointer* (+fx *pointer* sz))
	 res))
   
   ;; read-char
   (define (read-char s)
      (integer->char (read-integer s)))

   ;; read-word
   (define (read-word::long s sz::int)
      (let ((acc::long 0))
	 (for i 0 sz
	      (let ((d (string-ref s *pointer*)))
		 (set! acc (+fx (*fx 256 acc) (char->integer d)))
		 (set! *pointer* (+fx *pointer* 1))))
	 acc))
   
   ;; read-long-word
   (define (read-long-word::long s sz::int)
      (let ((acc::llong 0))
	 (for i 0 sz
	      (let ((d (string-ref s *pointer*)))
		 (set! acc (+llong (*llong #l256 acc)
				   (fixnum->llong (char->integer d))))
		 (set! *pointer* (+fx *pointer* 1))))
	 acc))
   
   ;; read-size
   (define (read-size::long s)
      (let ((sz (char->integer (string-ref s *pointer*))))
	 (set! *pointer* (+fx *pointer* 1))
	 (read-word s sz)))
   
   ;; read-string
   (define (read-string s)
      (let* ((sz (read-size s))
	     (res (substring s *pointer* (+fx *pointer* sz))))
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* res)
	    (set! *defining* #f))
	 (set! *pointer* (+fx *pointer* sz))
	 res))

   ;; read-definition
   (define (read-definition)
      (set! *defining* (read-item))
      (read-item))

   ;; read-reference
   (define (read-reference)
      (vector-ref *definitions* (read-item)))

   ;; read-symbol
   (define (read-symbol)
      (string->symbol (read-item)))

   ;; read-keyword
   (define (read-keyword)
      (string->keyword (read-item)))

   ;; read-cnst
   (define (read-cnst)
      (integer->cnst (read-integer s)))

   ;; read-ucs2
   (define (read-ucs2)
      (integer->ucs2 (read-integer s)))

   ;; read-custom
   (define (read-custom)
      (let* ((str (read-string s))
	     (str2 (read-string s)) 
	     (unserializer (find-custom-unserializer str)))
	 (if (not (procedure? unserializer))
	     (error "string->obj" "Can't unserialize custom object" str)
	     (unserializer str2))))

   ;; read-elong
   (define (read-elong)
      (let* ((sz (read-size s))
	     (res (string->elong (substring s *pointer* (+fx *pointer* sz)))))
	 (set! *pointer* (+fx *pointer* sz))
	 res))

   ;; read-llong
   (define (read-llong)
      (let* ((sz (read-size s))
	     (res (string->llong (substring s *pointer* (+fx *pointer* sz)))))
	 (set! *pointer* (+fx *pointer* sz))
	 res))

   ;; read-vector
   (define (read-vector)
      (let* ((sz (read-size s))
 	     (res ($create-vector sz)))
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* res)
	    (set! *defining* #f))
	 (for i 0 sz (vector-set! res i (read-item)))
	 res))

   ;; read-hvector
   (define (read-hvector)
      (let ((len (read-size s))
	    (bsize (read-size s)))
	 (case (read-symbol)
	    ((s8)
	     (let ((res (make-s8vector len)))
		(for i 0 len (s8vector-set! res i (read-word s bsize)))
		res))
	    ((u8)
	     (let ((res (make-u8vector len)))
		(for i 0 len (u8vector-set! res i (read-word s bsize)))
		res))
	    ((s16)
	     (let ((res (make-s16vector len)))
		(for i 0 len (s16vector-set! res i (read-word s bsize)))
		res))
	    ((u16)
	     (let ((res (make-u16vector len)))
		(for i 0 len (u16vector-set! res i (read-word s bsize)))
		res))
	    ((s32)
	     (let ((res (make-s32vector len)))
		(for i 0 len (s32vector-set! res i (read-word s bsize)))
		res))
	    ((u32)
	     (let ((res (make-u32vector len)))
		(for i 0 len (u32vector-set! res i (read-word s bsize)))
		res))
	    ((s64)
	     (let ((res (make-s64vector len)))
		(for i 0 len (s64vector-set! res i (read-long-word s bsize)))
		res))
	    ((u64)
	     (let ((res (make-u64vector len)))
		(for i 0 len (u64vector-set! res i (read-long-word s bsize)))
		res))
	    ((f32)
	     (let ((res (make-f32vector len)))
		(for i 0 len (f32vector-set! res i (read-float s)))
		res))
	    ((f64)
	     (let ((res (make-f64vector len)))
		(for i 0 len (f64vector-set! res i (read-float s)))
		res)))))

   ;; read-tagged-vector
   (define (read-tagged-vector)
      (let* ((tag (read-item))
	     (sz (read-size s))
	     (res ($create-vector sz)))
	 (vector-tag-set! res tag)
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* res)
	    (set! *defining* #f))
	 (for i 0 sz (vector-set! res i (read-item)))
	 res))

   ;; read-typed-vector
   (define (read-typed-vector)
      (let* ((id (read-item))
	     (v (read-item))
	     (tv (vector->tvector id v)))
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* tv)
	    (set! *defining* #f))
	 tv))

   ;; read-list
   (define (read-list)
      (let* ((sz (read-size s))
	     (res (cons '() '())))
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* res)
	    (set! *defining* #f))
	 (let loop ((i 0)
		    (hd res))
	    (if (=fx i (-fx sz 2))
		(begin
		   (set-car! hd (read-item))
		   (set-cdr! hd (read-item)))
		(begin
		   (set-car! hd (read-item))
		   (set-cdr! hd (cons '() '()))
		   (loop (+fx i 1) (cdr hd)))))
	 res))

   ;; read-extended-list
   (define (read-extended-list)
      (let* ((sz (read-size s))
	     (res (econs '() '() #unspecified)))
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* res)
	    (set! *defining* #f))
	 (let loop ((i 0)
		    (hd res))
	    (if (=fx i (-fx sz 2))
		(begin
		   (set-car! hd (read-item))
		   (set-cer! hd (read-item))
		   (set-cdr! hd (read-item)))
		(begin
		   (set-car! hd (read-item))
		   (set-cer! hd (read-item))
		   (set-cdr! hd (econs '() '() #unspecified))
		   (loop (+fx i 1) (cdr hd)))))
	 res))

   ;; read-cell
   (define (read-cell)
      (let ((res (make-cell (unspecified))))
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* res)
	    (set! *defining* #f))
	 (cell-set! res (read-item))
	 res))

   ;; read-weakptr
   (define (read-weakptr)
      (let ((res (make-weakptr (unspecified))))
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* res)
	    (set! *defining* #f))
	 (weakptr-data-set! res (read-item))
	 res))

   ;; read-special
   (define (read-special s converter)
      ;; unserialize a process or an opaque
      (let* ((sz (read-size s))
	     (res (substring s *pointer* (+fx *pointer* sz))))
	 (when (fixnum? *defining*)
	    (vector-set! *definitions* *defining* res)
	    (set! *defining* #f))
	 (set! *pointer* (+fx *pointer* sz))
	 (converter res)))

   ;; read-structure
   (define (read-structure)
      (let* ((defining (let ((old *defining*))
			  (set! *defining* #f)
			  old))
	     (key (read-item))
	     (sz (read-size s))
	     (res (make-struct key sz #unspecified)))
	 (if (fixnum? defining)
	     (vector-set! *definitions* defining res))
	 (for i 0 sz (struct-set! res i (read-item)))
	 res))

   ;; read-object
   (define (read-object)
      (let* ((defining (let ((old *defining*))
			  (set! *defining* #f)
			  old))
	     (key (read-item))
	     (sz (read-size s))
	     (struct (make-struct key sz (unspecified)))
	     (object (allocate-instance key)))
	 (if (fixnum? defining)
	     (vector-set! *definitions* defining object))
	 (for i 0 sz (struct-set! struct i (read-item)))
	 (let ((hash (read-item)))
	    (if (=fx hash (class-hash (object-class object)))
		(struct+object->object object struct)
		(error "string->obj" "corrupted class" key)))))

   ;; read-custom-object
   (define (read-custom-object)
      (let* ((defining (let ((old *defining*))
			  (set! *defining* #f)
			  old))
	     (obj (read-item))
	     (hash (read-item))
	     (unserializer (find-class-unserializer hash))
	     (object (unserializer obj)))
	 (if (fixnum? defining)
	     (vector-set! *definitions* defining object))
	 (if (=fx hash (class-hash (object-class object)))
	     object
	     (error "string->obj" "corrupted custom" object))))
   
   ;; read-procedure
   (define (read-procedure s)
      (let* ((arity (read-item))
	     (entry (read-item))
	     (len (read-size s))
	     (p ($make-procedure ($string->procedure-entry entry) arity len)))
	 (for i 0 len (procedure-set! p i (read-item)))
	 (procedure-attr-set! p (read-item))
	 p))

   ;; read-item
   (define (read-item)
      (let ((d (string-ref s *pointer*)))
	 (set! *pointer* (+fx *pointer* 1))
	 (case d
	    ((#\=) (read-definition))
	    ((#\#) (read-reference))
	    ((#\') (read-symbol))
	    ((#\:) (read-keyword))
	    ((#\a) (read-char s))
	    ((#\u) (read-ucs2))
	    ((#\F) #f)
	    ((#\T) #t)
	    ((#\;) #unspecified)
	    ((#\.) '())
	    ((#\<) (read-cnst))
	    ((#\") (read-string s))
	    ((#\U) (utf8-string->ucs2-string (read-string s)))
	    ((#\[) (read-vector))
	    ((#\t) (read-tagged-vector))
	    ((#\V) (read-typed-vector))
	    ((#\h) (read-hvector))
	    ((#\() (read-list))
	    ((#\^) (read-extended-list))
	    ((#\{) (read-structure))
	    ((#\|) (read-object))
	    ((#\O) (read-custom-object))
	    ((#\f) (read-float s))
	    ((#\z) (read-bignum s))
	    ((#\-) (negfx (read-integer s)))
	    ((#\!) (read-cell))
            ((#\w) (read-weakptr))
	    ((#\+) (read-custom))
	    ((#\E) (read-elong))
	    ((#\L) (read-llong))
	    ((#\p) (read-special s *string->process*))
	    ((#\e) (read-special s *string->process*))
	    ((#\o) (read-special s *string->opaque*))
	    ((#\d) (seconds->date (string->elong (read-string s))))
	    (else  (set! *pointer* (-fx *pointer* 1)) (read-integer s)))))

   (let ((d (string-ref s *pointer*)))
      (when (char=? d #\c)
	 (set! *pointer* (+fx *pointer* 1))
	 (set! *definitions* (make-vector (read-size s)))))
   
   (read-item))

;*---------------------------------------------------------------------*/
;*    @deffn obj->string@ ...                                          */
;*---------------------------------------------------------------------*/
(define (obj->string obj)
   (let* ((table (make-hashtable #unspecified
				 #unspecified
				 (lambda (a b)
				    (cond
				       ((string? a)
					(and (string? b) (string=? a b)))
				       ((ucs2-string? a)
					(equal? a b))
				       (else
					(eq? a b))))))
	  (nbref (mark-obj! table obj)))
      (print-obj table nbref obj)))

;*---------------------------------------------------------------------*/
;*    mark ...                                                         */
;*---------------------------------------------------------------------*/
(define-struct mark obj value ref-count ref)

;*---------------------------------------------------------------------*/
;*    mark-defined? ...                                                */
;*---------------------------------------------------------------------*/
(define (mark-defined? mark)
   (>=fx (mark-ref mark) 0))

;*---------------------------------------------------------------------*/
;*    put-mark! ...                                                    */
;*---------------------------------------------------------------------*/
(define (put-mark! table obj val)
   (hashtable-put! table obj (mark obj val 0 -1)))

;*---------------------------------------------------------------------*/
;*    get-mark ...                                                     */
;*---------------------------------------------------------------------*/
(define (get-mark table obj)
   (hashtable-get table obj))

;*---------------------------------------------------------------------*/
;*    objcustom ...                                                    */
;*    -------------------------------------------------------------    */
;*    This structure is used for representing custom serlialization    */
;*    of objects.                                                      */
;*---------------------------------------------------------------------*/
(define-struct objcustom val)

;*---------------------------------------------------------------------*/
;*    *max-size-word* ...                                              */
;*---------------------------------------------------------------------*/
(define *max-size-word* size-of-long)

;*---------------------------------------------------------------------*/
;*    size-of-word ...                                                 */
;*---------------------------------------------------------------------*/
(define (size-of-word m)
   (let loop ((size 0)
	      (m m))
      (if (=fx m 0)
	  size
	  (loop (+fx size 1) (bit-rsh m 8)))))

;*---------------------------------------------------------------------*/
;*    print-obj ...                                                    */
;*---------------------------------------------------------------------*/
(define (print-obj table nbref obj)
   
   (define buffer ($make-string/wo-fill 100))
   (define ptr 0)
   (define ref 0)

   ;; check-buffer
   (define (check-buffer! size)
      (let ((l (+fx ptr (+fx size *max-size-word*)))
	    (bl (string-length buffer)))
	 (when (>=fx l bl)
	    (let ((nbuf (make-string (*fx 2 (+fx l 100)))))
	       (blit-string-ur! buffer 0 nbuf 0 bl)
	       (set! buffer nbuf)))))
   
   ;; !print-markup
   (define (!print-markup c)
      (check-buffer! 1)
      (string-set! buffer ptr c)
      (set! ptr (+fx 1 ptr)))
   
   ;; print-chars
   (define (!print-chars s size)
      (print-word size)
      (check-buffer! size)
      (blit-string! s 0 buffer ptr size)
      (set! ptr (+fx ptr size)))
   
   ;; print-int-as-char
   (define (print-int-as-char c)
      (!print-markup (integer->char c)))
   
   ;; print-word/size
   (define (print-word/size m size)
      (let loop ((i (-fx size 1)))
	 (when (>=fx i 0)
	    (let ((d (bit-and (bit-rsh m (*fx 8 i)) #xff)))
	       (print-int-as-char d)
	       (loop (-fx i 1))))))
   
   ;; print-long-word/size
   (define (print-long-word/size m::llong size)
      (let loop ((i (-fx size 1)))
	 (when (>=fx i 0)
	    (let ((d (llong->fixnum
		      (bit-andllong (bit-rshllong m (*fx 8 i)) #l255))))
	       (print-int-as-char d)
	       (loop (-fx i 1))))))
   
   ;; print-word
   (define (print-word m)
      (let ((size (size-of-word m)))
	 (if (=fx size 0)
	     (print-int-as-char 0)
	     (begin
		(print-int-as-char size)
		(print-word/size m size)))))
   
   ;; print-fixnum
   (define (print-fixnum i::long)
      (if (<fx i 0)
	  (begin
	     (!print-markup #\-)
	     (print-word (negfx i)))
	  (print-word i)))
   
   ;; print-string
   (define (print-string markup item)
      (!print-markup markup)
      (!print-chars item (string-length item)))
   
   ;; print-composite
   (define (print-composite item printer)
      (let ((mark (get-mark table item)))
	 (cond
	    ((mark-defined? mark)
	     (!print-markup #\#)
	     (print-fixnum (mark-ref mark)))
	    ((=fx (mark-ref-count mark) 0)
	     (printer item mark))
	    (else
	     (mark-ref-set! mark ref)
	     (!print-markup #\=)
	     (print-fixnum ref)
	     (set! ref (+fx 1 ref))
	     (printer item mark)))))

   ;; print-epair
   (define (print-epair item mark)
      (!print-markup #\^)
      (let ((len (marked-pair-length table item)))
	 (print-word len)
	 (let loop ((i 0)
		    (p item))
	    (cond
	       ((=fx i (-fx len 1))
		(if (pair? p)
		    (begin
		       (print-item (car p))
		       (if (epair? p)
			   (print-item (cer p))
			   (print-item #unspecified))
		       (print-item '()))
		    (print-item p)))
	       (else
		(print-item (car p))
		(if (epair? p)
		    (print-item (cer p))
		    (print-item #unspecified))
		(let ((vcdr (cdr p)))
		   (if (and (pair? vcdr)
			    (let ((mark (get-mark table vcdr)))
			       (or (> (mark-ref-count mark) 0)
				   (mark-defined? mark))))
		       (print-item vcdr)
		       (loop (+fx i 1) vcdr))))))))
   
   ;; print-pair
   (define (print-pair item mark)
      (!print-markup #\()
      (let ((len (marked-pair-length table item)))
	 (print-word len)
	 (let loop ((i 0)
		    (p item))
	    (cond
	       ((=fx i (-fx len 1))
		(if (pair? p)
		    (begin
		       (print-item (car p))
		       (print-item '()))
		    (print-item p)))
	       (else
		(print-item (car p))
		(let ((vcdr (cdr p)))
		   (if (and (pair? vcdr)
			    (let ((mark (get-mark table vcdr)))
			       (or (>fx (mark-ref-count mark) 0)
				   (mark-defined? mark))))
		       (print-item vcdr)
		       (loop (+fx i 1) vcdr))))))))
   
   
   ;; print-object
   (define (print-object item mark)
      (let ((v (mark-value mark)))
	 (if (objcustom? v)
	     (print-object-custom item v)
	     (print-object-struct item v))))

   ;; print-object-custom
   (define (print-object-custom item o)
      (!print-markup #\O)
      (print-item (objcustom-val o))
      (print-item (class-hash (object-class item))))
   
   ;; print-object-struct
   (define (print-object-struct item o)
      (print-struct #\| o)
      (print-item (class-hash (object-class item))))
   
   ;; print-struct
   (define (print-struct markup item)
      (!print-markup markup)
      (print-item (struct-key item))
      (let ((len (struct-length item)))
	 (print-word len)
	 (for i 0 len (print-item (struct-ref item i)))))
   
   ;; print-cell
   (define (print-cell item mark)
      (!print-markup #\!)
      (print-item (cell-ref item)))
   
   ;; print-weakptr
   (define (print-weakptr o mark)
      (!print-markup #\w)
      (print-item (weakptr-data o)))

   ;; print-vector
   (define (print-vector item mark)
      (let ((tag (vector-tag item))
	    (len (vector-length item)))
	 (if (>fx tag 0)
	     (begin
		(!print-markup #\t)
		(print-fixnum tag))
	     (!print-markup #\[))
	 (print-word len)
	 (for i 0 len (print-item (vector-ref item i)))))
   
   ;; print-hvector
   (define (print-hvector item mark)
      (multiple-value-bind (tag bsize ref _)
	 (homogeneous-vector-info item)
	 (let ((len ($hvector-length item)))
	    (!print-markup #\h)
	    (print-word len)
	    (print-word bsize)
	    (print-string #\" (symbol->string! tag))
	    (case tag
	       ((s8 u8 s16 u16 s32 u32)
		(for i 0 len (print-word/size (ref item i) bsize)))
	       ((s64 u64)
		(for i 0 len (print-long-word/size (ref item i) bsize)))
	       ((f32 f64)
		(for i 0 len
		     (let ((s (real->string (ref item i))))
			(!print-chars s (string-length s)))))))))
   
   ;; print-tvector
   (define (print-tvector item mark)
      (let ((v (mark-value item)))
	 (!print-markup #\V)
	 (print-item (tvector-id item))
	 (print-item v)))
   
   ;; print-custom
   (define (print-custom item mark)
      (!print-markup #\+)
      (let* ((ident (custom-identifier item)))
	 (!print-chars ident (string-length ident))
	 (let ((s (mark-value mark)))
	    (!print-chars s (string-length s)))))

   ;; print-special
   (define (print-special markup item mark)
      (print-string markup (mark-value mark)))

   ;; print-item
   (define (print-item item)
      (cond
	 ((and (epair? item) *epair?*)
	  (print-composite item print-epair))
	 ((pair? item)
	  (print-composite item print-pair))
	 ((symbol? item)
	  (!print-markup #\')
	  (print-item (symbol->string! item)))
	 ((keyword? item)
	  (!print-markup #\:)
	  (print-item (keyword->string item)))
	 ((string? item)
	  (print-composite item (lambda (item mark) (print-string #\" item))))
	 ((object? item)
	  (print-composite item print-object))
	 ((struct? item)
	  (print-composite item (lambda (item mark) (print-struct #\{ item))))
	 ((char? item)
	  (!print-markup #\a)
	  (print-fixnum (char->integer item)))
	 ((ucs2? item)
	  (!print-markup #\u)
	  (print-fixnum (ucs2->integer item)))
	 ((eq? item #unspecified)
	  (!print-markup #\;))
	 ((eq? item '())
	  (!print-markup #\.))
	 ((eq? item #t)
	  (!print-markup #\T))
	 ((eq? item #f)
	  (!print-markup #\F))
	 ((cnst? item)
	  (!print-markup #\<)
	  (print-fixnum (cnst->integer item)))
	 ((fixnum? item)
	  (print-fixnum item))
	 ((real? item)
	  (!print-markup #\f)
	  (let ((s (real->string item)))
	     (!print-chars s (string-length s))))
	 ((ucs2-string? item)
	  (print-composite item (lambda (item mark)
				  (print-string #\U (mark-value mark)))))
	 ((cell? item)
	  (print-composite item print-cell))
         ((weakptr? item)
          (print-composite item print-weakptr))
	 ((vector? item)
	  (print-composite item print-vector))
	 ((homogeneous-vector? item)
	  (print-composite item print-hvector))
	 ((tvector? item)
	  (print-composite item print-tvector))
	 ((elong? item)
	  (!print-markup #\E)
	  (let ((s (elong->string item)))
	     (!print-chars s (string-length s))))
	 ((llong? item)
	  (!print-markup #\L)
	  (let ((s (llong->string item)))
	     (!print-chars s (string-length s))))
	 ((date? item)
	  (!print-markup #\d)
	  (let ((s (elong->string (date->seconds item))))
	     (!print-chars s (string-length s))))
	 ((bignum? item)
	  (!print-markup #\z)
	  (let ((s (bignum->string item)))
	     (!print-chars s (string-length s))))
	 ((custom? item)
	  (print-composite item print-custom))
	 ((procedure? item)
	  (print-composite item (lambda (i m) (print-special #\p i m))))
	 ((process? item)
	  (print-composite item (lambda (i m) (print-special #\e i m))))
	 ((opaque? item)
	  (print-composite item (lambda (i m) (print-special #\o i m))))
	 (else
	  (error "obj->string" "Unknown object" item))))

   (when (>fx nbref 0)
      (!print-markup #\c)
      (print-fixnum nbref))

   (print-item obj)
   
   (string-shrink! buffer ptr))
	    
;*---------------------------------------------------------------------*/
;*    mark-obj! ...                                                    */
;*---------------------------------------------------------------------*/
(define (mark-obj! table obj)
   
   ;; nbref
   (define nbref 0)
   
   ;; incr-mark!
   (define (incr-mark! m)
      (let ((om (mark-ref-count m)))
	 (mark-ref-count-set! m (+fx 1 om))
	 (when (=fx om 0) (set! nbref (+fx nbref 1)))))
   
   ;; mark-composite
   (define (mark-composite obj marker)
      (let ((m (get-mark table obj)))
	 (if (mark? m)
	     (incr-mark! m)
	     (marker obj))))

   ;; mark-epair
   (define (mark-epair obj)
      (put-mark! table obj #f)
      (mark (car obj))
      (mark (cdr obj))
      (mark (cer obj)))
   
   ;; mark-pair
   (define (mark-pair obj)
      (put-mark! table obj #f)
      (mark (car obj))
      (mark (cdr obj)))
   
   ;; mark-object
   (define (mark-object obj)
      (mark-class-instance obj (object-serializer obj)))

   ;; mark-class-instance
   (define (mark-class-instance obj custom)
      (put-mark! table obj custom)
      (mark (class-hash (object-class obj)))
      (mark custom)
      custom)
   
   ;; mark-struct
   (define (mark-struct obj)
      (put-mark! table obj #f)
      (let ((key (struct-key obj))
	    (len (struct-length obj)))
	 (mark key)
	 (for i 0 len (mark (struct-ref obj i)))))
   
   ;; mark-cell
   (define (mark-cell obj)
      (put-mark! table obj #f)
      (mark (cell-ref obj)))
   
   ;; mark-weakptr
   (define (mark-weakptr obj)
      (put-mark! table obj #f)
      (mark (weakptr-data obj)))

   ;; mark-vector
   (define (mark-vector obj)
      (put-mark! table obj #f)
      (let ((len (vector-length obj)))
	 (for i 0 len (mark (vector-ref obj i)))))
   
   ;; mark-tvector
   (define (mark-tvector obj)
      (let ((v (tvector->vector obj)))
	 (put-mark! table obj v)
	 (mark (tvector-id obj))
	 (mark v)))

   ;; mark-hvector
   (define (mark-hvector obj)
      (put-mark! table obj #f))

   ;; mark-string
   (define (mark-string obj)
      (put-mark! table obj #f))

   ;; mark-ucs2string
   (define (mark-ucs2string obj)
      (let ((str (ucs2-string->utf8-string obj)))
	 (put-mark! table obj str)
	 (mark str)))
   
   ;; mark-custom
   (define (mark-custom obj)
      (let ((v ((find-custom-serializer (custom-identifier obj)) obj)))
	 (put-mark! table obj v)
	 (mark v)))

   ;; mark-procedure
   (define (mark-procedure obj)
      (*procedure->string* obj))
   
   ;; mark-process
   (define (mark-process obj)
      (*process->string* obj))
   
   ;; mark-opaque
   (define (mark-opaque obj)
      (*opaque->string* obj))

   ;; mark
   (define (mark obj)
      (cond
	 ((and (epair? obj) *epair?*)
	  (mark-composite obj mark-epair))
	 ((pair? obj)
	  (mark-composite obj mark-pair))
	 ((object? obj)
	  (mark-composite obj mark-object))
 	 ((struct? obj)
	  (mark-composite obj mark-struct))
	 ((cell? obj)
	  (mark-composite obj mark-cell))
	 ((weakptr? obj)
	  (mark-composite obj mark-weakptr))
	 ((symbol? obj)
	  (mark (symbol->string obj)))
	 ((keyword? obj)
	  (mark (keyword->string obj)))
	 ((string? obj)
	  (mark-composite obj mark-string))
	 ((ucs2-string? obj)
	  (mark-composite obj mark-ucs2string))
	 ((vector? obj)
	  (mark-composite obj mark-vector))
	 ((homogeneous-vector? obj)
	  (mark-composite obj mark-hvector))
	 ((tvector? obj)
	  (mark-composite obj mark-tvector))
	 ((procedure? obj)
	  (mark-composite obj mark-procedure))
	 ((custom? obj)
	  (mark-composite obj mark-custom))
	 ((process? obj)
	  (mark-composite obj mark-process))
	 ((opaque? obj)
	  (mark-composite obj mark-opaque))
	 ((pointer? obj)
	  (mark-composite obj (lambda (obj) #f)))))

   (mark obj)

   nbref)

;*---------------------------------------------------------------------*/
;*    marked-pair-length ...                                           */
;*---------------------------------------------------------------------*/
(define (marked-pair-length table l)
   (let loop ((l l)
	      (r 1))
      (let ((vcdr (cdr l)))
	 (if (pair? vcdr)
	     (let ((mark (get-mark table vcdr)))
		(if (or (>fx (mark-ref-count mark) 0)
			(mark-defined? mark))
		    (+fx r 1)
		    (loop vcdr (+fx r 1))))
	     (+fx r 1)))))

;*---------------------------------------------------------------------*/
;*    *custom-serialization* ...                                       */
;*---------------------------------------------------------------------*/
(define *custom-serialization* '())

;*---------------------------------------------------------------------*/
;*    register-custom-serialization! ...                               */
;*---------------------------------------------------------------------*/
(define (register-custom-serialization! ident serializer unserializer)
   (let ((cell (assoc ident *custom-serialization*)))
      (if (not (pair? cell))
	  (set! *custom-serialization*
		(cons (list ident serializer unserializer)
		      *custom-serialization*)))))

;*---------------------------------------------------------------------*/
;*    find-custom-serializer ...                                       */
;*---------------------------------------------------------------------*/
(define (find-custom-serializer ident)
   (let ((cell (assoc ident *custom-serialization*)))
      (if (pair? cell)
	  (cadr cell)
	  (error 'obj->string "Cannot find custom serializer" ident))))
   
;*---------------------------------------------------------------------*/
;*    find-custom-unserializer ...                                     */
;*---------------------------------------------------------------------*/
(define (find-custom-unserializer ident)
   (let ((cell (assoc ident *custom-serialization*)))
      (if (pair? cell)
	  (caddr cell)
	  (error 'string->obj "Cannot find custom unserializer" ident))))

;*---------------------------------------------------------------------*/
;*    get-custom-serialization ...                                     */
;*---------------------------------------------------------------------*/
(define (get-custom-serialization ident)
   (let ((cell (assoc ident *custom-serialization*)))
      (if (pair? cell)
	  (values (cadr cell) (caddr cell))
	  (values #f #f))))
   
;*---------------------------------------------------------------------*/
;*    *procedure->string* ...                                          */
;*---------------------------------------------------------------------*/
(define *procedure->string*
   (lambda (item)
      (error "obj->string" "can't extern procedure" item)))

;*---------------------------------------------------------------------*/
;*    *string->procedure* ...                                          */
;*---------------------------------------------------------------------*/
(define *string->procedure*
   (lambda (string)
      (error "string->obj" "Can't intern procedure item" string)))

;*---------------------------------------------------------------------*/
;*    @deffn register-procedure-serialization!@ ...                    */
;*---------------------------------------------------------------------*/
(define (register-procedure-serialization! serializer unserializer)
   (set! *procedure->string* serializer)
   (set! *string->procedure* unserializer))
   
;*---------------------------------------------------------------------*/
;*    @deffn get-procedure-serialization@ ...                          */
;*---------------------------------------------------------------------*/
(define (get-procedure-serialization)
   (cons *procedure->string* *string->procedure*))
   
;*---------------------------------------------------------------------*/
;*    *process->string* ...                                            */
;*---------------------------------------------------------------------*/
(define *process->string*
   (lambda (item)
      (error "obj->string" "can't extern process" item)))

;*---------------------------------------------------------------------*/
;*    *string->process* ...                                            */
;*---------------------------------------------------------------------*/
(define *string->process*
   (lambda (string)
      (error "string->obj" "Can't intern process item" string)))

;*---------------------------------------------------------------------*/
;*    @deffn register-process-serialization!@ ...                      */
;*---------------------------------------------------------------------*/
(define (register-process-serialization! serializer unserializer)
   (set! *process->string* serializer)
   (set! *string->process* unserializer))
   
;*---------------------------------------------------------------------*/
;*    @deffn get-process-serialization@ ...                            */
;*---------------------------------------------------------------------*/
(define (get-process-serialization)
   (values *process->string* *string->process*))

;*---------------------------------------------------------------------*/
;*    *opaque->string* ...                                             */
;*---------------------------------------------------------------------*/
(define *opaque->string*
   (lambda (item)
      (error "obj->string" "can't extern opaque" item)))

;*---------------------------------------------------------------------*/
;*    *string->opaque* ...                                             */
;*---------------------------------------------------------------------*/
(define *string->opaque*
   (lambda (string)
      (error "string->obj" "Can't intern opaque item" string)))

;*---------------------------------------------------------------------*/
;*    @deffn register-opaque-serialization!@ ...                       */
;*---------------------------------------------------------------------*/
(define (register-opaque-serialization! serializer unserializer)
   (set! *opaque->string* serializer)
   (set! *string->opaque* unserializer))
   
;*---------------------------------------------------------------------*/
;*    @deffn get-opaque-serialization@ ...                             */
;*---------------------------------------------------------------------*/
(define (get-opaque-serialization)
   (values *opaque->string* *string->opaque*))

;*---------------------------------------------------------------------*/
;*    object-serializer ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (object-serializer obj::object)
   (object->struct obj))

;*---------------------------------------------------------------------*/
;*    *class-serialization* ...                                        */
;*---------------------------------------------------------------------*/
(define *class-serialization* '())

;*---------------------------------------------------------------------*/
;*    register-class-serialization! ...                                */
;*---------------------------------------------------------------------*/
(define (register-class-serialization! class serializer unserializer)
   (generic-add-method! object-serializer
			class
			(lambda (o)
			   (objcustom (serializer o)))
			(string-append (symbol->string! (class-name class))
				       "-serializer"))
   (let* ((hash (class-hash class))
	  (cell (assq hash *class-serialization*)))
      (if (not (pair? cell))
	  (set! *class-serialization*
		(cons (list hash serializer unserializer)
		      *class-serialization*)))))

;*---------------------------------------------------------------------*/
;*    find-class-unserializer ...                                      */
;*---------------------------------------------------------------------*/
(define (find-class-unserializer hash)
   (let ((cell (assv hash *class-serialization*)))
      (if (pair? cell)
	  (caddr cell)
	  (error 'string->obj "Cannot find class unserializer" hash))))

;*---------------------------------------------------------------------*/
;*    get-class-serialization ...                                      */
;*---------------------------------------------------------------------*/
(define (get-class-serialization class)
   (let* ((hash (class-hash class))
	  (cell (assq hash *class-serialization*)))
      (if (pair? cell)
	  (values (cadr cell) (caddr cell))
	  (values #f #f))))
