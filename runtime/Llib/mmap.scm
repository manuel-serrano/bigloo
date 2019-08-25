;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/mmap.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 10 10:40:10 2005                          */
;*    Last change :  Sun Aug 25 09:10:30 2019 (serrano)                */
;*    Copyright   :  2005-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Map IO                                                           */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/io.texi@                                  */
;*       @node Input And Output@                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mmap
   
   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    __object
	    __thread
	    __bit
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
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
	    __evenv)
   
   (extern  (macro $mmap?::bool (::obj) "BGL_MMAPP")
	    (macro $mmap-open::mmap (::bstring ::bool ::bool) "bgl_open_mmap")
	    (macro $string->mmap::mmap (::bstring ::bool ::bool) "bgl_string_to_mmap")
	    (macro $mmap->string::string (::mmap) "BGL_MMAP_TO_STRING")
	    (macro $mmap-close::obj (::mmap) "bgl_close_mmap")
	    (macro $mmap-length::elong (::mmap) "BGL_MMAP_LENGTH")
	    (macro $mmap-name::obj (::mmap) "BGL_MMAP_NAME")
	    (macro $mmap-ref::uchar (::mmap ::elong) "BGL_MMAP_REF")
 	    (macro $mmap-set!::obj (::mmap ::elong ::uchar) "BGL_MMAP_SET")
	    (macro $mmap-rp::elong (::mmap) "BGL_MMAP_RP_GET")
	    (macro $mmap-rp-set!::void (::mmap ::elong) "BGL_MMAP_RP_SET")
	    (macro $mmap-wp::elong (::mmap) "BGL_MMAP_WP_GET")
	    (macro $mmap-wp-set!::void (::mmap ::elong) "BGL_MMAP_WP_SET")
	    (macro $mmap-bound-check?::bool (::elong ::elong) "BOUND_CHECK"))
   
   (java    (class foreign
	       (method static $mmap?::bool (::obj)
		  "BGL_MMAPP")
	       (method static $mmap-open::mmap (::bstring ::bool ::bool)
		  "bgl_open_mmap")
	       (method static $string->mmap::mmap (::bstring ::bool ::bool)
		  "bgl_string_to_mmap")
	       (method static $mmap->string::string (::mmap)
		  "bgl_mmap_to_string")
	       (method static $mmap-close::obj (::mmap)
		  "bgl_close_mmap")
	       (method static $mmap-length::elong (::mmap)
		  "BGL_MMAP_LENGTH")
	       (method static $mmap-name::obj (::mmap)
		  "BGL_MMAP_NAME")
	       (method static $mmap-ref::uchar (::mmap ::elong)
		  "BGL_MMAP_REF")
	       (method static $mmap-set!::obj (::mmap ::elong ::uchar)
		  "BGL_MMAP_SET")
	       (method static $mmap-rp::elong (::mmap)
		  "BGL_MMAP_RP_GET")
	       (method static $mmap-rp-set!::void (::mmap ::elong)
		  "BGL_MMAP_RP_SET")
	       (method static $mmap-wp::elong (::mmap)
		  "BGL_MMAP_WP_GET")
	       (method static $mmap-wp-set!::void (::mmap ::elong)
		  "BGL_MMAP_WP_SET")	
	       (method static $mmap-bound-check?::bool (::elong ::elong)
		  "BOUND_CHECK")))
   
   (export  (inline mmap?::bool ::obj)
	    (open-mmap::mmap ::bstring #!key (read #t) (write #t))
	    (string->mmap::mmap ::bstring #!key (read #t) (write #t))
	    (inline mmap-name::bstring ::mmap)
	    (inline mmap->string::string ::mmap)
            (inline mmap->bstring::bstring ::mmap)
	    (inline close-mmap ::mmap)
	    (inline mmap-length::elong ::mmap)
	    (inline mmap-read-position::elong ::mmap)
	    (inline mmap-read-position-set!::elong ::mmap ::elong)
	    (inline mmap-write-position::elong ::mmap)
	    (inline mmap-write-position-set!::elong ::mmap ::elong)
	    (inline mmap-ref-ur::uchar ::mmap ::elong)
	    (inline mmap-set-ur!::obj ::mmap ::elong ::uchar)
	    (inline mmap-ref::uchar ::mmap ::elong)
	    (inline mmap-set!::obj ::mmap ::elong ::uchar)
	    (mmap-substring::bstring ::mmap ::elong ::elong)
	    (mmap-substring-set!::obj ::mmap ::elong ::bstring)
	    (inline mmap-get-char::uchar ::mmap)
	    (inline mmap-put-char! ::mmap ::uchar)
	    (inline mmap-get-string::bstring ::mmap ::elong)
	    (inline mmap-put-string! ::mmap ::bstring)))
 
;*---------------------------------------------------------------------*/
;*    mmap? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (mmap? obj)
   ($mmap? obj))
 
;*---------------------------------------------------------------------*/
;*    open-mmap ...                                                    */
;*---------------------------------------------------------------------*/
(define (open-mmap name::bstring #!key (read #t) (write #t))
   ($mmap-open name read write))

;*---------------------------------------------------------------------*/
;*    string->mmap ...                                                 */
;*---------------------------------------------------------------------*/
(define (string->mmap s::bstring #!key (read #t) (write #t))
   ($string->mmap s read write))

;*---------------------------------------------------------------------*/
;*    mmap-name ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (mmap-name mmap::mmap)
   ($mmap-name mmap))

;*---------------------------------------------------------------------*/
;*    mmap->string ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (mmap->string::string mmap::mmap)
   ($mmap->string mmap))

;*---------------------------------------------------------------------*/
;*    mmap->bstring ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (mmap->bstring::bstring mmap::mmap)
   (let ((len::int  (elong->fixnum ($mmap-length mmap))))
      ($string->bstring-len ($mmap->string mmap) len)))

;*---------------------------------------------------------------------*/
;*    close-mmap ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (close-mmap mmap::mmap)
   ($mmap-close mmap))

;*---------------------------------------------------------------------*/
;*    mmap-length ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (mmap-length obj::mmap)
   ($mmap-length obj))

;*---------------------------------------------------------------------*/
;*    mmap-read-position ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (mmap-read-position mm::mmap)
   ($mmap-rp mm))

;*---------------------------------------------------------------------*/
;*    mmap-read-position-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (mmap-read-position-set! mm::mmap p)
   ($mmap-rp-set! mm p)
   p)

;*---------------------------------------------------------------------*/
;*    mmap-write-position ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (mmap-write-position mm::mmap)
   ($mmap-wp mm))

;*---------------------------------------------------------------------*/
;*    mmap-write-position-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (mmap-write-position-set! mm::mmap p)
   ($mmap-wp-set! mm p)
   p)

;*---------------------------------------------------------------------*/
;*    mmap-ref-ur ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (mmap-ref-ur::uchar mm::mmap i::elong)
   (let ((c ($mmap-ref mm i)))
      (mmap-read-position-set! mm (+elong i #e1))
      c))

;*---------------------------------------------------------------------*/
;*    mmap-set-ur! ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (mmap-set-ur!::obj mm::mmap i::elong c::uchar)
   ($mmap-set! mm i c)
   (mmap-write-position-set! mm (+elong i #e1)))

;*---------------------------------------------------------------------*/
;*    mmap-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (mmap-ref::uchar mm::mmap i::elong)
   (if ($mmap-bound-check? i (mmap-length mm))
       (mmap-ref-ur mm i)
       (error 'mmap-ref
	      (string-append "index out of range [0.."
			     (number->string (- (mmap-length mm) 1))
			     "]")
	      i)))

;*---------------------------------------------------------------------*/
;*    mmap-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (mmap-set!::obj mm::mmap i::elong c::uchar)
   (if ($mmap-bound-check? i (mmap-length mm))
       (mmap-set-ur! mm i c)
       (error 'mmap-set!
	      (string-append "index out of range [0.."
			     (number->string (- (mmap-length mm) 1))
			     "]")
	      i)))

;*---------------------------------------------------------------------*/
;*    mmap-substring ...                                               */
;*---------------------------------------------------------------------*/
(define (mmap-substring::bstring mm::mmap start end)
   (cond
      ((<elong end start)
       (error 'mmap-substring "length too small" (-elong end start)))
      ((not ($mmap-bound-check? end (+elong #e1 (mmap-length mm))))
       (error 'mmap-substring
	      (string-append "start+length bigger than "
			     (number->string (mmap-length mm)))
	      end))
      ((not ($mmap-bound-check? start (mmap-length mm)))
       (error 'mmap-substring "Illegal index" start))
      (else
       (let ((r ($make-string/wo-fill (elong->fixnum (-elong end start)))))
	  (let loop ((i start)
		     (j 0))
	     (if (=elong i end)
		 (begin
		    (mmap-read-position-set! mm i)
		    r)
		 (begin
		    (string-set-ur! r j (mmap-ref-ur mm i))
		    (loop (+elong i 1) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    mmap-substring-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (mmap-substring-set! mm::mmap o::elong s::bstring)
   (let ((len (string-length s)))
      (cond
	 ((<elong o #e0)
	  (error "mmap-substring-set!" "Illegal index" o))
	 ((not ($mmap-bound-check? o (+elong (mmap-length mm) #e1)))
	  (error 'mmap-substring-set!
		 (string-append "index out of range [0.."
				(number->string (mmap-length mm))
				"[")
		 o))
	 ((not ($mmap-bound-check? (+elong o (fixnum->elong len))
				   (+elong (mmap-length mm) #e1)))
	  (error 'mmap-sbustring-set!
		 (string-append "index out of range [0.."
				(number->string (mmap-length mm) 1)
				"]")
		 (+ o len)))
	 (else
	  (let loop ((i 0)
		     (j o))
	     (if (=fx i len)
		 (begin
		    (mmap-write-position-set! mm j)
		    mm)
		 (begin
		    (mmap-set-ur! mm j (string-ref-ur s i))
		    (loop (+fx i 1) (+elong j 1)))))))))

;*---------------------------------------------------------------------*/
;*    mmap-get-char ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (mmap-get-char mm::mmap)
   (mmap-ref mm ($mmap-rp mm)))

;*---------------------------------------------------------------------*/
;*    mmap-put-char! ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (mmap-put-char! mm::mmap c)
   (mmap-set! mm ($mmap-wp mm) c))

;*---------------------------------------------------------------------*/
;*    mmap-get-string ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (mmap-get-string mm::mmap len)
   (mmap-substring mm ($mmap-rp mm) (+elong ($mmap-rp mm) len)))

;*---------------------------------------------------------------------*/
;*    mmap-put-string! ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (mmap-put-string! mm::mmap s)
   (mmap-substring-set! mm ($mmap-wp mm) s))
