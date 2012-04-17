;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/crc.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  3 12:03:10 2009                          */
;*    Last change :  Tue Apr 17 07:53:43 2012 (serrano)                */
;*    Copyright   :  2009-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    CRC                                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __crc
   
   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __object
	   __thread
	   __rgc
	   __bit
	   __bignum
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
	   __r4_input_6_10_2
	   __r5_control_features_6_4
	   __mmap
	   __foreign
	   __error
	   __evenv
	   __os
	   __srfi4
	   __param)
   
   (export (crc-polynomial name)
	   (crc-names)
	   (crc-length name)
	   (crc-polynomial-le name)
	   (crc-polynomial-be->le len poly)
	   (crc name obj #!key (init 0) (final-xor 0) (big-endian? #t))
	   (crc-string name str::bstring #!key (init 0) (final-xor 0) (big-endian? #t))
	   (crc-port name p::input-port #!key (init 0) (final-xor 0) (big-endian? #t))
	   (crc-mmap name m::mmap #!key (init 0) (final-xor 0) (big-endian? #t))
	   (crc-file name f::bstring #!key (init 0) (final-xor 0) (big-endian? #t))
	   (inline crc-long::long c::char crc::long poly::long len::long)
	   (inline crc-elong::elong c::char crc::elong poly::elong len::long)
	   (inline crc-llong::llong c::char crc::llong poly::llong len::long)
	   (inline crc-long-le::long c::char crc::long poly::long len::long)
	   (inline crc-elong-le::elong c::char crc::elong poly::elong len::long)
	   (inline crc-llong-le::llong c::char crc::llong poly::llong len::long)
	   (register-crc! name poly len)))

;*---------------------------------------------------------------------*/
;*    crc-long ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (crc-long::long c::char crc::long poly::long len::long)
   
   (define (slow-crc)
      ;; note: the bit-mask will work on the not-yet-shifted crc.
      ;;       This allows us to use (for instance) a 32bit type for a 32-bit
      ;;       crc.
      (let* ((octet (char->integer c))
	     (value::long (bit-lsh octet (-fx len 8)))
	     (m (bit-lsh 1 (-fx len 1)))) ;; something like #x100000
	 (let loop ((i 0)
		    (crc::long (bit-xor crc value)))
	    (if (=fx i 8)
		crc
		(let ((new-crc::long (bit-lsh crc 1)))
		   (loop (+fx i 1)
			 ;; bit-and m crc will be either 0 or 1<<len-1
			 ;; we shift it back to 0 or 1 and multiply the
			 ;; generator polynomial with it. -> no 'if'.
			 (bit-xor (*fx (bit-rsh (bit-and m crc) (-fx len 1))
				       poly)
				  new-crc)))))))
   (define (even-slower-crc)
      ;; we can't pre-xor as the remainder is smaller than 8 bits...
      (let* ((octet (char->integer c))
	     (m (bit-lsh 1 (-fx len 1))) ;; something like #x10
	     (shifted-value (bit-lsh octet len))) ;; 8 bits too high.
	 (let loop ((i 0)
		    (crc::long crc)
		    (shifted-value shifted-value))
	    (if (=fx i 8)
		crc
		(let* ((crc2 (bit-xor crc
				      (bit-and m
					       (bit-rsh shifted-value
							8))))
		       (new-crc::long (bit-lsh crc2 1)))
		   (loop (+fx i 1)
			 (bit-xor (*fx (bit-rsh (bit-and m crc2)
						(-fx len 1))
				       poly)
				  new-crc)
			 (bit-lsh shifted-value 1)))))))
   (if (>=fx len 8) ;; this if should be extremely well branch-predicted.
       (slow-crc)
       (even-slower-crc)))

;*---------------------------------------------------------------------*/
;*    crc-elong ...                                                    */
;*    -------------------------------------------------------------    */
;*    See crc-long                                                     */
;*---------------------------------------------------------------------*/
(define-inline (crc-elong::elong c::char crc::elong poly::elong len::long)
   (if (>=fx len 8)
       (let ((octet (char->integer c)))
	  (let ((value::elong (bit-lshelong (fixnum->elong octet) (-fx len 8)))
		(m (bit-lshelong #e1 (-fx len 1))))
	     (let loop ((i 0)
			(crc::elong (bit-xorelong crc value)))
		(if (=fx i 8)
		    crc
		    (let ((new-crc::elong (bit-lshelong crc 1)))
		       (if (=elong #e0 (bit-andelong m crc))
			   (loop (+fx i 1) new-crc)
			   (loop (+fx i 1) (bit-xorelong new-crc poly))))))))
       (fixnum->elong (crc-long c (elong->fixnum crc) (elong->fixnum poly) len))))

;*---------------------------------------------------------------------*/
;*    crc-llong ...                                                    */
;*    -------------------------------------------------------------    */
;*    See crc-long                                                     */
;*---------------------------------------------------------------------*/
(define-inline (crc-llong::llong c::char crc::llong poly::llong len::long)
   (if (>=fx len 8)
       (let ((octet (char->integer c)))
	  (let ((value::llong (bit-lshllong (fixnum->llong octet) (-fx len 8)))
		(m (bit-lshllong #l1 (-fx len 1))))
	     (let loop ((i 0)
			(crc::llong (bit-xorllong crc value)))
		(if (=fx i 8)
		    crc
		    (let ((new-crc::llong (bit-lshllong crc 1)))
		       (if (=llong #l0 (bit-andllong m crc))
			   (loop (+fx i 1) new-crc)
			   (loop (+fx i 1) (bit-xorllong new-crc poly))))))))
       (fixnum->llong (crc-long c (llong->fixnum crc) (llong->fixnum poly) len))))

;*---------------------------------------------------------------------*/
;*    crc-long-le ...                                                  */
;*    -------------------------------------------------------------    */
;*    LSBit first. (sometimes needed/better)                           */
;*---------------------------------------------------------------------*/
(define-inline (crc-long-le::long c::char crc::long poly::long len::long)
   (let ((octet (char->integer c)))
      (let loop ((i 0)
		 (crc::long (bit-xor crc octet)))
	 (if (=fx i 8)
	     crc
	     (let ((new-crc::long (bit-ursh crc 1)))
		(loop (+fx i 1)
		      (bit-xor (*fx (bit-and 1 crc) poly)
			       new-crc)))))))

;*---------------------------------------------------------------------*/
;*    crc-elong-le ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (crc-elong-le::elong c::char crc::elong poly::elong len::long)
   (let ((octet (char->integer c)))
      (let loop ((i 0)
		 (crc::elong (bit-xorelong crc (fixnum->elong octet))))
	 (if (=fx i 8)
	     crc
	     (let ((new-crc::elong (bit-urshelong crc 1)))
		(loop (+fx i 1)
		      (bit-xorelong (*elong (bit-andelong #e1 crc) poly)
				    new-crc)))))))

;*---------------------------------------------------------------------*/
;*    crc-llong-le ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (crc-llong-le::llong c::char crc::llong poly::llong len::long)
   (let ((octet (char->integer c)))
      (let loop ((i 0)
		 (crc::llong (bit-xorllong crc (fixnum->llong octet))))
	 (if (=fx i 8)
	     crc
	     (let ((new-crc::llong (bit-urshllong crc 1)))
		(loop (+fx i 1)
		      (bit-xorllong (*llong (bit-andllong #l1 crc) poly)
				    new-crc)))))))

;*---------------------------------------------------------------------*/
;*    crc-polynomial-be->le ...                                        */
;*---------------------------------------------------------------------*/
(define (crc-polynomial-be->le len poly)
   (let* ((type
	   (cond
	      ((fixnum? poly) 'long)
	      ((elong? poly) 'elong)
	      ((llong? poly) 'llong)
	      (else (error 'crc-gen "could not determine type" poly))))
	  
	  (one (case type ((long) 1) ((elong) #e1) ((llong) #l1)))
	  (zero (case type ((long) 0) ((elong) #e0) ((llong) #l0)))
	  (lsh (case type
		  ((long) bit-lsh) ((elong) bit-lshelong)
		  ((llong) bit-lshllong)))
	  (rsh (case type
		  ((long) bit-rsh) ((elong) bit-rshelong)
		  ((llong) bit-rshllong)))
	  (b-and (case type
		    ((long) bit-and) ((elong) bit-andelong)
		    ((llong) bit-andllong)))
	  (b-or (case type
		   ((long) bit-or) ((elong) bit-orelong)
		   ((llong) bit-orllong))))
      (let loop ((i 0)
		 (poly poly)
		 (res zero))
	 (if (>=fx i len)
	     res
	     (loop (+fx i 1)
		   (rsh poly 1)
		   (b-or (lsh res 1) (b-and one poly)))))))

;*---------------------------------------------------------------------*/
;*    *crcs* ...                                                       */
;*    -------------------------------------------------------------    */
;*    On machines where elongs are bigger than 32 bits we want the     */
;*    polynomials to stay 32bits long. -> Negative polynomials are     */
;*    bit-anded. I'm not sure if this is really necessary, but         */
;*    let's not take risks.                                            */
;*---------------------------------------------------------------------*/
(define *crcs*
   `((ieee-32 32 #e79764919 ,(bit-andelong #exFFFFFFFF #e-306674912))
     (radix-64-24 24 8801531 14627425)
     (ccitt-16 16 4129 33800)
     (dnp-16 16 15717 42684)
     (ibm-16 16 32773 40961)
     (24 24 6122955 13874874)
     (30 30 #e540064207 #e1021788929)
     (c-32 32 #e517762881 ,(bit-andelong #exFFFFFFFF #e-2097792136))
     (k-32 32 #e1947962583 ,(bit-andelong #exFFFFFFFF #e-349054930))
     (q-32 32
	   ,(bit-andelong #exFFFFFFFF #e-2126429781)
	   ,(bit-andelong #exFFFFFFFF #e-712867199))
     (itu-4 4 3 12)
     (epc-5 5 9 18)
     (itu-5 5 21 21)
     (usb-5 5 5 20)
     (itu-6 6 3 48)
     (7 7 9 72)
     (atm-8 8 7 224)
     (ccitt-8 8 141 177)
     (dallas/maxim-8 8 49 140)
     (8 8 213 171)
     (sae-j1850-8 8 29 184)
     (10 10 563 817)
     (11 11 901 1294)
     (12 12 2063 3841)
     (can-15 15 17817 19665)
     (iso-64 64 #l27 (bit-andellong #lxFFFFFFFFFFFFFFFF #l-2882303761517117440))
     (ecma-182-64 64
		  #l4823603603198064275
		  (bit-andellong #lxFFFFFFFFFFFFFFFF #l-3932672073523589310))))

;*---------------------------------------------------------------------*/
;*    register-crc! ...                                                */
;*---------------------------------------------------------------------*/
(define (register-crc! name poly len)
   (set! *crcs*
	 (cons (list name len poly (crc-polynomial-be->le poly len)) *crcs*)))

;*---------------------------------------------------------------------*/
;*    crc-polynomial ...                                               */
;*---------------------------------------------------------------------*/
(define (crc-polynomial name)
   (let ((t (assq name *crcs*)))
      (and t (caddr t))))

;*---------------------------------------------------------------------*/
;*    crc-polynomial-le ...                                            */
;*---------------------------------------------------------------------*/
(define (crc-polynomial-le name)
   (let ((t (assq name *crcs*)))
      (and t (cadddr t))))

;*---------------------------------------------------------------------*/
;*    crc-length ...                                                   */
;*---------------------------------------------------------------------*/
(define (crc-length name)
   (let ((t (assq name *crcs*)))
      (and t (cadr t))))

;*---------------------------------------------------------------------*/
;*    crc-names ...                                                    */
;*---------------------------------------------------------------------*/
(define (crc-names)
   (map car *crcs*))

;*---------------------------------------------------------------------*/
;*    crc ...                                                          */
;*---------------------------------------------------------------------*/
(define (crc name obj #!key (init 0) (final-xor 0) (big-endian? #t))
   (cond
      ((string? obj)
       (crc-fast-port name (open-input-string obj) init final-xor big-endian?))
      ((input-port? obj)
       (crc-fast-port name obj init final-xor big-endian?))
      ((mmap? obj)
       (crc-fast-mmap name obj init final-xor big-endian?))
      (else
       (error 'crc "Illegal argument" obj))))

;*---------------------------------------------------------------------*/
;*    crc-file ...                                                     */
;*---------------------------------------------------------------------*/
(define (crc-file name f::bstring #!key (init 0) (final-xor 0) (big-endian? #t))
   (let ((p (open-input-file f)))
      (unless p (error 'crc-file "Could not open file" f))
      (unwind-protect
	 (crc-fast-port name p init final-xor big-endian?)
	 (close-input-port p))))

;*---------------------------------------------------------------------*/
;*    crc-string ...                                                   */
;*---------------------------------------------------------------------*/
(define (crc-string name str::bstring #!key (init 0) (final-xor 0) (big-endian? #t))
   (crc-fast-port name (open-input-string str) init final-xor  big-endian?))

;*---------------------------------------------------------------------*/
;*    crc-port ...                                                     */
;*---------------------------------------------------------------------*/
(define (crc-port name p::input-port #!key (init 0) (final-xor 0) (big-endian? #t))
   (crc-fast-port name p init final-xor  big-endian?))

;*---------------------------------------------------------------------*/
;*    crc-mmap ...                                                     */
;*---------------------------------------------------------------------*/
(define (crc-mmap name m::mmap #!key (init 0) (final-xor 0) (big-endian? #t))
   (crc-fast-mmap name m init final-xor big-endian?))

;*---------------------------------------------------------------------*/
;*    get-crc ...                                                      */
;*---------------------------------------------------------------------*/
(define (get-crc name)
   (let ((crc-desc (assoc name *crcs*)))
      (unless crc-desc
	 (error 'crc "Could not find crc" name))
      (let ((len (cadr crc-desc))
	    (poly (caddr crc-desc))
	    (lsb-poly (cadddr crc-desc)))
	 (values len poly lsb-poly))))

;*---------------------------------------------------------------------*/
;*    any->elong ...                                                   */
;*---------------------------------------------------------------------*/
(define (any->elong any)
   (if (fixnum? any)
       (fixnum->elong any)
       any))

;*---------------------------------------------------------------------*/
;*    any->llong ...                                                   */
;*---------------------------------------------------------------------*/
(define (any->llong any)
   (cond
      ((fixnum? any) (fixnum->llong any))
      ((elong? any) (elong->llong any))
      (else any)))

;*---------------------------------------------------------------------*/
;*    crc-fast-port ...                                                */
;*---------------------------------------------------------------------*/
(define (crc-fast-port name p::input-port init final-xor big-endian?)
   (multiple-value-bind (len poly lsb-poly)
      (get-crc name)
      (cond
	 ((fixnum? poly)
	  (if big-endian?
	      (crc-port-long p init final-xor poly len crc-long)
	      (crc-port-long p init final-xor lsb-poly len crc-long-le)))
	 ((elong? poly)
	  (if big-endian?
	      (crc-port-elong p (any->elong init) (any->elong final-xor) poly
			      len crc-elong)
	      (crc-port-elong p (any->elong init) (any->elong final-xor)
			      lsb-poly len crc-elong-le)))
	 ((llong? poly)
	  (if big-endian?
	      (crc-port-llong p (any->llong init) (any->llong final-xor) poly
			      len crc-llong)
	      (crc-port-llong p (any->llong init) (any->llong final-xor)
			      lsb-poly len crc-llong-le)))
	 (else
	  (error 'crc "bad polynomial" poly)))))

;*---------------------------------------------------------------------*/
;*    crc-fast-mmap ...                                                */
;*---------------------------------------------------------------------*/
(define (crc-fast-mmap name m::mmap init final-xor big-endian?)
   (multiple-value-bind (len poly lsb-poly)
      (get-crc name)
      (cond
	 ((fixnum? poly)
	  (if big-endian?
	      (crc-mmap-long m init final-xor poly len crc-long)
	      (crc-mmap-long m init final-xor lsb-poly len crc-long-le)))
	 ((elong? poly)
	  (if big-endian?
	      (crc-mmap-elong m (any->elong init) (any->elong final-xor)
			      poly len crc-elong)
	      (crc-mmap-elong m (any->elong init) (any->elong final-xor)
			      lsb-poly len crc-elong-le)))
	 ((llong? poly)
	  (if big-endian?
	      (crc-mmap-llong m (any->llong init) (any->llong final-xor)
			      poly len crc-llong)
	      (crc-mmap-llong m (any->llong init) (any->llong final-xor)
			      lsb-poly len crc-llong-le)))
	 (else
	  (error 'crc "bad polynomial" poly)))))

;*---------------------------------------------------------------------*/
;*    crc-mmap-long ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (crc-mmap-long::long mmap init::long final-xor::long poly::long
				    crc-len::long crc-fun::procedure)
   ;; slightly complicated way to compute the mask, but this way we are sure
   ;; not to overshoot. that is: if we work with a type of length n we won't
   ;; use anything bigger than that.
   ;; I hope that the compiler will optimize this.
   (define m (+fx (bit-lsh 1 (-fx crc-len 1))
		  (-fx (bit-lsh 1 (-fx crc-len 1)) 1)))
   
   (let ((len (mmap-length mmap)))
      (let loop ((i 0)
		 (crc::long init))
	 (if (=fx i len)
	     (bit-and (bit-xor final-xor crc) m)
	     (loop (+fx i 1)
		   (crc-fun ($mmap-ref mmap i) crc
			    poly crc-len))))))

;*---------------------------------------------------------------------*/
;*    crc-mmap-elong ...                                               */
;*    -------------------------------------------------------------    */
;*    see comments for crc-mmap-long                                   */
;*---------------------------------------------------------------------*/
(define-inline (crc-mmap-elong::elong mmap init::elong final-xor::elong
				      poly::elong crc-len::long
				      crc-fun::procedure)
   (define m (+elong (bit-lshelong #e1 (-fx crc-len 1))
		     (-elong (bit-lshelong #e1 (-fx crc-len 1)) #e1)))
   
   (let ((len (mmap-length mmap)))
      (let loop ((i 0)
		 (crc::elong init))
	 (if (=fx i len)
	     (bit-andelong (bit-xorelong final-xor crc) m)
	     (loop (+fx i 1)
		   (crc-fun ($mmap-ref mmap i) crc
			    poly crc-len))))))

;*---------------------------------------------------------------------*/
;*    crc-mmap-long ...                                                */
;*    -------------------------------------------------------------    */
;*    see comments for crc-mmap-long                                   */
;*---------------------------------------------------------------------*/
(define-inline (crc-mmap-llong::llong mmap init::llong final-xor::llong
				      poly::llong crc-len::long
				      crc-fun::procedure)
   (define m (+llong (bit-lshllong #l1 (-fx crc-len 1))
		     (-llong (bit-lshllong #l1 (-fx crc-len 1)) #l1)))
   
   (let ((len (mmap-length mmap)))
      (let loop ((i 0)
		 (crc::llong init))
	 (if (=fx i len)
	     (bit-andllong (bit-xorllong final-xor crc) m)
	     (loop (+fx i 1)
		   (crc-fun ($mmap-ref mmap i) crc
			    poly crc-len))))))

;*---------------------------------------------------------------------*/
;*    crc-port-long ...                                                */
;*    -------------------------------------------------------------    */
;*    see comments for crc-mmap-long                                   */
;*---------------------------------------------------------------------*/
(define-inline (crc-port-long::long p::input-port init::long final-xor::long
				    poly::long crc-len::long
				    crc-fun::procedure)
   (define m (+fx (bit-lsh 1 (-fx crc-len 1))
		  (-fx (bit-lsh 1 (-fx crc-len 1)) 1)))
   
   (let loop ((crc::long init))
      (let ((c (read-char p)))
	 (if (eof-object? c)
	     (bit-and (bit-xor final-xor crc) m)
	     (loop (crc-fun c crc poly crc-len))))))

;*---------------------------------------------------------------------*/
;*    crc-port-elong ...                                               */
;*    -------------------------------------------------------------    */
;*    see comments for crc-mmap-long                                   */
;*---------------------------------------------------------------------*/
(define-inline (crc-port-elong::elong p::input-port init::elong
				      final-xor::elong poly::elong
				      crc-len::long crc-fun::procedure)
   (define m (+elong (bit-lshelong #e1 (-fx crc-len 1))
		     (-elong (bit-lshelong #e1 (-fx crc-len 1)) #e1)))
   
   (let loop ((crc::elong init))
      (let ((c (read-char p)))
	 (if (eof-object? c)
	     (bit-andelong (bit-xorelong final-xor crc) m)
	     (loop (crc-fun c crc poly crc-len))))))

;*---------------------------------------------------------------------*/
;*    crc-mmap-llong ...                                               */
;*    -------------------------------------------------------------    */
;*    see comments for crc-mmap-long                                   */
;*---------------------------------------------------------------------*/
(define-inline (crc-port-llong::llong p::input-port init::llong
				      final-xor::llong poly::llong
				      crc-len::long crc-fun::procedure)
   (define m (+llong (bit-lshllong #l1 (-fx crc-len 1))
		     (-llong (bit-lshllong #l1 (-fx crc-len 1)) #l1)))
   
   (let loop ((crc::llong init))
      (let ((c (read-char p)))
	 (if (eof-object? c)
	     (bit-andllong (bit-xorllong final-xor crc) m)
	     (loop (crc-fun c crc poly crc-len))))))

;; Use this function to produce .texi doc.
#;(define (print-doc)
   (define (poly->string poly)
      (if (> poly 0)
	  (format "0x~x" poly)
	  (cond
	     ((fixnum? poly) ;; must not happen
	      (error 'crc-gen
		     "longs must never be < 0"
		     poly))
	     ((elong? poly)
	      (format "0x~x~x"
		      (bit-andelong #exFFFF (bit-rshelong poly 16))
		      (bit-andelong #exFFFF poly)))
	     ((llong? poly)
	      (format "0x~x~x"
		      (bit-andllong #lxFFFFFFFF (bit-rshllong poly 32))
		      (bit-andllong #lxFFFFFFFF poly))))))
   (print "@itemize @bullet")
   (for-each (lambda (crc-desc)
		(print "@item @code{" (car crc-desc) "}: " (poly->string (caddr
									  crc-desc))))
	     *crcs*)
   (print "@end itemize"))
