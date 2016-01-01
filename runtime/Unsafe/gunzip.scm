;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/gunzip.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar  5 07:43:02 2006                          */
;*    Last change :  Sun Dec 27 17:31:33 2015 (serrano)                */
;*    Copyright   :  2006-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Traduction of gzip's inflate.c inspired from Mzscheme's port.    */
;*    -------------------------------------------------------------    */
;*    The big difference between Bigloo and MzScheme/Gzip is that      */
;*    the Bigloo version is incremental (and thread-safe). This        */
;*    impacts the performance but this enables PORT->GZIP-PORT!        */
;*    -------------------------------------------------------------    */
;*    Quick timing on a x86, Pentium M 1.1GHz, Linux 2.6.15.4 box:     */
;*      gunzip bigloo2.8.tar.gz:                                       */
;*        gzip -d: 1.1s                                                */
;*        bigloo: 2.50s                                                */
;*        mzscheme: 81.82s                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gunzip
   
   (import  __error
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r5_control_features_6_4
	    __object
	    __rgc)
   
   (use     __type
	    __bigloo
	    __param
	    __bexit
	    __thread
	    __tvector
	    __structure
	    __tvector
	    __bit
	    __bignum
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __evenv)
   
   (extern ($open-input-gzip-port::obj (::procedure ::input-port ::bstring)
				       "bgl_open_input_gzip_port"))

   (java   (class foreign 
	       (method static $open-input-gzip-port::obj (::procedure ::input-port ::bstring)
		       "bgl_open_input_gzip_port")))
    
   (static (class huft e::long b::long v))

   (export (port->gzip-port::input-port ::input-port #!optional (bufinfo #t))
	   (port->zlib-port::input-port ::input-port #!optional (bufinfo #t))
	   (port->inflate-port::input-port ::input-port #!optional (bufinfo #t))
	   (open-input-gzip-file ::bstring #!optional (bufinfo #t) (timeout 1000000))
	   (open-input-zlib-file ::bstring #!optional (bufinfo #t) (timeout 1000000))
	   (open-input-inflate-file ::bstring #!optional (bufinfo #t) (timeout 1000000))
	   (gunzip-sendchars ::input-port ::output-port)
	   (inflate-sendchars ::input-port ::output-port)
	   (gunzip-parse-header in)))

#|
   Inflate deflated data

   Copyright (C) 1997, 1998, 1999, 2002 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING.
   If not, write to the Free Software Foundation,
   59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

   Not copyrighted 1992 by Mark Adler
   version c10p1, 10 January 1993 */

   You can do whatever you like with this source file, though I would
   prefer that if you modify it and redistribute it that you include
   comments to that effect with your name and the date.  Thank you.
   [The history has been moved to the file ChangeLog.]

   Inflate deflated (PKZIP's method 8 compressed) data.  The compression
   method searches for as much of the current string of bytes (up to a
   length of 258) in the previous 32K bytes.  If it doesn't find any
   matches (of at least length 3), it codes the next byte.  Otherwise, it
   codes the length of the matched string and its distance backwards from
   the current position.  There is a single Huffman code that codes both
   single bytes (called "literals") and match lengths.  A second Huffman
   code codes the distance information, which follows a length code.  Each
   length or distance code actually represents a base value and a number
   of "extra" (sometimes zero) bits to get to add to the base value.  At
   the end of each deflated block is a special end-of-block (EOB) literal/
   length code.  The decoding process is basically: get a literal/length
   code; if EOB then done; if a literal, emit the decoded byte; if a
   length then get the distance and emit the referred-to bytes from the
   sliding window of previously emitted data.

   There are (currently) three kinds of inflate blocks: stored, fixed, and
   dynamic.  The compressor deals with some chunk of data at a time, and
   decides which method to use on a chunk-by-chunk basis.  A chunk might
   typically be 32K or 64K.  If the chunk is uncompressible, then the
   "stored" method is used.  In this case, the bytes are simply stored as
   is, eight bits per byte, with none of the above coding.  The bytes are
   preceded by a count, since there is no longer an EOB code.

   If the data is compressible, then either the fixed or dynamic methods
   are used.  In the dynamic method, the compressed data is preceded by
   an encoding of the literal/length and distance Huffman codes that are
   to be used to decode this block.  The representation is itself Huffman
   coded, and so is preceded by a description of that code.  These code
   descriptions take up a little space, and so for small blocks, there is
   a predefined set of codes, called the fixed codes.  The fixed method is
   used if the block codes up smaller that way (usually for quite small
   chunks), otherwise the dynamic method is used.  In the latter case, the
   codes are customized to the probabilities in the current block, and so
   can code it much better than the pre-determined fixed codes.
 
   The Huffman codes themselves are decoded using a mutli-level table
   lookup, in order to maximize the speed of decoding plus the speed of
   building the decoding tables.  See the comments below that precede the
   lbits and dbits tuning parameters.

   Notes beyond the 1.93a appnote.txt:

   1. Distance pointers never point before the beginning of the output
      stream.
   2. Distance pointers can point back across blocks, up to 32k away.
   3. There is an implied maximum of 7 bits for the bit length table and
      15 bits for the actual data.
   4. If only one code exists, then it is encoded using one bit.  (Zero
      would be more efficient, but perhaps a little confusing.)  If two
      codes exist, they are coded using one bit each (0 and 1).
   5. There is no way of sending zero distance codes--a dummy must be
      sent if there are none.  (History: a pre 2.0 version of PKZIP would
      store blocks with no distance codes, but this was discovered to be
      too harsh a criterion.)  Valid only for 1.93a.  2.04c does allow
      zero distance codes, which is sent as one code of zero bits in
      length.
   6. There are up to 286 literal/length codes.  Code 256 represents the
      end-of-block.  Note however that the static length tree defines
      288 codes just to fill out the Huffman codes.  Codes 286 and 287
      cannot be used though, since there is no length base or extra bits
      defined for them.  Similarly, there are up to 30 distance codes.
      However, static trees define 32 codes (all 5 bits) to fill out the
      Huffman codes, but the last two had better not show up in the data.
   7. Unzip can check dynamic Huffman blocks for complete code sets.
      The exception is that a single code would not be complete (see #4).
   8. The five bits following the block type is really the number of
      literal codes sent minus 257.
   9. Length codes 8,16,16 are interpreted as 13 length codes of 8 bits
      (1+6+6).  Therefore, to output three times the length, you output
      three codes (1+1+1), whereas to output four times the same length,
      you only need two codes (1+3).  Hmm.
  10. In the tree reconstruction algorithm, Code = Code + Increment
      only if BitLength(i) is not zero.  (Pretty obvious.)
  11. Correction: 4 Bits: # of Bit Length codes - 4     (4 - 19)
  12. Note: length code 284 can represent 227-258, but length code 285
      really is 258.  The last length deserves its own, short code
      since it gets used a lot in very redundant files.  The length
      258 is special since 258 - 3 (the min match length) is 255.
  13. The literal/length and distance code bit lengths are read as a
      single stream of lengths.  It is possible (and advantageous) for
      a repeat code (16, 17, or 18) to go across the boundary between
      the two sets of lengths.
 |#


;*---------------------------------------------------------------------*/
;*    huft accessors ...                                               */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo-class-sans
      
(define-macro (huft-e o)
   (let ((id (gensym)))
      `(with-access::huft ,o ((,id e)) ,id)))

(define-macro (huft-e-set! o v)
   (let ((id (gensym)))
      `(with-access::huft ,o ((,id e)) (set! ,id ,v))))

(define-macro (huft-v o)
   (let ((id (gensym)))
      `(with-access::huft ,o ((,id v)) ,id)))

(define-macro (huft-v-set! o v)
   (let ((id (gensym)))
      `(with-access::huft ,o ((,id v)) (set! ,id ,v))))

(define-macro (huft-b o)
   (let ((id (gensym)))
      `(with-access::huft ,o ((,id b)) ,id)))

(define-macro (huft-b-set! o v)
   (let ((id (gensym)))
      `(with-access::huft ,o ((,id b)) (set! ,id ,v))))
))

;*---------------------------------------------------------------------*/
;*    inflate-buffer-size ...                                          */
;*    -------------------------------------------------------------    */
;*    The algorithm requires a size of 32KB.                           */
;*---------------------------------------------------------------------*/
(define (inflate-buffer-size)
   32768)

;*---------------------------------------------------------------------*/
;*    huft-copy! ...                                                   */
;*---------------------------------------------------------------------*/
(define (huft-copy! dest src)
   (with-access::huft dest ((ed e) (bd b) (vd v))
      (with-access::huft src ((es e) (bs b) (vs v))
	 (set! ed es)
	 (set! bd bs)
	 (set! vd vs))))

;*---------------------------------------------------------------------*/
;*    ++ ...                                                           */
;*---------------------------------------------------------------------*/
(define-macro (++ var)
   (let ((old (gensym)))
      `(let ((,old ,var))
	  (set! ,var (+fx ,var 1))
	  ,old)))

;*---------------------------------------------------------------------*/
;*    step ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (step start end f)
   (let ((i (gensym 'i))
	 (loop (gensym 'loop)))
      `(let ,loop ((,i ,start))
	    (when (<fx ,i ,end)
	       (,f ,i)
	       (,loop (+fx ,i 1))))))

;*---------------------------------------------------------------------*/
;*    build-vector ...                                                 */
;*---------------------------------------------------------------------*/
(define (build-vector n p)
   (let ((v (make-vector n)))
      (step 0 n (lambda (i) (vector-set! v i (p i))))
      v))

;*---------------------------------------------------------------------*/
;*    subvector ...                                                    */
;*---------------------------------------------------------------------*/
(define (subvector v::vector offset::long)
   (let* ((len (-fx (vector-length v) offset))
	  (new (make-vector len)))
      (step 0 len
	    (lambda (i) 
	       (vector-set! new i (vector-ref v (+fx i offset)))))
      new))

;*---------------------------------------------------------------------*/
;*    Tables for deflate from PKZIP's appnote.txt                      */
;*---------------------------------------------------------------------*/
;; Order of the bit length code lengths
(define (border::vector)
   '#(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

;; Copy lengths for literal codes 257..285
(define (cplens::vector)
   '#(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31
        35 43 51 59 67 83 99 115 131 163 195 227 258 0 0))

;; Extra bits for literal codes 257..285
(define (cplext::vector)
   '#(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2
        3 3 3 3 4 4 4 4 5 5 5 5 0 99 99))

;; Copy offsets for distance codes 0..29
(define (cpdist::vector)
   '#(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193
        257 385 513 769 1025 1537 2049 3073 4097 6145
        8193 12289 16385 24577))

;; Extra bits for distance codes
(define (cpdext::vector)
   '#(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6
        7 7 8 8 9 9 10 10 11 11
        12 12 13 13))

(define (mask-bits::vector)
   '#(#x0000
      #x0001 #x0003 #x0007 #x000f #x001f #x003f #x007f #x00ff
      #x01ff #x03ff #x07ff #x0fff #x1fff #x3fff #x7fff #xffff))

;; bits in base literal/length lookup table
(define (lbits::long) 9) 
;; bits in base distance lookup table
(define (dbits::long) 6) 

(define (BMAX::long) 16)
(define (N-MAX::long) 288)

;*---------------------------------------------------------------------*/
;*    gunzip-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (gunzip-error proc msg obj)
   (raise (instantiate::&io-parse-error
	     (proc proc)
	     (msg msg)
	     (obj obj))))

;*---------------------------------------------------------------------*/
;*    inflate ...                                                      */
;*---------------------------------------------------------------------*/
(define (inflate-entry input-port slide)
   
   ;; decompress an inflated entry
   ;; initialize window, bit buffer
   (define wsize (string-length slide))
   
   (define wp 0)
   (define w 0)
   
   ;; bit buffer
   (define bb 0)
   ;; bits in bit buffer
   (define bk 0)

   (define buffer (make-string 256))
   (define bufpos 256)

   (define i 0)
   
   (define (GETBYTE::long)
      (let ((grammar (regular-grammar ()
			((in all #\Newline) (the-byte))
			(else (gunzip-error
				 "inflate" "premature end of file" input-port)))))
	 (read/rp grammar input-port)))

   (define (NEEDBITS n)
      (let loop ()
	 (when (<fx bk n)
	    (set! bb (+fx bb (bit-lsh (GETBYTE) bk)))
	    (set! bk (+fx bk 8))
	    (loop))))
   
   (define (DUMPBITS n)
      (set! bb (bit-rsh bb n))
      (set! bk (-fx bk n)))
   
   (define (GETBITS n)
      (NEEDBITS n)
      (let ((r bb))
	 (DUMPBITS n)
	 r))
   
   (define (check-flush)
      (if (=fx wp wsize)
	  (begin
	     (set! wp 0)
	     wsize)
	  0))
   
   (define (huft-build b::vector n::long s::long d::vector e::vector m::long incomp-okp::bool)
      (define c (make-vector (+fx (BMAX) 1) 0))
      (define v (make-vector (N-MAX)))
      (define x (make-vector (+fx (BMAX) 1)))
      (define final-y 0)
      (define t-result #f)
      
      ;; Generate counts for each bit length
      (step 0 n
	    (lambda (i)
	       (let ((*p (vector-ref b i)))
		  (vector-set! c *p (+fx (vector-ref c *p) 1)))))
      
      (if (=fx (vector-ref c 0) n)
	  ;; null input--all zero length codes
	  (values #f 0 #f)
	  ;; Find minimum and maximum length, bound *m by those
	  (let* ((j (let loop ((j 1))
		       (cond
			  ((>fx j (BMAX)) j)
			  ((>fx (vector-ref c j) 0) j)
			  (else (loop (+fx j 1))))))
		 (k j)
		 (i (let loop ((i (BMAX)))
		       (cond
			  ((=fx i 0) 0)
			  ((>fx (vector-ref c i) 0) i)
			  (else (loop (-fx i 1))))))
		 (g i)
		 (l (minfx (maxfx m j) i))
		 (m-result l))
	     
	     ;; Adjust last length count to fill out codes, if needed
	     (let ((y0 (let loop ((y (bit-lsh 1 j))
				  (j j))
			  (if (>=fx j i)
			      y
			      (let ((y2 (-fx y (vector-ref c j))))
				 (if (<fx y2 0)
				     (gunzip-error
				      "inflate"
				      "bad input: more codes than bits"
				      input-port)
				     (loop (*fx y2 2) (+fx j 1))))))))
		(set! final-y (-fx y0 (vector-ref c i)))
		(when (<fx final-y 0)
		   (gunzip-error "inflate"
				 "bad input: mode codes than bits"
				 input-port))
		(vector-set! c i (+fx (vector-ref c i) final-y)))
	     
	     ;; Generate starting offsets into the value table for each length
	     (vector-set! x 1 0)
	     (let ((j (let loop ((i (-fx i 1))
				 (x-pos 2)
				 (c-pos 1)
				 (j 0))
			 (if (=fx i 0)
			     j
			     (let ((v (vector-ref c c-pos)))
				(vector-set! x x-pos (+fx j v))
				(loop (-fx i 1)
				      (+fx x-pos 1)
				      (+fx c-pos 1)
				      (+fx j v)))))))
		
		;; Make a table of values in order of bit lengths
		(let loop ((i 0)
			   (b-pos 0))
		   (let ((j (vector-ref b b-pos)))
		      (unless (=fx j 0)
			 (let ((xj (vector-ref x j)))
			    (vector-set! x j (+fx 1 xj))
			    (vector-set! v xj i)))
		      (let ((i2 (+fx 1 i)))
			 (when (<fx i2 n)
			    (loop i2 (+fx 1 b-pos))))))
		
		;; Generate the Huffman codes and for each,
		;; make the table entries
		(vector-set! x 0 0)
		(let ((v-pos 0)
		      (i 0)
		      (h -1)
		      (w (negfx l))
		      (u (make-vector (BMAX)))
		      (q #unspecified)
		      (z 0)
		      (r (instantiate::huft (e 0) (b 0) (v 0))))
		   
		   ;; go through the bit lengths (k already is
		   ;; bits in shortest code
		   (let k-loop ((k k))
		      (when (<=fx k g)
			 (let ((a (vector-ref c k)))
			    (let a-loop ((a (-fx a 1)))
			       (unless (<fx a 0)
				  ;; here i is the Huffman code of length
				  ;; k bits for value *p
				  ;; make tables up to required level
				  (let kwl-loop ()
				     (when (>fx k (+fx w l))
					(set! h (+fx h 1))
					(set! w (+fx w l))
					
					;; compute minimum size table
					;; less than or equal to l bits
					(set! z (minfx (-fx g w) l))
					(let* ((j (-fx k w))
					       (f (bit-lsh 1 j)))
					   (when (>fx f (+fx a 1))
					      (set! f (-fx f (+fx a 1)))
					      (let loop ((c-pos k))
						 (set! j (+fx j 1))
						 (when (<fx j z)
						    (set! f (*fx f 2))
						    (let* ((c-pos (+fx c-pos 1))
							   (cv (vector-ref c c-pos)))
						       (unless (<=fx f cv)
							  (set! f (-fx f cv))
							  (loop c-pos))))))
					   (set! z (bit-lsh 1 j))
					   
					   ;; allocate and link in new table
					   (set! q (build-vector
						    z (lambda (i)
							 (instantiate::huft
							    (e 0)
							    (b 0)
							    (v 0)))))
					   
					   (when (not t-result)
					      (set! t-result q))
					   
					   (vector-set! u h q)
					   
					   ;; connect to last table,
					   ;; if there is one
					   (unless (=fx h 0)
					      (vector-set! x h i)
					      (huft-b-set! r l)
					      (huft-e-set! r (+fx j 16))
					      (huft-v-set! r q)
					      (set! j (bit-rsh i (-fx w l)))
					      ;; connect to last table:
					      (huft-copy!
					       (vector-ref
						(vector-ref u (-fx h 1)) j)
					       r))) 
					
					(kwl-loop)))
				  
				  (huft-b-set! r (-fx k w))
				  
				  (if (>=fx v-pos n)
				      (huft-e-set! r 99)
				      (let ((vv (vector-ref v v-pos)))
					 (if (<fx vv s)
					     (begin
						(huft-e-set!
						 r (if (<fx vv 256) 16 15))
						(huft-v-set! r vv))
					     (begin
						(huft-e-set!
						 r (vector-ref e (-fx vv s)))
						(huft-v-set!
						 r (vector-ref d (-fx vv s)))))
					 (set! v-pos (+fx v-pos 1))))
				  ;; fill code-like entries with r
				  (let ((f (bit-lsh 1 (-fx k w))))
				     (let loop ((j (bit-rsh i w)))
					(when (<fx j z)
					   (huft-copy! (vector-ref q j) r)
					   (loop (+fx j f)))))
				  ;; backwards increment the k-bit code i
				  (let loop ((j (bit-lsh 1 (-fx k 1))))
				     (if (positive? (bit-and i j))
					 (begin
					    (set! i (bit-xor i j))
					    (loop (bit-rsh j 1)))
					 (set! i (bit-xor i j))))
				  ;; backup over finished tables
				  (let loop ()
				     (unless (=fx (vector-ref x h)
						  (bit-and
						   i (-fx (bit-lsh 1 w) 1)))
					(set! h (-fx h 1))
					(set! w (-fx w l))
					(loop)))
				  
				  (a-loop (-fx a 1))))
			    (k-loop (+fx k 1)))))
		   
		   ;; Return #f as third if we were given an incomplete table
		   ; (printf "done: ~s ~s~n" final-y g)
		   (let ((okp (or incomp-okp
				  (not (and (not (=fx 0 final-y))
					    (not (=fx g 1)))))))
		      (unless okp 
			 (gunzip-error "inflate-entry"
				       "incomplete table"
				       input-port))
		      (values t-result m-result okp)))))))
   
   (define (inflate-codes tl td bl bd)
      ;; nflate the coded data
      (let ((ml (vector-ref (mask-bits) bl))
	    (md (vector-ref (mask-bits) bd))
	    (t #unspecified)
	    (e 0)
	    (n 0)
	    (d 0))
	 
	 (define (jump-to-next)
	    (let loop ()
	       (when (=fx e 99)
		  (gunzip-error "inflate-entry"
				(format "bad inflate code `~a'" e)
				input-port))
	       (DUMPBITS (huft-b t))
	       (set! e (-fx e 16))
	       (NEEDBITS e)
	       (set! t (vector-ref
			(huft-v t) (bit-and bb (vector-ref (mask-bits) e))))
	       (set! e (huft-e t))
	       (when (>fx e 16) (loop))))
	 
	 ;; do-copy
	 (define (do-copy)
	    (set! d (bit-and d (-fx wsize 1)))
	    (set! e (minfx n (-fx wsize (maxfx d wp))))
	    (set! n (-fx n e))
	    (let liip ()
	       (string-set! slide wp (string-ref slide d))
	       (set! wp (+fx wp 1))
	       (set! d (+fx d 1))
	       (set! e (-fx e 1))
	       (unless (=fx e 0) (liip)))
	    (let ((r (check-flush)))
	       (cond
		  ((=fx n 0)
		   (loop-inflate r))
		  ((=fx r 0)
		   (do-copy))
		  (else
		   (values 'flush r (lambda () (%do-copy2)))))))

	 ;; %do-copy2 (this function does the very
	 ;; same thing as do-copy. It exists only to
	 ;; permit loop-inflate and do-copy to be compiled
	 ;; as a while (this is the first example I have
	 ;; ever seen where the tail-rec optimization
	 ;; is needed).
	 (define (%do-copy2)
	    (set! d (bit-and d (-fx wsize 1)))
	    (set! e (minfx n (-fx wsize (maxfx d wp))))
	    (set! n (-fx n e))
	    (let liip2 ()
	       (string-set! slide wp (string-ref slide d))
	       (set! wp (+fx wp 1))
	       (set! d (+fx d 1))
	       (set! e (-fx e 1))
	       (unless (=fx e 0) (liip2)))
	    (let ((r (check-flush)))
	       (cond
		  ((=fx n 0)
		   (loop-inflate r))
		  ((=fx r 0)
		   (%do-copy2))
		  (else
		   (values 'flush r (lambda () (%do-copy2)))))))

	 (define (loop-inflate ret)
	    (if (>fx ret 0)
		(values 'flush ret (lambda () (loop-inflate 0)))
		(begin
		   (NEEDBITS bl)
		   (set! t (vector-ref tl (bit-and bb ml)))
		   (set! e (huft-e t))
		   (when (>fx e 16) (jump-to-next))
		   (DUMPBITS (huft-b t))
		   (if (=fx e 16)
		       ;; then it's a literal
		       (begin
			  (string-set! slide wp (integer->char (huft-v t)))
			  (set! wp (+fx wp 1))
			  (loop-inflate (check-flush)))
		       ;; it's an EOB or a length
		       ;; exit if end of block
		       (if (=fx e 15)
			   (values 'return #t #f)
			   (begin
			      ;; get length of block to copy
			      (NEEDBITS e)
			      (set! n (+fx (huft-v t)
					   (bit-and
					    bb (vector-ref (mask-bits) e))))
			      (DUMPBITS e)
			      
			      ;; decode distance of block to copy
			      (NEEDBITS bd)
			      (set! t (vector-ref td (bit-and bb md)))
			      (set! e (huft-e t))
			      (when (>fx e 16) (jump-to-next))
			      (DUMPBITS (huft-b t))
			      
			      (NEEDBITS e)
			      (set! d (modulofx
				       (-fx wp
					    (+fx (huft-v t)
						 (bit-and
						  bb
						  (vector-ref (mask-bits) e))))
				       wsize))
			      (DUMPBITS e)
			      
			      ;; do the copy
			      (do-copy)))))))

	 ;; do until end of block
	 (loop-inflate 0)))
   
   (define (inflate-stored)
      ;; "decompress" an inflated type 0 (stored) block.
      
      ;; go to byte boundary
      (DUMPBITS (bit-and bk 7))
      
      ;; get the length and its complement
      (NEEDBITS 16)
      (let ((n (bit-and bb #xffff)))
	 (DUMPBITS 16)
	 (NEEDBITS 16)
	 (unless (=fx n (bit-and (bit-not bb) #xffff))
	    (gunzip-error "inflate-entry"
			  (format "error in compressed data `~a'" n)
			  input-port))
	 (DUMPBITS 16)
	 
	 ;; read and output the compressed data
	 (let loop ((n n))
	    (if (>fx n 0)
		(begin
		   (NEEDBITS 8)
		   (string-set! slide wp (integer->char (bit-and bb #xff)))
		   (set! wp (+fx wp 1))
		   (let ((r (check-flush)))
		      (DUMPBITS 8)
		      (if (>fx r 0)
			  (values 'flush r (lambda () (loop (-fx n 1))))
			  (loop (-fx n 1)))))
		(values 'return #t #unspecified)))))
   
   (define (inflate-fixed)
      ;; decompress an inflated type 1 (fixed Huffman codes) block.  We should
      ;; either replace this with a custom decoder, or at least precompute the
      ;; Huffman tables.
      (let ((l (make-vector 288)))
	 
	 (step 0 144 (lambda (i) (vector-set! l i 8)))
	 (step 144 256 (lambda (i) (vector-set! l i 9)))
	 (step 256 280 (lambda (i) (vector-set! l i 7)))
	 (step 280 288 (lambda (i) (vector-set! l i 8)))
	 
	 (multiple-value-bind (tl bl ok?)
	    (huft-build l 288 257 (cplens) (cplext) 7 #f)
	    
	    (and ok?
		 (begin
		    (step 0 30 (lambda (i) (vector-set! l i 5)))
		    (multiple-value-bind (td bd ok?)
		       (huft-build l 30 0 (cpdist) (cpdext) 5 #t)
		       (and ok?
			    ;; decompress until an end-of-block code
			    (inflate-codes tl td bl bd))))))))
   
   (define (inflate-dynamic)
      ;; decompress an inflated type 2 (dynamic Huffman codes) block.
      
      ;; read in table lengths
      (define nl (+fx 257 (bit-and (GETBITS 5) #x1f)))
      (define nd (+fx 1 (bit-and (GETBITS 5) #x1f)))
      (define nb (+fx 4 (bit-and (GETBITS 4) #xf)))
      (define ll (make-vector (+fx 286 30)))
      (define i 0)
      (define l 0)
      
      (cond
	 ((>fx nl 286)
	  (gunzip-error "inflate" (format "bad lengths `~a'" nl) input-port))
	 ((>fx nd 30)
	  (gunzip-error "inflate" (format "bad lengths `~a'" nd) input-port))
	 (else
	  ;; read in bit-length-code lengths
	  (step 0 nb
		(lambda (j) 
		   (vector-set!
		    ll (vector-ref (border) j) (bit-and (GETBITS 3) 7))))
	  (step nb 19
		(lambda (j) 
		   (vector-set!
		    ll (vector-ref (border) j) 0)))
	  
	  ;; build decoding table for trees--single level, 7 bit lookup
	  (multiple-value-bind (tl bl ok?)
	     (huft-build ll 19 19 '#() '#() 7 #f)
	     (and ok?
		  (begin
		     ;; read in literal and distance code lengths
		     (let ((n (+fx nl nd))
			   (m (vector-ref (mask-bits) bl)))
			(set! i 0)
			(set! l 0)
			(let loop ()
			   (when (<fx i n)
			      (NEEDBITS bl)
			      (let* ((pos (bit-and bb m))
				     (td (vector-ref tl pos))
				     (dmp (huft-b td))
				     (j (huft-v td))
				     (set-lit (lambda (j l)
						 (when (>fx (+fx i j) n)
						    (gunzip-error
						     "inflate"
						     (format "bad hop `~a'" n)
						     input-port))
						 (let loop ((j j))
						    (unless (=fx j 0)
						       (vector-set! ll i l)
						       (set! i (+fx i 1))
						       (loop (-fx j 1)))))))
				 (DUMPBITS dmp)
				 (cond
				    ((<fx j 16)
				     ;; length of code in bits (0..15)
				     (vector-set! ll i j)
				     (set! l j)
				     ;; save last length in l
				     (set! i (+fx i 1)))
				    ((=fx j 16)
				     ;; repeat last length 3 to 6 times
				     (let ((j (+fx 3 (bit-and (GETBITS 2) 3))))
					(set-lit j l)))
				    ((= j 17)
				     ;; 3 to 10 zero length codes
				     (let ((j (+fx 3 (bit-and (GETBITS 3) 7))))
					(set-lit j 0)
					(set! l 0)))
				    (else
				     ;; j == 18: 11 to 138 zero length codes
				     (let ((j (+fx 11 (bit-and (GETBITS 7) #x7f))))
					(set-lit j 0)
					(set! l 0)))))
			      (loop)))
			
			;; build the decoding tables for
			;; literal/length and distance codes
			(multiple-value-bind (tl bl ok?)
			   (huft-build ll nl 257 (cplens) (cplext) (lbits) #f)
			   (if (not ok?)
			       (gunzip-error "inflate"
					     "incomplete code set"
					     input-port)
			       (multiple-value-bind (td bd ok?)
				  (huft-build (subvector ll nl) nd 0 (cpdist) (cpdext) (dbits) #f)
				  (if (not ok?)
				      (gunzip-error "inflate"
						    "incomplete code set"
						    input-port)
				      ;; decompress until an end-of-block code
				      (inflate-codes tl td bl bd))))))))))))
   
   (define (inflate-block)
      ;; decompress an inflated block
      (let ((e (bit-and (GETBITS 1) 1)))
	 ;; read in block type
	 (let ((t (bit-and (GETBITS 2) 3)))
	    (multiple-value-bind (state val kont)
	       (case t
		  ((2) (inflate-dynamic))
		  ((0) (inflate-stored))
		  ((1) (inflate-fixed))
		  (else (gunzip-error "inflate"
				      (format "unknown inflate type `~A'" t)
				      input-port)))
	       (let loop ((state state)
			  (val val)
			  (kont kont))
		  (case state
		     ((return)
		      (values 'return e val))
		     ((flush)
		      (values 'flush
			      val
			      (lambda ()
				 (multiple-value-bind (state2 val2 kont2)
				    (kont)
				    (loop state2 val2 kont2)))))
		     (else
		      (gunzip-error "inflate" "Illegal state" state))))))))
   
   ;; initialize window, bit buffer
   (set! wp 0)
   (set! bk 0)
   (set! bb 0)
   
   ;; decompress until the last block
   (let loop ((h 0))
      (let ((hufts 0))
	 (multiple-value-bind (state e r)
	    (inflate-block)
	    (let laap ((state state)
		       (e e)
		       (r r))
	       (case state
		  ((return)
		   (if (and r (=fx e 0))
		       (loop (if (>fx hufts h) hufts h))
		       ;; Undo too much lookahead. The next read will be
		       ;; byte aligned so we can discard unused bits in
		       ;; the last meaningful byte
		       (values 'complete wp #unspecified)))
		  ((flush)
		   (values 'step
			   e
			   (lambda ()
			      (multiple-value-bind (state2 e2 r2)
				 (r)
				 (laap state2 e2 r2)))))
		  (else
		   (gunzip-error "inflate" "Illegal state" state))))))))

;*---------------------------------------------------------------------*/
;*    make-small-endian2 ...                                           */
;*---------------------------------------------------------------------*/
(define (make-small-endian2 a b)
   (bit-or (char->integer a) (bit-lsh (char->integer b) 8)))

;*---------------------------------------------------------------------*/
;*    make-small-endian4 ...                                           */
;*---------------------------------------------------------------------*/
(define (make-small-endian4 a b c d)
   (bit-orelong
    (fixnum->elong (char->integer a))
    (bit-orelong
     (bit-lshelong (fixnum->elong (char->integer b)) 8)
     (bit-orelong
      (bit-lshelong (fixnum->elong (char->integer c)) 16)
      (bit-lshelong (fixnum->elong (char->integer d)) 24)))))

;*---------------------------------------------------------------------*/
;*    gunzip-parse-header ...                                          */
;*---------------------------------------------------------------------*/
(define (gunzip-parse-header in)
   (define buf (make-string 4))
   (define (read-int2)
      (read-chars! buf 2 in)
      (make-small-endian2 (string-ref buf 0) (string-ref buf 1)))
   (define (read-int4)
      (read-chars! buf 4 in)
      (make-small-endian4 (string-ref buf 0) (string-ref buf 1)
			  (string-ref buf 2) (string-ref buf 3)))
   (define (read-null-term-string)
      (let loop ((s '()))
	 (let ((r (read-char in)))
	    (if (char=? #a000 r)
		(list->string (reverse! s))
		(loop (cons r s))))))
   (let ((header (read-chars 2 in)))
      (unless (and (string? header)
		   (=fx (string-length header) 2)
		   (=fx (char->integer (string-ref header 0)) #o37)
		   (=fx (char->integer (string-ref header 1)) #o213))
	 (gunzip-error "gunzip"
		       (format "bad header `~a'" header)
		       in)))
   (let ((compression-type (read-char in)))
      (unless (eq? compression-type #\010)
	 (gunzip-error "gunzip"
		       (format "unknown compression type `~a'"
			       compression-type)
		       in)))
   (let* ((flags (char->integer (read-char in)))
	  (ascii? (positive? (bit-and flags #b1)))
	  (continuation? (positive? (bit-and flags #b10)))
	  (has-extra-field? (positive? (bit-and flags #b100)))
	  (has-original-filename? (positive? (bit-and flags #b1000)))
	  (has-comment? (positive? (bit-and flags #b10000)))
	  (encrypted? (positive? (bit-and flags #b100000))))
      (when encrypted?
	 (gunzip-error "gunzip" "cannot unzip encrypted file" in))
      (when continuation?
	 (gunzip-error "gunzip" "cannot handle multi-part files" in))
      (let* ((unix-mod-time (read-int4))
	     (extra-flags (read-char in))
	     (source-os (read-char in)))
	 (when continuation?
	    (read-int2))
	 (when has-extra-field?
	    (let ((len (read-int2)))
	       (let loop ((len len))
		  (unless (zero? len)
		     (read-char in)
		     (loop (-fx len 1))))))
	 (let* ((original-filename (and has-original-filename?
					(read-null-term-string)))
		(comment (and has-comment? (read-null-term-string))))
	    (when encrypted?
	       (let loop ((n 12))
		  (unless (zero? n)
		     (read-char in)
		     (loop (-fx n 1)))))))))

;*---------------------------------------------------------------------*/
;*    inflate ...                                                      */
;*---------------------------------------------------------------------*/
(define (inflate in::input-port out::output-port)
   (define buffer (make-string (inflate-buffer-size)))
   (multiple-value-bind (state val kont)
      (inflate-entry in buffer)
      (let loop ((state state)
		 (val val)
		 (kont kont)
		 (size 0))
	 (case state
	    ((complete)
	     (display-substring buffer 0 val out)
	     (set! buffer #f)
	     (+fx size val))
	    ((step)
	     (display-substring buffer 0 val out)
	     (multiple-value-bind (state2 val2 kont2)
		(kont)
		(loop state2 val2 kont2 (+fx val size))))))))

;*---------------------------------------------------------------------*/
;*    gunzip ...                                                       */
;*---------------------------------------------------------------------*/
(define (gunzip in::input-port out::output-port)
   (gunzip-parse-header in)
   (inflate in out))

;*---------------------------------------------------------------------*/
;*    gunzip-sendchars ...                                             */
;*---------------------------------------------------------------------*/
(define (gunzip-sendchars in::input-port out::output-port)
   (gunzip (c-input-gzip-port-input-port in) out))

;*---------------------------------------------------------------------*/
;*    inflate-sendchars ...                                            */
;*---------------------------------------------------------------------*/
(define (inflate-sendchars in::input-port out::output-port)
   (inflate (c-input-gzip-port-input-port in) out))

;*---------------------------------------------------------------------*/
;*    subbuffer ...                                                    */
;*---------------------------------------------------------------------*/
(define (subbuffer buffer len bufsize in)
   (if (=fx len bufsize)
       buffer
       (substring buffer 0 len)))

;*---------------------------------------------------------------------*/
;*    subbuffer! ...                                                   */
;*---------------------------------------------------------------------*/
(define (subbuffer! buffer len bufsize in)
   (if (=fx len bufsize)
       buffer
       (string-shrink! buffer len)))

;*---------------------------------------------------------------------*/
;*    port->port ...                                                   */
;*---------------------------------------------------------------------*/
(define (port->port::input-port in::input-port state bufinfo bufsize oncomplete)
   (let ((buffer (make-string bufsize))
	 (state state)
	 (kont #unspecified))
      ($open-input-gzip-port
       (lambda ()
	  (let loop ((val 0))
	     (case state
		((eof)
		 (when (procedure? oncomplete)
		    (oncomplete in buffer))
		 (set! buffer #f)
		 #f)
		((complete)
		 (set! state 'eof)
		 (subbuffer! buffer val bufsize in))
		((step)
		 (set! state 'resume)
		 (subbuffer buffer val bufsize in))
		((resume)
		 (multiple-value-bind (state2 val2 kont2)
		    (kont)
		    (set! state state2)
		    (set! kont kont2)
		    (loop val2)))
		((port->gzip-port)
		 (gunzip-parse-header in)
		 (set! state 'port->inflate-port)
		 (loop val))
		((port->inflate-port)
		 (multiple-value-bind (state0 val0 kont0)
		    (inflate-entry in buffer)
		    (set! state state0)
		    (set! kont kont0)
		    (loop val0)))
		(else
		 (error "port->port" "Illegal state" state)))))
       in
       (get-port-buffer state bufinfo c-default-io-bufsiz))))

;*---------------------------------------------------------------------*/
;*    port->inflated-port ...                                          */
;*---------------------------------------------------------------------*/
(define (port->inflate-port::input-port in::input-port #!optional (bufinfo #t))
   (port->port in 'port->inflate-port bufinfo (inflate-buffer-size) #f))
   
;*---------------------------------------------------------------------*/
;*    port->gzip-port ...                                              */
;*---------------------------------------------------------------------*/
(define (port->gzip-port::input-port in::input-port #!optional (bufinfo #t))
   (port->port in 'port->gzip-port bufinfo (inflate-buffer-size) #f))

;*---------------------------------------------------------------------*/
;*    open-input-gzip-file ...                                         */
;*---------------------------------------------------------------------*/
(define (open-input-gzip-file name #!optional (bufinfo #t) (timeout 1000000))
   (let ((p (open-input-file name bufinfo)))
      (and (input-port? p)
	   (let ((pi (port->gzip-port p #t)))
	      (input-port-close-hook-set! pi (lambda (v) (close-input-port p)))
	      pi))))

;*---------------------------------------------------------------------*/
;*    open-input-inflate-file ...                                      */
;*---------------------------------------------------------------------*/
(define (open-input-inflate-file name #!optional (bufinfo #t) (timeout 1000000))
   (let ((p (open-input-file name bufinfo))
	 (b (get-port-buffer "open-input-deflate-file" #t c-default-io-bufsiz)))
      (and (input-port? p)
	   (let ((pi (port->inflate-port p b)))
	      (input-port-close-hook-set! pi (lambda (v) (close-input-port p)))
	      pi))))
 
;*---------------------------------------------------------------------*/
;*    port->zlib-port ...                                              */
;*---------------------------------------------------------------------*/
(define (port->zlib-port::input-port in::input-port #!optional (bufinfo #t))
   (let* ((cmf (read-byte in))
	  (flg (read-byte in))
	  (cm (bit-and cmf #b1111))
	  (cinfo (bit-rsh cmf 4))
	  (fcheck (bit-and flg #b1111))
	  (fdict (bit-rsh (bit-and flg #b10000) 5))
	  (flevel (bit-rsh flg 6)))
      (cond
	 ((not (=fx cm 8))
	  (error "port->zlib-port" "Unsupported format" cm))
	 ((not (=fx (remainder (+fx (*fx cmf 256) flg) 31) 0))
	  (error "port->zlib-port" "Illegal fcheck" fcheck))
	 ((=fx fdict 0)
	  (port->port in 'port->inflate-port #t (bit-lsh 1 (+fx 8 cinfo))
		      check-adler32))
	 (else
	  (let ((dict (read-chars 4 in)))
	     (port->port in 'port->inflate-port #t (bit-lsh 1 (+fx 8 cinfo)) #f))))))
      
;*---------------------------------------------------------------------*/
;*    open-input-zlib-file ...                                         */
;*---------------------------------------------------------------------*/
(define (open-input-zlib-file name #!optional (bufinfo #t) (timeout 1000000))
   (let ((p (open-input-file name bufinfo)))
      (and (input-port? p)
	   (let ((pi (port->zlib-port p)))
	      (input-port-close-hook-set! pi (lambda (v) (close-input-port p)))
	      pi))))

;*---------------------------------------------------------------------*/
;*    check-adler32 ...                                                */
;*    -------------------------------------------------------------    */
;*    This procedure is not implemented yet. It should compute         */
;*    the ADLER32 checksum of the buffer and test it against the       */
;*    read long value.                                                 */
;*---------------------------------------------------------------------*/
(define (check-adler32 in::input-port buf::bstring)
   (read-byte in)
   (read-byte in)
   (read-byte in)
   (read-byte in))
