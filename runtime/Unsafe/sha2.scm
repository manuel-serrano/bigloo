;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/sha2.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Wayne Richards and Manuel Serrano                 */
;*    Creation    :  Mon May 26 08:40:27 2008                          */
;*    Last change :  Fri Sep 22 08:32:56 2017 (serrano)                */
;*    Copyright   :  2008-21 Wayne Richards, Manuel Serrano            */
;*    -------------------------------------------------------------    */
;*    SHA-256 Bigloo implementation                                    */
;*=====================================================================*/

;; This code has been inspired by a C implementation written by
;; Aaron D. Gifford and distributed under the following copyright:
;; 
;; FILE:	sha2.c
;; AUTHOR:	Aaron D. Gifford <me@aarongifford.com>
;; 
;; Copyright (c) 2000-2001, Aaron D. Gifford
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the copyright holder nor the names of contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTOR(S) ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTOR(S) BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.
;;
;; $Id: sha2.c,v 1.1 2001/11/08 00:01:51 adg Exp adg $

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sha2
   
   (use    __type
	   __bigloo
	   __bexit
	   __object
	   __thread
	   __rgc
	   __bit
	   __bignum
	   __r4_numbers_6_5
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_numbers_6_5_flonum_dtoa
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
	   __structure)
   
   (import __param
	   __hmac
	   __tvector)

   ;; >>> TBR when debug done
   (import __r4_output_6_10_3
            
            __r4_numbers_6_5_fixnum
            __r4_numbers_6_5_flonum
            __r4_numbers_6_5
            __r4_equivalence_6_2
            __r4_vectors_6_8
            __r4_booleans_6_1
            __r4_characters_6_6
            __r4_symbols_6_4
            __r4_pairs_and_lists_6_3
            __r4_strings_6_7
            __r4_ports_6_10_1
            __r4_control_features_6_9)
   ;; <<< TBR when debug done

   (from   __srfi4)
   
   (export (sha256sum::bstring ::obj)
	   (sha256sum-string::bstring ::bstring)
	   (sha256sum-mmap::bstring ::mmap)
	   (sha256sum-port::bstring ::input-port)
	   (sha256sum-file::bstring ::bstring)
	   
	   (sha512sum::bstring ::obj)
	   (sha512sum-string::bstring ::bstring)
	   (sha512sum-mmap::bstring ::mmap)
	   (sha512sum-port::bstring ::input-port)
	   (sha512sum-file::bstring ::bstring)
	   
	   (hmac-sha256sum-string::bstring ::bstring ::bstring)
	   (hmac-sha512sum-string::bstring ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    u32 ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (u32 hi lo)
   (if (and (fixnum? hi) (fixnum? lo))
       (bit-oru32 (bit-lshu32 (fixnum->uint32 hi) 16) (fixnum->uint32 lo))
       `(bit-oru32 (bit-lshu32 (fixnum->uint32 ,hi) 16) (fixnum->uint32 ,lo))))

;*---------------------------------------------------------------------*/
;*    u64 ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (u64 w0 w1 w2 w3)
   (if (and (fixnum? w0) (fixnum? w1) (fixnum? w2) (fixnum? w3))
       (bit-oru64 (bit-lshu64 (fixnum->uint64 w0) 48)
	  (bit-oru64 (bit-lshu64 (fixnum->uint64 w1) 32)
	     (bit-oru64 (bit-lshu64 (fixnum->uint64 w2) 16)
		(fixnum->uint64 w3))))
       `(bit-oru64 (bit-lshu64 (fixnum->uint64 ,w0) 48)
	   (bit-oru64 (bit-lshu64 (fixnum->uint64 ,w1) 32)
	      (bit-oru64 (bit-lshu64 (fixnum->uint64 ,w2) 16)
		 (fixnum->uint64 ,w3))))))

;*---------------------------------------------------------------------*/
;*    u16 ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (u16 hi lo)
   `(bit-or (uint32->fixnum (bit-lshu32 ,hi 8)) (uint32->fixnum ,lo)))

;*---------------------------------------------------------------------*/
;*    u32-hi ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (u32-hi w)
   `(uint32->fixnum (bit-urshu32 ,w 16)))

;*---------------------------------------------------------------------*/
;*    u32-low ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (u32-lo w)
   `(uint32->fixnum (bit-andu32 ,w (-u32 (bit-lshu32 #u32:1 16) 1))))

;*---------------------------------------------------------------------*/
;*    u64-wXX ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (u64-w3 w)
   `(uint32->fixnum
       (u32-hi
	  (uint64->uint32 (bit-urshu64 w 32)))))
(define-macro (u64-w2 w)
   `(uint32->fixnum
       (u32-lo
	  (uint64->uint32 (bit-urshu64 w 32)))))
(define-macro (u64-w1 w)
   `(uint32->fixnum
       (u32-hi
	  (uint64->uint32 (bit-andu64 w (-u64 (bit-lshu64 #u64:1 32) 1))))))
(define-macro (u64-w0 w)
   `(uint32->fixnum
       (u32-lo
	  (uint64->uint32 (bit-andu64 w (-u64 (bit-lshu64 #u64:1 32) 1))))))

;*---------------------------------------------------------------------*/
;*    addu32 ...                                                       */
;*    -------------------------------------------------------------    */
;*    unsigned binary addition                                         */
;*---------------------------------------------------------------------*/
(define (addu32::uint32 n1::uint32 n2::uint32)
   (let* ((h1::uint32 (bit-urshu32 n1 16))
	  (h2::uint32 (bit-urshu32 n2 16))
	  (h::uint32 (bit-andu32 (fixnum->uint32 #xffff) (+u32 h1 h2)))
	  (l1::uint32 (bit-andu32 n1 (fixnum->uint32 #xffff)))
	  (l2::uint32 (bit-andu32 n2 (fixnum->uint32 #xffff)))
	  (l::uint32 (+u32 l1 l2))
	  (lh::uint32 (bit-lshu32 h 16)))
      (+u32 lh l)))
   
;*---------------------------------------------------------------------*/
;*    u32+ ...                                                         */
;*---------------------------------------------------------------------*/
(define-expander u32+
   (lambda (x e)
      (match-case x
	 (()
	  #u32:0)
	 ((?- ?a)<
	  (e a e))
	 ((?- ?a . ?b)
	  (e `(addu32 ,a (u32+ ,@b)) e)))))

;*---------------------------------------------------------------------*/
;*    u64+ ...                                                         */
;*---------------------------------------------------------------------*/
(define-expander u64+
   (lambda (x e)
      (match-case x
	 (()
	  #u64:0)
	 ((?- ?a)
	  (e a e))
	 ((?- ?a . ?b)
	  (e `(+u64 ,a (u64+ ,@b)) e)))))

;*---------------------------------------------------------------------*/
;*    rotr32 ...                                                       */
;*---------------------------------------------------------------------*/
(define (rotr32::uint32 x::uint32 n::int)
   (bit-oru32 (bit-urshu32 x n) (bit-lshu32 x (-fx 32 n))))

;*---------------------------------------------------------------------*/
;*    rotr64 ...                                                       */
;*---------------------------------------------------------------------*/
(define (rotr64::uint64 x::uint64 n::int)
   (bit-oru64 (bit-urshu64 x n) (bit-lshu64 x (-fx 64 n))))

;*---------------------------------------------------------------------*/
;*    u32-fill! ...                                                    */
;*---------------------------------------------------------------------*/
(define (u32-fill! str offset w::uint32)
   (let* ((s1 (integer->string (u32-hi w) 16))
          (l1 (string-length s1))
          (s2 (integer->string (u32-lo w) 16))
          (l2 (string-length s2)))
      (blit-string! s1 0 str (+fx offset (-fx 4 l1)) l1)
      (blit-string! s2 0 str (+fx offset (+fx 4 (-fx 4 l2))) l2)))

;*---------------------------------------------------------------------*/
;*    u64-fill! ...                                                    */
;*---------------------------------------------------------------------*/
(define (u64-fill! str offset w::uint64)
   (let* ((s1 (integer->string (u64-w3 w) 16))
          (l1 (string-length s1))
          (s2 (integer->string (u64-w2 w) 16))
          (l2 (string-length s2))
          (s3 (integer->string (u64-w1 w) 16))
          (l3 (string-length s3))
          (s4 (integer->string (u64-w0 w) 16))
          (l4 (string-length s4)))
      (blit-string! s1 0 str (+fx offset (-fx 4 l1)) l1)
      (blit-string! s2 0 str (+fx offset (+fx 4 (-fx 4 l2))) l2)
      (blit-string! s3 0 str (+fx offset (+fx 8 (-fx 4 l3))) l3)
      (blit-string! s4 0 str (+fx offset (+fx 12 (-fx 4 l4))) l4)))

;*---------------------------------------------------------------------*/
;*    state->string ...                                                */
;*---------------------------------------------------------------------*/
(define (state->string state::u32vector)
   (let ((r (make-string 64 #\0)))
      (u32-fill! r 0 (u32vector-ref state 0))
      (u32-fill! r 8 (u32vector-ref state 1))
      (u32-fill! r 16 (u32vector-ref state 2))
      (u32-fill! r 24 (u32vector-ref state 3))
      (u32-fill! r 32 (u32vector-ref state 4))
      (u32-fill! r 40 (u32vector-ref state 5))
      (u32-fill! r 48 (u32vector-ref state 6))
      (u32-fill! r 56 (u32vector-ref state 7))
      r))

;*---------------------------------------------------------------------*/
;*    state64->string ...                                              */
;*---------------------------------------------------------------------*/
(define (state64->string state::u64vector)
   (let ((r (make-string 128 #\0)))
      (u64-fill! r 0 (u64vector-ref state 0))
      (u64-fill! r 16 (u64vector-ref state 1))
      (u64-fill! r 32 (u64vector-ref state 2))
      (u64-fill! r 48 (u64vector-ref state 3))
      (u64-fill! r 64 (u64vector-ref state 4))
      (u64-fill! r 80 (u64vector-ref state 5))
      (u64-fill! r 96 (u64vector-ref state 6))
      (u64-fill! r 112 (u64vector-ref state 7))
      r))

;*---------------------------------------------------------------------*/
;*    bit-or* ...                                                      */
;*---------------------------------------------------------------------*/
(define-expander bit-or*
   (lambda (x e)
      (match-case x
	 ((?- ?a ?b)
	  (e `(bit-or ,a ,b) e))
	 ((?- ?a . ?b)
	  (e `(bit-or ,a (bit-or* ,@b)) e)))))
	  
;*---------------------------------------------------------------------*/
;*    bit-xor* ...                                                     */
;*---------------------------------------------------------------------*/
(define-expander bit-xor*
   (lambda (x e)
      (match-case x
	 ((?- ?a ?b)
	  (e `(bit-xoru32 ,a ,b) e))
	 ((?- ?a . ?b)
	  (e `(bit-xoru32 ,a (bit-xor* ,@b)) e)))))

;*---------------------------------------------------------------------*/
;*    bit-or64* ...                                                    */
;*---------------------------------------------------------------------*/
(define-expander bit-or64*
   (lambda (x e)
      (match-case x
	 ((?- ?a ?b)
	  (e `(bit-or64 ,a ,b) e))
	 ((?- ?a . ?b)
	  (e `(bit-or64 ,a (bit-or64* ,@b)) e)))))
	  
;*---------------------------------------------------------------------*/
;*    bit-xor64* ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander bit-xor64*
   (lambda (x e)
      (match-case x
	 ((?- ?a ?b)
	  (e `(bit-xoru64 ,a ,b) e))
	 ((?- ?a . ?b)
	  (e `(bit-xoru64 ,a (bit-xor64* ,@b)) e)))))

;*---------------------------------------------------------------------*/
;*    K256 ...                                                         */
;*---------------------------------------------------------------------*/
(define K256
   (let ((v (make-u32vector 64)))
      (u32vector-set! v 0 (u32 #x428a #x2f98))
      (u32vector-set! v 1 (u32 #x7137 #x4491))
      (u32vector-set! v 2 (u32 #xb5c0 #xfbcf))
      (u32vector-set! v 3 (u32 #xe9b5 #xdba5))
      (u32vector-set! v 4 (u32 #x3956 #xc25b))
      (u32vector-set! v 5 (u32 #x59f1 #x11f1))
      (u32vector-set! v 6 (u32 #x923f #x82a4))
      (u32vector-set! v 7 (u32 #xab1c #x5ed5))
      (u32vector-set! v 8 (u32 #xd807 #xaa98))
      (u32vector-set! v 9 (u32 #x1283 #x5b01))
      (u32vector-set! v 10 (u32 #x2431 #x85be))
      (u32vector-set! v 11 (u32 #x550c #x7dc3))
      (u32vector-set! v 12 (u32 #x72be #x5d74))
      (u32vector-set! v 13 (u32 #x80de #xb1fe))
      (u32vector-set! v 14 (u32 #x9bdc #x06a7))
      (u32vector-set! v 15 (u32 #xc19b #xf174))
      (u32vector-set! v 16 (u32 #xe49b #x69c1))
      (u32vector-set! v 17 (u32 #xefbe #x4786))
      (u32vector-set! v 18 (u32 #x0fc1 #x9dc6))
      (u32vector-set! v 19 (u32 #x240c #xa1cc))
      (u32vector-set! v 20 (u32 #x2de9 #x2c6f))
      (u32vector-set! v 21 (u32 #x4a74 #x84aa))
      (u32vector-set! v 22 (u32 #x5cb0 #xa9dc))
      (u32vector-set! v 23 (u32 #x76f9 #x88da))
      (u32vector-set! v 24 (u32 #x983e #x5152))
      (u32vector-set! v 25 (u32 #xa831 #xc66d))
      (u32vector-set! v 26 (u32 #xb003 #x27c8))
      (u32vector-set! v 27 (u32 #xbf59 #x7fc7))
      (u32vector-set! v 28 (u32 #xc6e0 #x0bf3))
      (u32vector-set! v 29 (u32 #xd5a7 #x9147))
      (u32vector-set! v 30 (u32 #x06ca #x6351))
      (u32vector-set! v 31 (u32 #x1429 #x2967))
      (u32vector-set! v 32 (u32 #x27b7 #x0a85))
      (u32vector-set! v 33 (u32 #x2e1b #x2138))
      (u32vector-set! v 34 (u32 #x4d2c #x6dfc))
      (u32vector-set! v 35 (u32 #x5338 #x0d13))
      (u32vector-set! v 36 (u32 #x650a #x7354))
      (u32vector-set! v 37 (u32 #x766a #x0abb))
      (u32vector-set! v 38 (u32 #x81c2 #xc92e))
      (u32vector-set! v 39 (u32 #x9272 #x2c85))
      (u32vector-set! v 40 (u32 #xa2bf #xe8a1))
      (u32vector-set! v 41 (u32 #xa81a #x664b))
      (u32vector-set! v 42 (u32 #xc24b #x8b70))
      (u32vector-set! v 43 (u32 #xc76c #x51a3))
      (u32vector-set! v 44 (u32 #xd192 #xe819))
      (u32vector-set! v 45 (u32 #xd699 #x0624))
      (u32vector-set! v 46 (u32 #xf40e #x3585))
      (u32vector-set! v 47 (u32 #x106a #xa070))
      (u32vector-set! v 48 (u32 #x19a4 #xc116))
      (u32vector-set! v 49 (u32 #x1e37 #x6c08))
      (u32vector-set! v 50 (u32 #x2748 #x774c))
      (u32vector-set! v 51 (u32 #x34b0 #xbcb5))
      (u32vector-set! v 52 (u32 #x391c #x0cb3))
      (u32vector-set! v 53 (u32 #x4ed8 #xaa4a))
      (u32vector-set! v 54 (u32 #x5b9c #xca4f))
      (u32vector-set! v 55 (u32 #x682e #x6ff3))
      (u32vector-set! v 56 (u32 #x748f #x82ee))
      (u32vector-set! v 57 (u32 #x78a5 #x636f))
      (u32vector-set! v 58 (u32 #x84c8 #x7814))
      (u32vector-set! v 59 (u32 #x8cc7 #x0208))
      (u32vector-set! v 60 (u32 #x90be #xfffa))
      (u32vector-set! v 61 (u32 #xa450 #x6ceb))
      (u32vector-set! v 62 (u32 #xbef9 #xa3f7))
      (u32vector-set! v 63 (u32 #xc671 #x78f2))
      v))

;*---------------------------------------------------------------------*/
;*    sha256-initial-hash-value ...                                    */
;*---------------------------------------------------------------------*/
(define (sha256-initial-hash-value)
   (let ((v (make-u32vector 8)))
      (u32vector-set! v 0 (u32 #x6a09 #xe667))
      (u32vector-set! v 1 (u32 #xbb67 #xae85))
      (u32vector-set! v 2 (u32 #x3c6e #xf372))
      (u32vector-set! v 3 (u32 #xa54f #xf53a))
      (u32vector-set! v 4 (u32 #x510e #x527f))
      (u32vector-set! v 5 (u32 #x9b05 #x688c))
      (u32vector-set! v 6 (u32 #x1f83 #xd9ab))
      (u32vector-set! v 7 (u32 #x5be0 #xcd19))
      v))

;*---------------------------------------------------------------------*/
;*    sha512-initial-hash-value ...                                    */
;*---------------------------------------------------------------------*/
(define (sha512-initial-hash-value)
   (let ((v (make-u64vector 8)))
      (u64vector-set! v 0 (u64 #x6a09 #xe667 #xf3bc #xc908))
      (u64vector-set! v 1 (u64 #xbb67 #xae85 #x84ca #xa73b))
      (u64vector-set! v 2 (u64 #x3c6e #xf372 #xfe94 #xf82b))
      (u64vector-set! v 3 (u64 #xa54f #xf53a #x5f1d #x36f1))
      (u64vector-set! v 4 (u64 #x510e #x527f #xade6 #x82d1))
      (u64vector-set! v 5 (u64 #x9b05 #x688c #x2b3e #x6c1f))
      (u64vector-set! v 6 (u64 #x1f83 #xd9ab #xfb41 #xbd6b))
      (u64vector-set! v 7 (u64 #x5be0 #xcd19 #x137e #x2179))
      v))

;*---------------------------------------------------------------------*/
;*    K512 ...                                                         */
;*---------------------------------------------------------------------*/
(define K512
   (let ((v (make-u64vector 80)))
      (u64vector-set! v 0 (u64 #x428a #x2f98 #xd728 #xae22))
      (u64vector-set! v 1 (u64 #x7137 #x4491 #x23ef #x65cd))
      (u64vector-set! v 2 (u64 #xb5c0 #xfbcf #xec4d #x3b2f))
      (u64vector-set! v 3 (u64 #xe9b5 #xdba5 #x8189 #xdbbc))
      (u64vector-set! v 4 (u64 #x3956 #xc25b #xf348 #xb538))
      (u64vector-set! v 5 (u64 #x59f1 #x11f1 #xb605 #xd019))
      (u64vector-set! v 6 (u64 #x923f #x82a4 #xaf19 #x4f9b))
      (u64vector-set! v 7 (u64 #xab1c #x5ed5 #xda6d #x8118))
      (u64vector-set! v 8 (u64 #xd807 #xaa98 #xa303 #x0242))
      (u64vector-set! v 9 (u64 #x1283 #x5b01 #x4570 #x6fbe))
      (u64vector-set! v 10 (u64 #x2431 #x85be #x4ee4 #xb28c))
      (u64vector-set! v 11 (u64 #x550c #x7dc3 #xd5ff #xb4e2))
      (u64vector-set! v 12 (u64 #x72be #x5d74 #xf27b #x896f))
      (u64vector-set! v 13 (u64 #x80de #xb1fe #x3b16 #x96b1))
      (u64vector-set! v 14 (u64 #x9bdc #x06a7 #x25c7 #x1235))
      (u64vector-set! v 15 (u64 #xc19b #xf174 #xcf69 #x2694))
      (u64vector-set! v 16 (u64 #xe49b #x69c1 #x9ef1 #x4ad2))
      (u64vector-set! v 17 (u64 #xefbe #x4786 #x384f #x25e3))
      (u64vector-set! v 18 (u64 #x0fc1 #x9dc6 #x8b8c #xd5b5))
      (u64vector-set! v 19 (u64 #x240c #xa1cc #x77ac #x9c65))
      (u64vector-set! v 20 (u64 #x2de9 #x2c6f #x592b #x0275))
      (u64vector-set! v 21 (u64 #x4a74 #x84aa #x6ea6 #xe483))
      (u64vector-set! v 22 (u64 #x5cb0 #xa9dc #xbd41 #xfbd4))
      (u64vector-set! v 23 (u64 #x76f9 #x88da #x8311 #x53b5))
      (u64vector-set! v 24 (u64 #x983e #x5152 #xee66 #xdfab))
      (u64vector-set! v 25 (u64 #xa831 #xc66d #x2db4 #x3210))
      (u64vector-set! v 26 (u64 #xb003 #x27c8 #x98fb #x213f))
      (u64vector-set! v 27 (u64 #xbf59 #x7fc7 #xbeef #x0ee4))
      (u64vector-set! v 28 (u64 #xc6e0 #x0bf3 #x3da8 #x8fc2))
      (u64vector-set! v 29 (u64 #xd5a7 #x9147 #x930a #xa725))
      (u64vector-set! v 30 (u64 #x06ca #x6351 #xe003 #x826f))
      (u64vector-set! v 31 (u64 #x1429 #x2967 #x0a0e #x6e70))
      (u64vector-set! v 32 (u64 #x27b7 #x0a85 #x46d2 #x2ffc))
      (u64vector-set! v 33 (u64 #x2e1b #x2138 #x5c26 #xc926))
      (u64vector-set! v 34 (u64 #x4d2c #x6dfc #x5ac4 #x2aed))
      (u64vector-set! v 35 (u64 #x5338 #x0d13 #x9d95 #xb3df))
      (u64vector-set! v 36 (u64 #x650a #x7354 #x8baf #x63de))
      (u64vector-set! v 37 (u64 #x766a #x0abb #x3c77 #xb2a8))
      (u64vector-set! v 38 (u64 #x81c2 #xc92e #x47ed #xaee6))
      (u64vector-set! v 39 (u64 #x9272 #x2c85 #x1482 #x353b))
      (u64vector-set! v 40 (u64 #xa2bf #xe8a1 #x4cf1 #x0364))
      (u64vector-set! v 41 (u64 #xa81a #x664b #xbc42 #x3001))
      (u64vector-set! v 42 (u64 #xc24b #x8b70 #xd0f8 #x9791))
      (u64vector-set! v 43 (u64 #xc76c #x51a3 #x0654 #xbe30))
      (u64vector-set! v 44 (u64 #xd192 #xe819 #xd6ef #x5218))
      (u64vector-set! v 45 (u64 #xd699 #x0624 #x5565 #xa910))
      (u64vector-set! v 46 (u64 #xf40e #x3585 #x5771 #x202a))
      (u64vector-set! v 47 (u64 #x106a #xa070 #x32bb #xd1b8))
      (u64vector-set! v 48 (u64 #x19a4 #xc116 #xb8d2 #xd0c8))
      (u64vector-set! v 49 (u64 #x1e37 #x6c08 #x5141 #xab53))
      (u64vector-set! v 50 (u64 #x2748 #x774c #xdf8e #xeb99))
      (u64vector-set! v 51 (u64 #x34b0 #xbcb5 #xe19b #x48a8))
      (u64vector-set! v 52 (u64 #x391c #x0cb3 #xc5c9 #x5a63))
      (u64vector-set! v 53 (u64 #x4ed8 #xaa4a #xe341 #x8acb))
      (u64vector-set! v 54 (u64 #x5b9c #xca4f #x7763 #xe373))
      (u64vector-set! v 55 (u64 #x682e #x6ff3 #xd6b2 #xb8a3))
      (u64vector-set! v 56 (u64 #x748f #x82ee #x5def #xb2fc))
      (u64vector-set! v 57 (u64 #x78a5 #x636f #x4317 #x2f60))
      (u64vector-set! v 58 (u64 #x84c8 #x7814 #xa1f0 #xab72))
      (u64vector-set! v 59 (u64 #x8cc7 #x0208 #x1a64 #x39ec))
      (u64vector-set! v 60 (u64 #x90be #xfffa #x2363 #x1e28))
      (u64vector-set! v 61 (u64 #xa450 #x6ceb #xde82 #xbde9))
      (u64vector-set! v 62 (u64 #xbef9 #xa3f7 #xb2c6 #x7915))
      (u64vector-set! v 63 (u64 #xc671 #x78f2 #xe372 #x532b))
      (u64vector-set! v 64 (u64 #xca27 #x3ece #xea26 #x619c))
      (u64vector-set! v 65 (u64 #xd186 #xb8c7 #x21c0 #xc207))
      (u64vector-set! v 66 (u64 #xeada #x7dd6 #xcde0 #xeb1e))
      (u64vector-set! v 67 (u64 #xf57d #x4f7f #xee6e #xd178))
      (u64vector-set! v 68 (u64 #x06f0 #x67aa #x7217 #x6fba))
      (u64vector-set! v 69 (u64 #x0a63 #x7dc5 #xa2c8 #x98a6))
      (u64vector-set! v 70 (u64 #x113f #x9804 #xbef9 #x0dae))
      (u64vector-set! v 71 (u64 #x1b71 #x0b35 #x131c #x471b))
      (u64vector-set! v 72 (u64 #x28db #x77f5 #x2304 #x7d84))
      (u64vector-set! v 73 (u64 #x32ca #xab7b #x40c7 #x2493))
      (u64vector-set! v 74 (u64 #x3c9e #xbe0a #x15c9 #xbebc))
      (u64vector-set! v 75 (u64 #x431d #x67c4 #x9c10 #x0d4c))
      (u64vector-set! v 76 (u64 #x4cc5 #xd4be #xcb3e #x42b6))
      (u64vector-set! v 77 (u64 #x597f #x299c #xfc65 #x7e2a))
      (u64vector-set! v 78 (u64 #x5fcb #x6fab #x3ad6 #xfaec))
      (u64vector-set! v 79 (u64 #x6c44 #x198c #x4a47 #x5817))
      v))


;*---------------------------------------------------------------------*/
;*    Sigma0-256 ...                                                   */
;*---------------------------------------------------------------------*/
(define (Sigma0-256::uint32 x::uint32)
   (bit-xor* (rotr32 x 2) (rotr32 x 13) (rotr32 x 22)))

;*---------------------------------------------------------------------*/
;*    sigma0-256 ...                                                   */
;*---------------------------------------------------------------------*/
(define (sigma0-256::uint32 x::uint32)
   (bit-xor* (rotr32 x 7) (rotr32 x 18) (bit-urshu32 x 3)))

;*---------------------------------------------------------------------*/
;*    Sigma1-256 ...                                                   */
;*---------------------------------------------------------------------*/
(define (Sigma1-256::uint32 x::uint32)
   (bit-xor* (rotr32 x 6) (rotr32 x 11) (rotr32 x 25)))

;*---------------------------------------------------------------------*/
;*    sigma1-256 ...                                                   */
;*---------------------------------------------------------------------*/
(define (sigma1-256::uint32 x::uint32)
   (bit-xor* (rotr32 x 17) (rotr32 x 19) (bit-urshu32 x 10)))

;*---------------------------------------------------------------------*/
;*    Sigma0-512 ...                                                   */
;*---------------------------------------------------------------------*/
(define (Sigma0-512::uint64 x::uint64)
   (bit-xor64* (rotr64 x 28) (rotr64 x 34) (rotr64 x 39)))

;*---------------------------------------------------------------------*/
;*    Sigma1-512 ...                                                   */
;*---------------------------------------------------------------------*/
(define (Sigma1-512::uint64 x::uint64)
   (bit-xor64* (rotr64 x 14) (rotr64 x 18) (rotr64 x 41)))

;*---------------------------------------------------------------------*/
;*    sigma0-512 ...                                                   */
;*---------------------------------------------------------------------*/
(define (sigma0-512::uint64 x::uint64)
   (bit-xor64* (rotr64 x 1) (rotr64 x 8) (bit-urshu64 x 7)))

;*---------------------------------------------------------------------*/
;*    sigma1-512 ...                                                   */
;*---------------------------------------------------------------------*/
(define (sigma1-512::uint64 x::uint64)
   (bit-xor64* (rotr64 x 19) (rotr64 x 61) (bit-urshu64 x 6)))

;*---------------------------------------------------------------------*/
;*    get-a/b/c/d/e/f/g/h ...                                          */
;*---------------------------------------------------------------------*/
(define (get-a::uint32 st) (u32vector-ref st 0))
(define (get-b::uint32 st) (u32vector-ref st 1))
(define (get-c::uint32 st) (u32vector-ref st 2))
(define (get-d::uint32 st) (u32vector-ref st 3))
(define (get-e::uint32 st) (u32vector-ref st 4))
(define (get-f::uint32 st) (u32vector-ref st 5))
(define (get-g::uint32 st) (u32vector-ref st 6))
(define (get-h::uint32 st) (u32vector-ref st 7))

;*---------------------------------------------------------------------*/
;*    sha256-internal-transform ...                                    */
;*---------------------------------------------------------------------*/
(define (sha256-internal-transform state::u32vector buffer::u32vector)
   
   (define (Maj::uint32 x::uint32 y::uint32 z::uint32)
      (bit-xor* (bit-andu32 x y) (bit-andu32 x z) (bit-andu32 y z)))

   (define (Ch::uint32 x::uint32 y::uint32 z::uint32)
      (bit-xoru32 (bit-andu32 x y) (bit-andu32 (bit-notu32 x) z)))

   (define (compress e f g h w::uint32 j)
      (u32+ h (Sigma1-256 e) (Ch e f g) (u32vector-ref K256 j) w))
   
   (define (xf-ref::uint32 v::u32vector n::long)
      (u32vector-ref v (bit-and n #xf)))
   
   (define (set-state! state
		       a::uint32 b::uint32 c::uint32 d::uint32
		       e::uint32 f::uint32 g::uint32 h::uint32)
      (let ((oa::uint32 (get-a state))
	    (ob::uint32 (get-b state))
	    (oc::uint32 (get-c state))
	    (od::uint32 (get-d state))
	    (oe::uint32 (get-e state))
	    (of::uint32 (get-f state))
	    (og::uint32 (get-g state))
	    (oh::uint32 (get-h state))
	    (oj::long 0))
	 (u32vector-set! state 0 (u32+ oa a))
	 (u32vector-set! state 1 (u32+ ob b))
	 (u32vector-set! state 2 (u32+ oc c))
	 (u32vector-set! state 3 (u32+ od d))
	 (u32vector-set! state 4 (u32+ oe e))
	 (u32vector-set! state 5 (u32+ of f))
	 (u32vector-set! state 6 (u32+ og g))
	 (u32vector-set! state 7 (u32+ oh h))))
   
   (let loop ((a::uint32 (get-a state))
	      (b::uint32 (get-b state))
	      (c::uint32 (get-c state))
	      (d::uint32 (get-d state))
	      (e::uint32 (get-e state))
	      (f::uint32 (get-f state))
	      (g::uint32 (get-g state))
	      (h::uint32 (get-h state))
	      (j::long 0))
      (cond
	 ((<fx j 16)
	  (let* ((w::uint32 (u32vector-ref buffer j))
		 (T1::uint32 (compress e f g h w j))
		 (T2::uint32 (u32+ (Sigma0-256 a) (Maj a b c))))
	     (loop (u32+ T1 T2) a b c (u32+ d T1) e f g (+ j 1))))
	 ((<fx j 64)
	  (let* ((s0 (sigma0-256 (xf-ref buffer (+ j 1))))
		 (s1 (sigma1-256 (xf-ref buffer (+ j 14))))
		 (ndx (bit-and j #lxF))
		 (w::uint32 (u32+ (xf-ref buffer j) s1 (xf-ref buffer (+fx j 9)) s0))
		 (T1::uint32 (compress e f g h w j))
		 (T2::uint32 (u32+ (Sigma0-256 a) (Maj a b c))))
	     (u32vector-set! buffer ndx w)
	     (loop (u32+ T1 T2) a b c (u32+ d T1) e f g (+ j 1))))
	 (else
	  (set-state! state a b c d e f g h)
	  state))))

;*---------------------------------------------------------------------*/
;*    sha256-update ...                                                */
;*---------------------------------------------------------------------*/
(define (sha256-update state::u32vector buffer::u32vector
		       o::obj fill-word!::procedure)
   
   (define (fill-buffer! buffer i)
      ;; fills 16 words of 4 bytes, returns the number of read bytes
      (let loop ((j 0)
		 (i i)
		 (n 0))
	 (if (<fx j 16)
	     (loop (+fx j 1) (+fx i 4) (+ n (fill-word! buffer j o i)))
	     n)))

   (define (u32vector-fill! v len val)
      (let loop ((i 0))
	 (when (<fx i len)
	    (u32vector-set! v i val)
	    (loop (+fx i 1)))))
   
   (let loop ((i 0)
	      (l 0))
      (let ((bytes (fill-buffer! buffer i)))
	 (cond
	    ((=fx bytes 64)
	     ;; a full buffer
	     (sha256-internal-transform state buffer)
	     (loop (+fx i 64) (+fx l 64)))
	    ((>=fx (-fx 64 bytes) 8)
	     ;; we have room for the length of the message. The length is
	     ;; a 64 bits integer but we are using here 32bits values
	     (let ((ulen::uint32 (*fx 8 (+fx (-fx l 1) bytes))))
		(u32vector-set! buffer 15 ulen))
	     (sha256-internal-transform state buffer))
	    (else
	     ;; we don't have space for the length
	     (sha256-internal-transform state buffer)
	     (u32vector-fill! buffer 15 #u32:0)
	     (let ((ulen::uint32 (*fx 8 (+fx (-fx l 1) bytes))))
		(u32vector-set! buffer 15 ulen))
	     (sha256-internal-transform state buffer))))))

;*---------------------------------------------------------------------*/
;*    sha256sum-mmap ...                                               */
;*---------------------------------------------------------------------*/
(define (sha256sum-mmap mm)
   
   (define (u32mmap-ref::uint32 mm::mmap i::long)
      (char->integer ($mmap-ref mm i)))
   
   (define (fill-word32-mmap! v32::u32vector i::long mm::mmap n::long)
      (let ((l (mmap-length mm)))
	 (cond
	    ((<=fx (+fx n 4) l)
	     (let* ((v0::uint32 (u32mmap-ref mm n))
		    (v1::uint32 (u32mmap-ref mm (+fx n 1)))
		    (v2::uint32 (u32mmap-ref mm (+fx n 2)))
		    (v3::uint32 (u32mmap-ref mm (+fx n 3)))
		    (v::uint32 (u32 (u16 v0 v1) (u16 v2 v3))))
		(u32vector-set! v32 i v)
		4))
	    ((>=fx n (+fx 1 l))
	     (u32vector-set! v32 i 0)
	     0)
	    (else
	     (let ((v (make-u32vector 4 0))
		   (k (-fx 4 (-fx (+fx n 4) l))))
		(let loop ((j 0))
		   (if (=fx j k)
		       (begin
			  (u32vector-set! v j #x80)
			  (let* ((v0::uint32 (u32vector-ref v 0))
				 (v1::uint32 (u32vector-ref v 1))
				 (v2::uint32 (u32vector-ref v 2))
				 (v3::uint32 (u32vector-ref v 3))
				 (v::uint32 (u32 (u16 v0 v1) (u16 v2 v3))))
			     (u32vector-set! v32 i v)
			     (+fx j 1)))
		       (begin
			  (u32vector-set! v j (u32mmap-ref mm (+ n j)))
			  (loop (+fx j 1))))))))))
   
   (let ((state (sha256-initial-hash-value))
	 (buffer (make-u32vector 16)))
      (sha256-update state buffer mm fill-word32-mmap!)
      (state->string state)))

;*---------------------------------------------------------------------*/
;*    sha256sum-string ...                                             */
;*---------------------------------------------------------------------*/
(define (sha256sum-string str)
   
   (define (u32string-ref::uint32 str i)
      (char->integer (string-ref str i)))

   (define (fill-word32-string! v32::u32vector i::long str::bstring n::long)
      (let ((l (string-length str)))
	 (cond
	    ((<=fx (+fx n 4) l)
	     (let* ((v0::uint32 (u32string-ref str n))
		    (v1::uint32 (u32string-ref str (+fx n 1)))
		    (v2::uint32 (u32string-ref str (+fx n 2)))
		    (v3::uint32 (u32string-ref str (+fx n 3)))
		    (v::uint32 (u32 (u16 v0 v1) (u16 v2 v3))))
		(u32vector-set! v32 i v)
		4))
	    ((>=fx n (+fx 1 l))
	     (u32vector-set! v32 i 0)
	     0)
	    (else
	     (let ((v (make-u32vector 4 0))
		   (k (-fx 4 (-fx (+fx n 4) l))))
		(let loop ((j 0))
		   (if (=fx j k)
		       (begin
			  (u32vector-set! v j #x80)
			  (let* ((v0::uint32 (u32vector-ref v 0))
				 (v1::uint32 (u32vector-ref v 1))
				 (v2::uint32 (u32vector-ref v 2))
				 (v3::uint32 (u32vector-ref v 3))
				 (v::uint32 (u32 (u16 v0 v1) (u16 v2 v3))))
			     (u32vector-set! v32 i v)
			     (+fx j 1)))
		       (begin
			  (u32vector-set! v j (u32string-ref str (+ n j)))
			  (loop (+fx j 1))))))))))
   
   (let ((state (sha256-initial-hash-value))
	 (buffer (make-u32vector 16)))
      (sha256-update state buffer str fill-word32-string!)
      (state->string state)))

;*---------------------------------------------------------------------*/
;*    sha256sum-port ...                                               */
;*---------------------------------------------------------------------*/
(define (sha256sum-port p)

   (define buf (make-u32vector 4))

   (define len 0)

   (define (read-word! p::input-port)
      (let loop ((i 0))
	 (if (=fx i 4)
	     i
	     (let ((c (read-byte p)))
		(if (eof-object? c)
		    (let liip ((j i))
		       (if (=fx j 4)
			   i
			   (begin
			      (u32vector-set! buf j 0)
			      (liip (+fx j 1)))))
		    (begin
		       (u32vector-set! buf i ($byte->uint32 c))
		       (loop (+fx i 1))))))))

   (define (fill-word32-port! v32::u32vector i::long p::input-port n::long)
      (let ((l (read-word! p)))
	 (set! len (+fx len l))
	 (cond
	    ((<=fx (+fx n 4) len)
	     (let* ((v0::uint32 (u32vector-ref buf 0))
		    (v1::uint32 (u32vector-ref buf 1))
		    (v2::uint32 (u32vector-ref buf 2))
		    (v3::uint32 (u32vector-ref buf 3))
		    (v::uint32 (u32 (u16 v0 v1) (u16 v2 v3))))
		(u32vector-set! v32 i v)
		4))
	    ((>=fx n (+fx 1 len))
	     (u32vector-set! v32 i 0)
	     0)
	    (else
	     (let ((v (make-u32vector 4 0))
		   (k (-fx 4 (-fx (+fx n 4) len))))
		(let loop ((j 0))
		   (if (=fx j k)
		       (begin
			  (u32vector-set! v j #x80)
			  (let* ((v0::uint32 (u32vector-ref v 0))
				 (v1::uint32 (u32vector-ref v 1))
				 (v2::uint32 (u32vector-ref v 2))
				 (v3::uint32 (u32vector-ref v 3))
				 (v::uint32 (u32 (u16 v0 v1) (u16 v2 v3))))
			     (u32vector-set! v32 i v)
			     (+fx j 1)))
		       (begin
			  (u32vector-set! v j (u32vector-ref buf j))
			  (loop (+fx j 1))))))))))
   
   (let ((state (sha256-initial-hash-value))
	 (buffer (make-u32vector 16)))
      (sha256-update state buffer p fill-word32-port!)
      (state->string state)))

;*---------------------------------------------------------------------*/
;*    sha256sum-file ...                                               */
;*---------------------------------------------------------------------*/
(define (sha256sum-file fname)
   (let ((mm (open-mmap fname :write #f)))
      (if (mmap? mm)
	  (unwind-protect
	     (sha256sum-mmap mm)
	     (close-mmap mm))
	  (let ((p (open-input-file fname)))
	     (unwind-protect
		(sha256sum-port p)
		(close-input-port p))))))

;*---------------------------------------------------------------------*/
;*    sha256sum ...                                                    */
;*---------------------------------------------------------------------*/
(define (sha256sum obj)
   (cond
      ((mmap? obj)
       (sha256sum-mmap obj))
      ((string? obj)
       (sha256sum-string obj))
      ((input-port? obj)
       (sha256sum-port obj))
      (else
       (error "sha256sum" "Illegal argument" obj))))

;*---------------------------------------------------------------------*/
;*    get64-a/b/c/d/e/f/g/h ...                                        */
;*---------------------------------------------------------------------*/
(define (get64-a::uint64 st) (u64vector-ref st 0))
(define (get64-b::uint64 st) (u64vector-ref st 1))
(define (get64-c::uint64 st) (u64vector-ref st 2))
(define (get64-d::uint64 st) (u64vector-ref st 3))
(define (get64-e::uint64 st) (u64vector-ref st 4))
(define (get64-f::uint64 st) (u64vector-ref st 5))
(define (get64-g::uint64 st) (u64vector-ref st 6))
(define (get64-h::uint64 st) (u64vector-ref st 7))

;*---------------------------------------------------------------------*/
;*    sha512-internal-transform ...                                    */
;*---------------------------------------------------------------------*/
(define (sha512-internal-transform state::u64vector buffer::u64vector)
   
   (define (Maj::uint64 x::uint64 y::uint64 z::uint64)
      (bit-xor64* (bit-andu64 x y) (bit-andu64 x z) (bit-andu64 y z)))

   (define (Ch::uint64 x::uint64 y::uint64 z::uint64)
      (bit-xoru64 (bit-andu64 x y) (bit-andu64 (bit-notu64 x) z)))

   (define (compress e f g h w::uint64 j)
      (u64+ h (Sigma1-512 e) (Ch e f g) (u64vector-ref K512 j) w))
   
   (define (xf-ref::uint64 v::u64vector n::long)
      (u64vector-ref v (bit-and n #xf)))
   
   (define (set-state! state
		       a::uint64 b::uint64 c::uint64 d::uint64
		       e::uint64 f::uint64 g::uint64 h::uint64)
      (let ((oa::uint64 (get64-a state))
	    (ob::uint64 (get64-b state))
	    (oc::uint64 (get64-c state))
	    (od::uint64 (get64-d state))
	    (oe::uint64 (get64-e state))
	    (of::uint64 (get64-f state))
	    (og::uint64 (get64-g state))
	    (oh::uint64 (get64-h state))
	    (oj::long 0))
	 (u64vector-set! state 0 (u64+ oa a))
	 (u64vector-set! state 1 (u64+ ob b))
	 (u64vector-set! state 2 (u64+ oc c))
	 (u64vector-set! state 3 (u64+ od d))
	 (u64vector-set! state 4 (u64+ oe e))
	 (u64vector-set! state 5 (u64+ of f))
	 (u64vector-set! state 6 (u64+ og g))
	 (u64vector-set! state 7 (u64+ oh h))))
   
   (let loop ((a::uint64 (get64-a state))
	      (b::uint64 (get64-b state))
	      (c::uint64 (get64-c state))
	      (d::uint64 (get64-d state))
	      (e::uint64 (get64-e state))
	      (f::uint64 (get64-f state))
	      (g::uint64 (get64-g state))
	      (h::uint64 (get64-h state))
	      (j::long 0))
      (cond
	 ((<fx j 16)
	  (let* ((w::uint64 (u64vector-ref buffer j))
		 (T1::uint64 (compress e f g h w j))
		 (T2::uint64 (u64+ (Sigma0-512 a) (Maj a b c))))
	     (loop (u64+ T1 T2) a b c (u64+ d T1) e f g (+ j 1))))
	 ((<fx j 80)
	  (let* ((s0 (sigma0-512 (xf-ref buffer (+ j 1))))
		 (s1 (sigma1-512 (xf-ref buffer (+ j 14))))
		 (ndx (bit-and j #lxF))
		 (w::uint64 (u64+ (xf-ref buffer j) s1 (xf-ref buffer (+fx j 9)) s0))
		 (T1::uint64 (compress e f g h w j))
		 (T2::uint64 (u64+ (Sigma0-512 a) (Maj a b c))))
	     (u64vector-set! buffer ndx w)
	     (loop (u64+ T1 T2) a b c (u64+ d T1) e f g (+ j 1))))
	 (else
	  (set-state! state a b c d e f g h)
	  state))))

;*---------------------------------------------------------------------*/
;*    sha512-update ...                                                */
;*---------------------------------------------------------------------*/
(define (sha512-update state::u64vector buffer::u64vector
		       o::obj fill-word!::procedure)
   
   (define (fill-buffer! buffer i)
      ;; fills 16 words of 8 bytes, returns the number of read bytes
      (let loop ((j 0)
		 (i i)
		 (n 0))
	 (if (<fx j 16)
	     (loop (+fx j 1) (+fx i 8) (+ n (fill-word! buffer j o i)))
	     n)))

   (define (u64vector-fill! v len val)
      (let loop ((i 0))
	 (when (<fx i len)
	    (u64vector-set! v i val)
	    (loop (+fx i 1)))))
   
   (let loop ((i 0)
	      (l 0))
      (let ((bytes (fill-buffer! buffer i)))
	 (cond
	    ((=fx bytes 128)
	     ;; a full buffer
	     (sha512-internal-transform state buffer)
	     (loop (+fx i 128) (+fx l 128)))
	    ((>=fx (-fx 128 bytes) 8)
	     ;; we have room for the length of the message. The length is
	     ;; a 64 bits integer but we are using here 64bits values
	     (let ((ulen::uint64 (*fx 8 (+fx (-fx l 1) bytes))))
		(u64vector-set! buffer 15 ulen))
	     (sha512-internal-transform state buffer))
	    (else
	     ;; we don't have space for the length
	     (sha512-internal-transform state buffer)
	     (u64vector-fill! buffer 15 #u64:0)
	     (let ((ulen::uint64 (*fx 8 (+fx (-fx l 1) bytes))))
		(u64vector-set! buffer 15 ulen))
	     (sha512-internal-transform state buffer))))))

;*---------------------------------------------------------------------*/
;*    sha512sum-mmap ...                                               */
;*---------------------------------------------------------------------*/
(define (sha512sum-mmap mm)
   
   (define (u32mmap-ref::uint32 mm:mmap i)
      (char->integer ($mmap-ref mm i)))

   (define (fill-word64-mmap! v64::u64vector i::long mm::mmap n::long)
      (let ((l (mmap-length mm)))
	 (cond
	    ((<=fx (+fx n 8) l)
	     (let* ((v0::uint32 (u32mmap-ref mm n))
		    (v1::uint32 (u32mmap-ref mm (+fx n 1)))
		    (v2::uint32 (u32mmap-ref mm (+fx n 2)))
		    (v3::uint32 (u32mmap-ref mm (+fx n 3)))
		    (v4::uint32 (u32mmap-ref mm (+fx n 4)))
		    (v5::uint32 (u32mmap-ref mm (+fx n 5)))
		    (v6::uint32 (u32mmap-ref mm (+fx n 6)))
		    (v7::uint32 (u32mmap-ref mm (+fx n 7)))
		    (v::uint64 (u64 (u16 v0 v1) (u16 v2 v3)
				  (u16 v4 v5) (u16 v6 v7))))
		(u64vector-set! v64 i v)
		8))
	    ((>=fx n (+fx 1 l))
	     (u64vector-set! v64 i #u64:0)
	     0)
	    (else
	     (let ((v (make-u32vector 8 0))
		   (k (-fx 8 (-fx (+fx n 8) l))))
		(let loop ((j 0))
		   (if (=fx j k)
		       (begin
			  (u32vector-set! v j #x80)
			  (let* ((v0::uint32 (u32vector-ref v 0))
				 (v1::uint32 (u32vector-ref v 1))
				 (v2::uint32 (u32vector-ref v 2))
				 (v3::uint32 (u32vector-ref v 3))
				 (v4::uint32 (u32vector-ref v 4))
				 (v5::uint32 (u32vector-ref v 5))
				 (v6::uint32 (u32vector-ref v 6))
				 (v7::uint32 (u32vector-ref v 7))
				 (v::uint64 (u64 (u16 v0 v1) (u16 v2 v3)
					       (u16 v4 v5) (u16 v6 v7))))
			     (u64vector-set! v64 i v)
			     (+fx j 1)))
		       (begin
			  (u32vector-set! v j (u32mmap-ref mm (+ n j)))
			  (loop (+fx j 1))))))))))
   
   (let ((state (sha512-initial-hash-value))
	 (buffer (make-u64vector 16)))
      (sha512-update state buffer mm fill-word64-mmap!)
      (state64->string state)))

;*---------------------------------------------------------------------*/
;*    sha512sum-string ...                                             */
;*---------------------------------------------------------------------*/
(define (sha512sum-string str)
   
   (define (u32string-ref::uint32 str i)
      (char->integer (string-ref str i)))

   (define (fill-word64-string! v64::u64vector i::long str::bstring n::long)
      (let ((l (string-length str)))
	 (cond
	    ((<=fx (+fx n 8) l)
	     (let* ((v0::uint32 (u32string-ref str n))
		    (v1::uint32 (u32string-ref str (+fx n 1)))
		    (v2::uint32 (u32string-ref str (+fx n 2)))
		    (v3::uint32 (u32string-ref str (+fx n 3)))
		    (v4::uint32 (u32string-ref str (+fx n 4)))
		    (v5::uint32 (u32string-ref str (+fx n 5)))
		    (v6::uint32 (u32string-ref str (+fx n 6)))
		    (v7::uint32 (u32string-ref str (+fx n 7)))
		    (v::uint64 (u64 (u16 v0 v1) (u16 v2 v3)
				  (u16 v4 v5) (u16 v6 v7))))
		(u64vector-set! v64 i v)
		8))
	    ((>=fx n (+fx 1 l))
	     (u64vector-set! v64 i 0)
	     0)
	    (else
	     (let ((v (make-u32vector 8 0))
		   (k (-fx 8 (-fx (+fx n 8) l))))
		(let loop ((j 0))
		   (if (=fx j k)
		       (begin
			  (u32vector-set! v j #x80)
			  (let* ((v0::uint32 (u32vector-ref v 0))
				 (v1::uint32 (u32vector-ref v 1))
				 (v2::uint32 (u32vector-ref v 2))
				 (v3::uint32 (u32vector-ref v 3))
				 (v4::uint32 (u32vector-ref v 4))
				 (v5::uint32 (u32vector-ref v 5))
				 (v6::uint32 (u32vector-ref v 6))
				 (v7::uint32 (u32vector-ref v 7))
				 (v::uint64 (u64 (u16 v0 v1) (u16 v2 v3)
					       (u16 v4 v5) (u16 v6 v7))))
			     (u64vector-set! v64 i v)
			     (+fx j 1)))
		       (begin
			  (u32vector-set! v j (u32string-ref str (+ n j)))
			  (loop (+fx j 1))))))))))
   
   (let ((state (sha512-initial-hash-value))
	 (buffer (make-u64vector 16)))
      (sha512-update state buffer str fill-word64-string!)
      (state64->string state)))

;*---------------------------------------------------------------------*/
;*    sha512sum-port ...                                               */
;*---------------------------------------------------------------------*/
(define (sha512sum-port p)

   (define buf (make-u32vector 8))

   (define len 0)

   (define (read-word! p::input-port)
      (let loop ((i 0))
	 (if (=fx i 4)
	     i
	     (let ((c (read-byte p)))
		(if (eof-object? c)
		    (let liip ((j i))
		       (if (=fx j 4)
			   i
			   (begin
			      (u32vector-set! buf j 0)
			      (liip (+fx j 1)))))
		    (begin
		       (u32vector-set! buf i ($byte->uint32 c))
		       (loop (+fx i 1))))))))

   (define (fill-word64-port! v64::u64vector i::long p::input-port n::long)
      (let ((l (read-word! p)))
	 (set! len (+fx len l))
	 (cond
	    ((<=fx (+fx n 8) l)
	     (let* ((v0::uint32 (u32vector-ref buf 0))
		    (v1::uint32 (u32vector-ref buf 1))
		    (v2::uint32 (u32vector-ref buf 2))
		    (v3::uint32 (u32vector-ref buf 3))
		    (v4::uint32 (u32vector-ref buf 4))
		    (v5::uint32 (u32vector-ref buf 5))
		    (v6::uint32 (u32vector-ref buf 6))
		    (v7::uint32 (u32vector-ref buf 7))
		    (v::uint64 (u64 (u16 v0 v1) (u16 v2 v3)
				  (u16 v4 v5) (u16 v6 v7))))
		(u64vector-set! v64 i v)
		8))
	    ((>=fx n (+fx 1 l))
	     (u64vector-set! v64 i 0)
	     0)
	    (else
	     (let ((v (make-u32vector 8 0))
		   (k (-fx 8 (-fx (+fx n 8) l))))
		(let loop ((j 0))
		   (if (=fx j k)
		       (begin
			  (u32vector-set! v j #x80)
			  (let* ((v0::uint32 (u32vector-ref v 0))
				 (v1::uint32 (u32vector-ref v 1))
				 (v2::uint32 (u32vector-ref v 2))
				 (v3::uint32 (u32vector-ref v 3))
				 (v4::uint32 (u32vector-ref v 4))
				 (v5::uint32 (u32vector-ref v 5))
				 (v6::uint32 (u32vector-ref v 6))
				 (v7::uint32 (u32vector-ref v 7))
				 (v::uint64 (u64 (u16 v0 v1) (u16 v2 v3)
					       (u16 v4 v5) (u16 v6 v7))))
			     (u64vector-set! v64 i v)
			     (+fx j 1)))
		       (begin
			  (u32vector-set! v j (u32vector-ref buf j))
			  (loop (+fx j 1))))))))))
   
   (let ((state (sha512-initial-hash-value))
	 (buffer (make-u64vector 16)))
      (sha512-update state buffer p fill-word64-port!)
      (state64->string state)))

;*---------------------------------------------------------------------*/
;*    sha512sum-file ...                                               */
;*---------------------------------------------------------------------*/
(define (sha512sum-file fname)
   (let ((mm (open-mmap fname :write #f)))
      (if (mmap? mm)
	  (unwind-protect
	     (sha512sum-mmap mm)
	     (close-mmap mm))
	  (let ((p (open-input-file fname)))
	     (unwind-protect
		(sha512sum-port p)
		(close-input-port p))))))

;*---------------------------------------------------------------------*/
;*    sha512sum ...                                                    */
;*---------------------------------------------------------------------*/
(define (sha512sum obj)
   (cond
      ((mmap? obj)
       (sha512sum-mmap obj))
      ((string? obj)
       (sha512sum-string obj))
      ((input-port? obj)
       (sha512sum-port obj))
      (else
       (error "sha512sum" "Illegal argument" obj))))

;*---------------------------------------------------------------------*/
;*    hmac-sh256sum-string ...                                         */
;*---------------------------------------------------------------------*/
(define (hmac-sha256sum-string key msg)
   (hmac-string key msg sha256sum-string))

;*---------------------------------------------------------------------*/
;*    hmac-sh512sum-string ...                                         */
;*---------------------------------------------------------------------*/
(define (hmac-sha512sum-string key msg)
   (hmac-string key msg sha512sum-string))


