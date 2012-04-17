;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/uuid.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 24 12:01:49 2010                          */
;*    Last change :  Tue Apr 17 07:53:53 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generate a UUID according to rfc4122 Version 4.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __uuid
   
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
	   __date
	   __param)
   
   (export (genuuid)))

;*---------------------------------------------------------------------*/
;*    string128 ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (string128 . args)
   (let ((s (gensym)))
      `(let ((,s ($make-string/wo-fill 36)))
	  ,@(map (lambda (i v)
		    `(string-set! ,s ,i ,v))
		 (iota 36)
		 args)
	  ,s)))

;*---------------------------------------------------------------------*/
;*    genuuid ...                                                      */
;*    -------------------------------------------------------------    */
;*    UUID generation                                                  */
;*    See: http://www.ietf.org/rfc/rfc4122.txt                         */
;*                                                                     */
;*    Version 4 UUID, see section 4.4                                  */
;*                                                                     */
;*      0                   1                   2                   3  */
;*    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1  */
;*   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ */
;*   |                          time_low                             | */
;*   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ */
;*   |       time_mid                |         time_hi_and_version   | */
;*   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ */
;*   |clk_seq_hi_res |  clk_seq_low  |         node (0-1)            | */
;*   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ */
;*   |                         node (2-5)                            | */
;*   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ */
;*                                                                     */
;*---------------------------------------------------------------------*/
(define (genuuid)
   
   (define hex
      '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
   
   (define (bit-field bb be n)
      (bit-and (bit-rsh n be) (- (bit-lsh 1 bb) 1)))
   
   (let ((n1 (bit-xor (elong->fixnum (current-seconds)) (random 65536)))
	 (n2 (random 65536))
	 (n3 (random 65536))
	 (n4 (random 65536))
	 (n5 (random 65536))
	 (n6 (random 65536))
	 (n7 (random 65536))
	 (n8 (random 65536)))
      (string128
       ;; time_lo
       (vector-ref hex (bit-field 4 12 n1))
       (vector-ref hex (bit-field 4 8 n1))
       (vector-ref hex (bit-field 4 4 n1))
       (vector-ref hex (bit-field 4 0 n1))
       (vector-ref hex (bit-field 4 12 n2))
       (vector-ref hex (bit-field 4 8 n2))
       (vector-ref hex (bit-field 4 4 n2))
       (vector-ref hex (bit-field 4 0 n2))
       #\-
       ;; time_mid
       (vector-ref hex (bit-field 4 12 n3))
       (vector-ref hex (bit-field 4 8 n3))
       (vector-ref hex (bit-field 4 4 n3))
       (vector-ref hex (bit-field 4 0 n3))
       #\-
       ;; time_hi_and_version
       (vector-ref hex #b0100)
       (vector-ref hex (bit-field 4 8 n4))
       (vector-ref hex (bit-field 4 4 n4))
       (vector-ref hex (bit-field 4 0 n4))
       #\-
       ;; clock_seq_hi_and_reserved
       (vector-ref hex (bit-or (bit-field 2 12 n5) #b1000))
       (vector-ref hex (bit-field 4 8 n5))
       ;; clock_seq_low
       (vector-ref hex (bit-field 4 4 n5))
       (vector-ref hex (bit-field 4 0 n5))
       #\-
       ;; node
       (vector-ref hex (bit-field 4 12 n6))
       (vector-ref hex (bit-field 4 8 n6))
       (vector-ref hex (bit-field 4 4 n6))
       (vector-ref hex (bit-field 4 0 n6))
       (vector-ref hex (bit-field 4 12 n7))
       (vector-ref hex (bit-field 4 8 n7))
       (vector-ref hex (bit-field 4 4 n7))
       (vector-ref hex (bit-field 4 0 n7))
       (vector-ref hex (bit-field 4 12 n8))
       (vector-ref hex (bit-field 4 8 n8))
       (vector-ref hex (bit-field 4 4 n8))
       (vector-ref hex (bit-field 4 0 n8)))))


