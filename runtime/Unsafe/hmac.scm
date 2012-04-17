;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/hmac.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  5 08:31:42 2008                          */
;*    Last change :  Tue Apr 17 07:52:18 2012 (serrano)                */
;*    Copyright   :  2008-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HMAC and CRAM                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hmac
   
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
	   __srfi4)
   
   (import __param
	   __base64)
   
   (export (hmac-string::bstring ::bstring ::bstring ::procedure)))

;*---------------------------------------------------------------------*/
;*    hmac-string ...                                                  */
;*    -------------------------------------------------------------    */
;*    keyed-Hash Message Authentication Code.                          */
;*---------------------------------------------------------------------*/
(define (hmac-string key message hash)

   (let* ((block-size 64)
	  (ipadc #x36)
	  (opadc #x5c)
	  ;; keyb is the key padded to 64 with 0
	  (keyb (make-string block-size #a000))
	  (ixor ($make-string/wo-fill block-size))
	  (oxor ($make-string/wo-fill block-size)))
   
      (let ((key-length (string-length key)))
	 (if (>fx key-length block-size)
	     (blit-string! (string-hex-intern! (hash key)) 0 keyb 0 16)
	     (blit-string! key 0 keyb 0 key-length)))
   
      (let loop ((i 0))
	 (when (<fx i block-size)
	    (let ((xi (char->integer (string-ref keyb i))))
	       (string-set! ixor i (integer->char (bit-xor ipadc xi)))
	       (string-set! oxor i (integer->char (bit-xor opadc xi)))
	       (loop (+fx i 1)))))
   
      (hash
       (string-append
	oxor
	(string-hex-intern! (hash (string-append ixor message)))))))
