;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/crc16.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 26 09:34:19 2007                          */
;*    Last change :  Tue Apr 17 07:52:39 2012 (serrano)                */
;*    Copyright   :  2007-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    CRC16 encoding                                                   */
;*    -------------------------------------------------------------    */
;*    crc16 is equivalent to (crc 'ibm-16 :init #xFFFF ...)            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __crc16
   
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
	   __base64)

   (import __param)

   (export (crc16::int ::obj)
	   (crc16-mmap::int ::mmap)
	   (crc16-string::int ::bstring)
	   (crc16-port::int ::input-port)
	   (crc16-file::int ::bstring)))

;*---------------------------------------------------------------------*/
;*    crc16-polynomial ...                                             */
;*---------------------------------------------------------------------*/
(define (crc16-polynomial::long) #x8005)

;*---------------------------------------------------------------------*/
;*    crc16 ...                                                        */
;*---------------------------------------------------------------------*/
(define (crc16 obj)
   (cond
      ((mmap? obj)
       (crc16-mmap obj))
      ((string? obj)
       (crc16-string obj))
      ((input-port? obj)
       (crc16-port obj))
      (else
       (error 'crc16 "Illegal argument" obj))))

;*---------------------------------------------------------------------*/
;*    crc-value ...                                                    */
;*---------------------------------------------------------------------*/
(define (crc-value::long val::long crc::long)
   (let loop ((i 0)
	      (value::long (bit-lsh val 8))
	      (crc::long crc))
      (if (=fx i 8)
	  crc
	  (let ((value::long (bit-lsh value 1))
		(crc::long (bit-lsh crc 1)))
	     (loop (+fx i 1)
		   value
		   (if (=fx 0 (bit-and #x10000 (bit-xor crc value)))
		       crc
		       (bit-xor crc (crc16-polynomial))))))))

;*---------------------------------------------------------------------*/
;*    crc16-mmap ...                                                   */
;*---------------------------------------------------------------------*/
(define (crc16-mmap mmap)
   (let ((len (mmap-length mmap)))
      (let loop ((i 0)
		 (crc::long #xffff))
	 (if (=fx i len)
	     (bit-and crc #xffff)
	     (loop (+fx i 1)
		   (crc-value (char->integer ($mmap-ref mmap i)) crc))))))

;*---------------------------------------------------------------------*/
;*    crc16-string ...                                                 */
;*---------------------------------------------------------------------*/
(define (crc16-string string)
   (let ((len (string-length string)))
      (let loop ((i 0)
		 (crc::long #xffff))
	 (if (=fx i len)
	     (bit-and crc #xffff)
	     (loop (+fx i 1)
		   (crc-value (char->integer (string-ref string i)) crc))))))

;*---------------------------------------------------------------------*/
;*    crc16-port ...                                                   */
;*---------------------------------------------------------------------*/
(define (crc16-port port)
   (let loop ((crc::long #xffff))
      (let ((byte::long (read-byte port)))
	 (if (eof-object? byte)
	     (bit-and crc #xffff)
	     (loop (crc-value byte crc))))))

;*---------------------------------------------------------------------*/
;*    crc16-file ...                                                   */
;*---------------------------------------------------------------------*/
(define (crc16-file file)
   (with-input-from-file file
      (lambda () (crc16-port (current-input-port)))))
