;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/mmap_numeric.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  User                                              */
;*    Creation    :  Sat Jul 18 2026                                   */
;*    -------------------------------------------------------------    */
;*    Test numeric mmap access functions                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mmap-numeric
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-mmap-numeric)))

;*---------------------------------------------------------------------*/
;*    test-mmap-numeric ...                                            */
;*---------------------------------------------------------------------*/
(define (test-mmap-numeric)
   (test-module "test-mmap-numeric" "mmap_numeric.scm")
   
   (let ((path "tmp_mmap_numeric.data"))
      ;; Create a test file with known binary data
      (with-output-to-file path
	 (lambda ()
	    ;; Write 200 bytes of zeros
	    (let loop ((i 0))
	       (when (<fx i 200)
		  (write-byte 0)
		  (loop (+fx i 1))))))
      
      (let ((mm (open-mmap path :read #t :write #t)))
	 
	 ;; Test 8-bit access
	 (test "mmap-s8-set!.1" 
	       (begin
		  (mmap-s8-set! mm #e10 #s8:-42)
		  (int8->fixnum (mmap-s8-ref mm #e10)))
	       -42)
	 
	 (test "mmap-u8-set!.1"
	       (begin
		  (mmap-u8-set! mm #e11 #u8:100)
		  (uint8->fixnum (mmap-u8-ref mm #e11)))
	       100)
	 
	 ;; Test 16-bit access
	 (test "mmap-u16-set!.1"
	       (begin
		  (mmap-u16-set! mm #e20 #u16:1234)
		  (uint16->fixnum (mmap-u16-ref mm #e20)))
	       1234)
	 
	 (test "mmap-s16-set!.1"
	       (begin
		  (mmap-s16-set! mm #e22 #s16:-1000)
		  (int16->fixnum (mmap-s16-ref mm #e22)))
	       -1000)
	 
	 ;; Test 32-bit access
	 (test "mmap-u32-set!.1"
	       (begin
		  (mmap-u32-set! mm #e30 #u32:12345678)
		  (uint32->fixnum (mmap-u32-ref mm #e30)))
	       12345678)
	 
	 (test "mmap-s32-set!.1"
	       (begin
		  (mmap-s32-set! mm #e34 #s32:-100000)
		  (int32->fixnum (mmap-s32-ref mm #e34)))
	       -100000)
	 
	 ;; Test 64-bit access (use typed constants for comparison)
	 (test "mmap-u64-set!.1"
	       (begin
		  (mmap-u64-set! mm #e40 #u64:1234567891234)
		  (mmap-u64-ref mm #e40))
	       #u64:1234567891234)
	 
	 (test "mmap-s64-set!.1"
	       (begin
		  (mmap-s64-set! mm #e48 #s64:-9876543210)
		  (mmap-s64-ref mm #e48))
	       #s64:-9876543210)
	 
	 ;; Test float access
	 (test "mmap-f32-set!.1"
	       (begin
		  (mmap-f32-set! mm #e60 3.14159)
		  (< (abs (- (mmap-f32-ref mm #e60) 3.14159)) 0.0001))
	       #t)
	 
	 (test "mmap-f64-set!.1"
	       (begin
		  (mmap-f64-set! mm #e70 2.718281828459045)
		  (< (abs (- (mmap-f64-ref mm #e70) 2.718281828459045)) 0.000000001))
	       #t)
	 
	 ;; Test read position tracking
	 (test "mmap-s32-ref.position"
	       (begin
		  (mmap-read-position-set! mm #e0)
		  (mmap-s32-ref mm #e0)
		  (mmap-read-position mm))
	       #e4)
	 
	 (test "mmap-f64-ref.position"
	       (begin
		  (mmap-f64-ref mm #e10)
		  (mmap-read-position mm))
	       #e18)
	 
	 ;; Test write position tracking
	 (test "mmap-u16-set!.position"
	       (begin
		  (mmap-write-position-set! mm #e0)
		  (mmap-u16-set! mm #e0 #u16:42)
		  (mmap-write-position mm))
	       #e2)
	 
	 ;; Test bounds checking
	 (test "mmap-s32-ref.bounds"
	       (with-handler
		  (lambda (e) #t)
		  (mmap-s32-ref mm #e197)  ; Would need bytes 197-200, but only 0-199 exist
		  #f)
	       #t)
	 
	 (test "mmap-f64-set!.bounds"
	       (with-handler
		  (lambda (e) #t)
		  (mmap-f64-set! mm #e195 1.0)  ; Would need bytes 195-202
		  #f)
	       #t)
	 
	 (close-mmap mm))
      
      ;; Clean up
      (delete-file path)))
