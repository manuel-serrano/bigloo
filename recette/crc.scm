;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/crc.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Thu Sep  3 12:31:31 2009                          */
;*    Last change :  Thu Sep  3 12:31:44 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    CRC tests                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module crc
   (import (main "main.scm"))
   (include "test.sch")
   (export (test-crc)))

;; a online CRC-tester can be found at: http://zorc.breitbandkatze.de/crc.html
(define (number->+string nb)
   (if (>= nb 0)
       (format "0x~x" nb)
       (cond
	  ((fixnum? nb) ;; must not happen
	   (error 'crc-gen
		  "longs must never be < 0"
		  nb))
	  ((elong? nb)
	   (format "0x~x~x"
		   (bit-andelong #exFFFF (bit-rshelong nb 16))
		   (bit-andelong #exFFFF nb)))
	  ((llong? nb)
	   (format "0x~x~x"
		   (bit-andllong #lxFFFFFFFF (bit-rshllong nb 32))
		   (bit-andllong #lxFFFFFFFF nb))))))

; (define (test name)
;    (print "(")
;    (print "(" name " \"" (number->+string (crc name "0")) "\" \"0\")")
;    (print "(" name " \"" (number->+string (crc name "A")) "\" \"A\")")
;    (print "(" name " \""
; 	  (number->+string
; 	   (crc name "the quick brown fox jumps over the lazy dog"))
; 	  "\" \"the quick brown fox jumps over the lazy dog\")")
;    (print "(" name " \""
; 	  (number->+string
; 	   (crc name "the quick brown fox jumps over the lazy dog"
; 		:init -1 :final-xor -1))
; 	  "\" \"the quick brown fox jumps over the lazy dog\""
; 	  " :init -1 :final-xor -1)")
;    (print ")"))

(define (test-crc-line line)
   (let ((name (car line))
	 (result (cadr line))
	 (params (cddr line)))
      (test (format "CRC ~a" name)
	    (number->+string (apply crc name params))
	    result)))

(define (test-crc-little-endian)
   (test "CRC little endian" (number->+string (crc 'atm-8 "W" :big-endian? #f))  "0x19"))

(define *tests*
   '(
     (                                                                         
      (ieee-32 "0xd4326d90" "0")                                                
      (ieee-32 "0x30476dc0" "A")                                                
      (ieee-32 "0x7cf3039d" "the quick brown fox jumps over the lazy dog")      
      (ieee-32 "0xf0269282" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)
      )                                                                                          
     (                                                                                          
      (radix-64-24 "0xc99f60" "0")                                                               
      (radix-64-24 "0x9fef29" "A")                                                               
      (radix-64-24 "0x70d45d" "the quick brown fox jumps over the lazy dog")                     
      (radix-64-24 "0xff8015" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)
      )                                                                                            
     (                                                                                            
      (ccitt-16 "0x3653" "0")                                                                      
      (ccitt-16 "0x58e5" "A")                                                                      
      (ccitt-16 "0x7025" "the quick brown fox jumps over the lazy dog")                            
      (ccitt-16 "0xf0cf" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)     
      )                                                                                            
     (                                                                                            
      (dnp-16 "0x8f64" "0")                                                                        
      (dnp-16 "0x53" "A")                                                                          
      (dnp-16 "0x13e6" "the quick brown fox jumps over the lazy dog")                              
      (dnp-16 "0x63ff" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)       
      )                                                                                            
     (                                                                                            
      (ibm-16 "0xa0" "0")                                                                          
      (ibm-16 "0x186" "A")                                                                         
      (ibm-16 "0xb04c" "the quick brown fox jumps over the lazy dog")                              
      (ibm-16 "0x8d99" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)       
      )                                                                                            
     (                                                                                            
      (24 "0xb8d16f" "0")                                                                          
      (24 "0x6a182d" "A")                                                                          
      (24 "0x695db" "the quick brown fox jumps over the lazy dog")                                 
      (24 "0xa8853" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)          
      )                                                                                            
     (                                                                                            
      (30 "0x61739e0" "0")                                                                         
      (30 "0x8148c0a" "A")                                                                         
      (30 "0x2e093295" "the quick brown fox jumps over the lazy dog")                              
      (30 "0x349979c5" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)       
      )                                                                                            
     (                                                                                            
      (c-32 "0xbf3c2b2" "0")                                                                       
      (c-32 "0xf1d2b3c6" "A")                                                                      
      (c-32 "0xdc57d5c1" "the quick brown fox jumps over the lazy dog")                            
      (c-32 "0x8630ec48" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)     
      )                                                                                            
     (                                                                                            
      (k-32 "0x8a222886" "0")                                                                      
      (k-32 "0xe7879e08" "A")                                                                      
      (k-32 "0x15725c85" "the quick brown fox jumps over the lazy dog")                            
      (k-32 "0xa4ed801e" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)     
      )                                                                                            
     (                                                                                            
      (q-32 "0x28283560" "0")                                                                      
      (q-32 "0x61e1cdb2" "A")                                                                      
      (q-32 "0xc14dda96" "the quick brown fox jumps over the lazy dog")                            
      (q-32 "0xdcbe462e" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)     
      )                                                                                            
     (                                                                                            
      (itu-4 "0xf" "0")                                                                            
      (itu-4 "0x4" "A")                                                                            
      (itu-4 "0x0" "the quick brown fox jumps over the lazy dog")                                  
      (itu-4 "0x1" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)           
      )                                                                                            
     (                                                                                            
      (epc-5 "0xe" "0")                                                                            
      (epc-5 "0x6" "A")                                                                            
      (epc-5 "0x1a" "the quick brown fox jumps over the lazy dog")                                 
      (epc-5 "0xb" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)           
      )                                                                                            
     (                                                                                            
      (itu-5 "0x1e" "0")                                                                           
      (itu-5 "0x1b" "A")                                                                           
      (itu-5 "0x0" "the quick brown fox jumps over the lazy dog")                                  
      (itu-5 "0xa" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)           
      )                                                                                            
     (                                                                                            
      (usb-5 "0xb" "0")                                                                            
      (usb-5 "0x2" "A")                                                                            
      (usb-5 "0x1a" "the quick brown fox jumps over the lazy dog")                                 
      (usb-5 "0x6" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)           
      )                                                                                            
     (                                                                                            
      (itu-6 "0x13" "0")                                                                           
      (itu-6 "0x6" "A")                                                                            
      (itu-6 "0x3c" "the quick brown fox jumps over the lazy dog")                                 
      (itu-6 "0x12" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)          
      )                                                                                            
     (                                                                                            
      (7 "0x2b" "0")                                                                               
      (7 "0x6d" "A")                                                                               
      (7 "0x21" "the quick brown fox jumps over the lazy dog")                                     
      (7 "0x5b" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)              
      )                                                                                            
     (                                                                                            
      (atm-8 "0x90" "0")                                                                           
      (atm-8 "0xc0" "A")                                                                           
      (atm-8 "0x6c" "the quick brown fox jumps over the lazy dog")                                 
      (atm-8 "0x96" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)          
      )                                                                                            
     (                                                                                            
      (ccitt-8 "0x2d" "0")                                                                         
      (ccitt-8 "0xe1" "A")                                                                         
      (ccitt-8 "0xe7" "the quick brown fox jumps over the lazy dog")                               
      (ccitt-8 "0xb" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)         
      )                                                                                            
     (                                                                                            
      (dallas/maxim-8 "0xc5" "0")                                                                  
      (dallas/maxim-8 "0xc" "A")                                                                   
      (dallas/maxim-8 "0xf" "the quick brown fox jumps over the lazy dog")                         
      (dallas/maxim-8 "0xd2" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1) 
      )                                                                                            
     (                                                                                            
      (8 "0xf6" "0")                                                                               
      (8 "0x48" "A")                                                                               
      (8 "0xb8" "the quick brown fox jumps over the lazy dog")                                     
      (8 "0xac" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)              
      )                                                                                            
     (                                                                                            
      (sae-j1850-8 "0x4a" "0")                                                                     
      (sae-j1850-8 "0xe" "A")                                                                      
      (sae-j1850-8 "0x62" "the quick brown fox jumps over the lazy dog")                           
      (sae-j1850-8 "0xa7" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)    
      )                                                                                            
     (                                                                                            
      (10 "0x53" "0")                                                                              
      (10 "0x2f7" "A")                                                                             
      (10 "0x143" "the quick brown fox jumps over the lazy dog")                                   
      (10 "0x3c3" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)            
      )                                                                                            
     (
      (11 "0x3d2" "0")
      (11 "0x6bd" "A")
      (11 "0x3" "the quick brown fox jumps over the lazy dog")
      (11 "0x11a" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)
      )
     (
      (12 "0x1e0" "0")
      (12 "0x28a" "A")
      (12 "0x198" "the quick brown fox jumps over the lazy dog")
      (12 "0x5d4" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)
      )
     (
      (can-15 "0x76b9" "0")
      (can-15 "0x365c" "A")
      (can-15 "0x7d22" "the quick brown fox jumps over the lazy dog")
      (can-15 "0x43a8" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)
      )
     (
      (iso-64 "0x2d0" "0")
      (iso-64 "0x6db" "A")
      (iso-64 "0xcec18283cd5f9d90" "the quick brown fox jumps over the lazy dog")
      (iso-64 "0x313ebbd058a0626f" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)
      )
     (
      (ecma-182-64 "0xaa478900b1228e31" "0")
      (ecma-182-64 "0x98f5e3fe438617bc" "A")
      (ecma-182-64 "0xd1548731e5eb4fb8" "the quick brown fox jumps over the lazy dog")
      (ecma-182-64 "0x2c6c6e457767a695" "the quick brown fox jumps over the lazy dog" :init -1 :final-xor -1)
      )
     ))

(define (test-crc)
   (test-module "crc" "crc.scm")
   (for-each (lambda (t)
		(test-crc-line (car t))
		(test-crc-line (cadr t))
		(test-crc-line (caddr t))
		(test-crc-line (cadddr t)))
	     *tests*)
   (test-crc-little-endian))
