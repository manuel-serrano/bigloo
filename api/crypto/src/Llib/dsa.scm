;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2009-11 Florian Loitsch                           */
;*    -------------------------------------------------------------    */
;*    Message encryption and decryption based on the DSA asymmetric    */
;*    cipher.                                                          */
;*=====================================================================*/

(module __crypto-dsa
   (import __crypto-util)
   (export (class Dsa-Key
	      p::bignum q::bignum g::bignum y::bignum)
	   (class Complete-Dsa-Key::Dsa-Key
	    x::bignum)) ;; the private key
   
   (export (extract-public-dsa-key::Dsa-Key key::Complete-Dsa-Key)
	   (dsa-sign   key::Complete-Dsa-Key m::bignum)
	   (dsa-verify key::Dsa-Key m::bignum r::bignum s::bignum)))

(define (extract-public-dsa-key key)
   (duplicate::Dsa-Key key))

(define (dsa-sign key m::bignum)
   (with-access::Complete-Dsa-Key key (p q g y x)
      (let* ((k (make-random-bignum (-fx (bignum-bit-length q) 1)))
	     (r (modulobx (expt-modbx g k p) q))
	     (k-1 (mod-inverse k q))
	     (s (modulobx (*bx k-1 (+bx m (*bx x r))) q)))
	 (if (or (=bx r #z0)
		 (=bx s #z0))
	     (dsa-sign key m)
	     (values r s)))))

(define (dsa-verify key m::bignum r::bignum s::bignum)
   (with-access::Dsa-Key key (p q g y)
      (and (>bx r #z0) (<bx r q)
	   (>bx s #z0) (<bx s q)
	   (let* ((w (mod-inverse s q))
		  (u1 (modulobx (*bx m w) q))
		  (u2 (modulobx (*bx r w) q))
		  (v (modulobx (modulobx (*bx (expt-modbx g u1 p)
					      (expt-modbx y u2 p))
					 p)
			       q)))
	      (=bx v r)))))
