;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2009-11 Florian Loitsch                           */
;*    -------------------------------------------------------------    */
;*    Message encryption and decryption based on the ElGamal           */
;*    asymmetric cipher.                                               */
;*=====================================================================*/

(module __crypto-elgamal
   (import __crypto-util)
   (export (class ElGamal-Key
	      p::bignum
	      g::bignum
	      y::bignum)
	   (final-class Complete-ElGamal-Key::ElGamal-Key
	      x::bignum)) ;; the private key
   
   (export (elgamal-decrypt::bignum key::Complete-ElGamal-Key c1::bignum c2::bignum)
	   (elgamal-encrypt key::ElGamal-Key m::bignum)
	   (extract-public-elgamal-key::ElGamal-Key key::Complete-ElGamal-Key)
	   (elgamal-key-length::long key::ElGamal-Key)))

(define (elgamal-key-length key)
   (with-access::ElGamal-Key key (p)
      (/ceilingfx (bignum-bit-length p) 8)))

(define (extract-public-elgamal-key key)
   (duplicate::ElGamal-Key key))

(define (find-relatively-prime n)
   (let ((k (make-random-bignum (-fx (bignum-bit-length n) 1))))
      (if (=bx #z1 (gcdbx n k))
	  k
	  (find-relatively-prime n))))

(define (elgamal-encrypt key m::bignum)
   (with-access::ElGamal-Key key (p g y)
      (let* ((k (find-relatively-prime (-bx p #z1)))
	     (c1 (expt-modbx g k p))
	     (c2 (modulobx (*bx m (expt-modbx y k p)) p)))
	 (values c1 c2))))

(define (elgamal-decrypt::bignum key c1::bignum c2::bignum)
   (with-access::Complete-ElGamal-Key key (p g y x)
      (let ((w (mod-inverse (expt-modbx c1 x p) p)))
	 (modulobx (*bx w c2) p))))
