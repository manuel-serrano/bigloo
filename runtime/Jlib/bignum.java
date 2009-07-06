package bigloo;

import java.math.BigInteger;

public class bignum extends numeral
{
  public final BigInteger value;

  public bignum( final int value )
  {
     if (value == 0)
	this.value = BigInteger.ZERO;
     else if (value == 1)
	this.value = BigInteger.ONE;
     else
	this.value = new BigInteger(Integer.toString(value));
  }

  public bignum( final long value )
  {
     if (value == 0)
	this.value = BigInteger.ZERO;
     else if (value == 1)
	this.value = BigInteger.ONE;
     else
	this.value = new BigInteger(Long.toString(value));
  }

  public bignum( final double value )
  {
     /* we do not need to care about denormals, ... as they
	are too small for Bignums */
     
     long l = Double.doubleToLongBits(value);
     long shifted_e = l & 0x7FF0000000000000L;
     long mantissa =  l & 0x000FFFFFFFFFFFFFL;  // 52 bits mantissa
     // add the hidden bit. (would be wrong for denormals)
     long f = mantissa |  0x0010000000000000L;
     if (l < 0) f = -f;

     if (shifted_e == 0x7FF0000000000000L) // NaN, Inf
	throw new ArithmeticException("Can't transform NaN, Inf to Bignum");

     int biased_e = (int) (shifted_e >> 52);

     // Usually (in other literature) 'e' is biased by 0x3FF. However then the
     // number is represented as 1.mantissa * 2^e
     // Here we prefer 1mantissa * 2^e and we have hence to remove this 52 bit
     // shift too.
     int e = biased_e - 0x3FF - 52;
     
     if (e < -52) // 52 == mantissa size
	this.value = BigInteger.ZERO;
     else if (e <= 0)
	this.value = new BigInteger(Long.toString(f >> -e));
     else {
	BigInteger unshifted = new BigInteger(Long.toString(f));
	this.value = unshifted.shiftLeft(e);
     }
  }

  public bignum( final BigInteger value )
  {
     this.value = value;
  }

  public void write( final output_port p )
  {
     p.write( value.toString() );
  }
}
