using System;
// using Mono.Math;

namespace bigloo 
{
  public sealed class bignum: numeral 
  {
    public readonly BigInteger value;

    public static readonly BigInteger BZERO = new BigInteger( 0 );

    public bignum( BigInteger value ) 
    {
      this.value = value;
    }

    public bignum( int value ) 
    {
      this.value = new BigInteger(value);
    }

    public bignum( long value ) 
    {
      this.value = new BigInteger(value);
    }

    public bignum( double value ) 
    {
      /* we do not need to care about denormals, ... as they
	are too small for Bignums */
     
     long l = BitConverter.DoubleToInt64Bits(value);
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
     
     if (e <= 0)
	this.value = new BigInteger(f >> -e);
     else
	this.value = new BigInteger(f) << e;
    }

    public bignum rand( Random randg ) {
       BigInteger b = this.value;
       int numBits = b.bitCount();
       BigInteger x = new BigInteger();
       x.genRandomBits( numBits, randg );
	    
       if (x > b)
	  return new bignum(x % b);
       else
	  return new bignum(x);
    }

    public double DoubleValue( ) 
    {
       return Double.Parse(value.ToString());
    }

    public int IntegerValue( ) 
    {
       return (int)Double.Parse(value.ToString());
    }

    public override void write( output_port p ) 
    {
      p.write( value.ToString() );
    }
  }
}
