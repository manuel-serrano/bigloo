package bigloo;

import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import java.lang.Runtime;
import java.net.*;
import java.util.regex.*;

public final class foreign
{
   private static final bint[] bint_allocated = new bint[2148];
   private static Random randg = new Random( 1 );

   static
      {
	 for (int i = -100; i < 2048; ++i)
	    bint_allocated[i + 100] = new bint(i);
      }

   // CARE no foreign functions may throws something.. CATCH it

   public static byte[] bigloo_backend() {
      return "bigloo-jvm".getBytes();
   }
   
   public static void trace_exit()
      {
	 // System.out.println( "Good bye." );
      }

   public static void dump_stack( output_port p )
      {
	 final Exception o = new Exception( "JVM Bigloo Error" );
	 final stackwriter sw = new stackwriter( p.out, true );

	 //o.printStackTrace( new PrintStream( p.out ) );
	 synchronized( p ) {
	    o.printStackTrace( sw );
	 }
      }

   /////
   // PRAGMAS
   /////
   public static int ptr_alg()
      {
	 return 2;
      }

   /////
   // INTERNAL
   /////
   public static void print(String msg)
      {
	 System.out.println(msg);
      }

   public static void print(byte[] msg)
      {
	 print( new String( msg ) );
      }

   public static void oprint(Object msg)
      {
	 System.out.println("OPRINT = " + msg);
      }

   public static void Error(String msg)
      {
	 System.err.println(msg);
	 System.exit(-1);
      }

   public static byte[] FOREIGN_TYPE_NAME(Object o)
      {
	 return o.getClass().toString().getBytes();
      }

   public static Object listargv(String[]argv)
      {
	 final int len = argv.length;
	 Object result = BNIL;

	 for (int i = 0; i < len; ++i)
	    result = new pair(argv[len - i - 1].getBytes(), result);
	 result = new pair(executable_name, result);
	 command_line = result;
	 return result;
      }

   public static int parseint(byte[]buf, int pos, int bout, int radix)
      {
	 int result = 0;
	 boolean neg = false;
	 byte cn;

	 if (buf[pos] == (byte) '-')
	 {
	    ++pos;
	    neg = true;
	 }
	 if (buf[pos] == (byte) '+')
	 {
	    ++pos;
	    neg = false;
	 }

	 while (pos < bout)
	 {
	    cn = buf[pos++];
	    if (((byte) '0' <= cn) && (cn <= (byte) '9'))
	       result = result * radix + (cn - (byte) '0');
	    else if( (cn >= (byte)'A') && (cn <= (byte) 'F') )
	       result = result * radix + 10 + (cn - (byte) 'A');
	    else if( (cn >= (byte)'a') && (cn <= (byte) 'f') )
	       result = result * radix + 10 + (cn - (byte) 'a');
	 }

	 return (neg ? -result : result);
      }

   public static long parselong(byte[]buf, int pos, int bout, int radix)
      {
	 long result = 0;
	 boolean neg = false;
	 byte cn;

	 if (buf[pos] == (byte) '-')
	 {
	    ++pos;
	    neg = true;
	 }
	 if (buf[pos] == (byte) '+')
	 {
	    ++pos;
	    neg = false;
	 }

	 while (pos < bout)
	 {
	    cn = buf[pos++];
	    if (((byte) '0' <= cn) && (cn <= (byte) '9'))
	       result = result * radix + (cn - (byte) '0');
	    else if( (cn >= (byte)'A') && (cn <= (byte) 'F') )
	       result = result * radix + 10 + (cn - (byte) 'A');
	    else if( (cn >= (byte)'a') && (cn <= (byte) 'f') )
	       result = result * radix + 10 + (cn - (byte) 'a');
	 }

	 return (neg ? -result : result);
      }

   private static int charDigit2Val(byte cn) {
      if (((byte) '0' <= cn) && (cn <= (byte) '9'))
	 return cn - (byte) '0';
      else if (cn <= (byte) 'Z')
	 return cn - (byte) 'A';
      else
	 return cn - (byte) 'a';
   }
   
   public static Object parseinteger(byte[]buf, int initPos, int bout, int radix)
      {
	 int pos = initPos;
	 int result = 0;
	 boolean neg = false;
	 int cn;

	 if (buf[pos] == (byte) '-')
	 {
	    ++pos;
	    neg = true;
	 }
	 if (buf[pos] == (byte) '+')
	 {
	    ++pos;
	    neg = false;
	 }

	 while (pos < bout)
	 {
	    cn = charDigit2Val( buf[pos] );

	    if ( result > MAX_VALUE_FX / radix - cn )
	    {
	       long lresult = result; 
	       while (pos < bout)
	       {
		  cn = charDigit2Val( buf[pos] );
		  if ( lresult > MAX_VALUE_ELONG / radix - cn )
		     return bgl_string_to_bignum( new String(buf, initPos, bout - initPos), radix );

		  lresult = lresult * radix + cn;
		  pos++;
	       }
	       return LLONG_TO_BLLONG(neg ? -lresult : lresult);
	    } else {
	       result = result * radix + cn;
	       pos++;
	    }
	 }

	 return BINT(neg ? -result : result);
      }

   private static int Integer_signum( int i ) {
      if( i > 0 ) return 1;
      if( i == 0 ) return 0;
      return -1;
   }

   private static int Long_signum( long i ) {
      if( i > 0 ) return 1;
      if( i == 0 ) return 0;
      return -1;
   }
   
   /////
   // BOOLEAN
   /////
   // Constants
   public static final bbool BTRUE = bbool.vrai;
   public static final bbool BFALSE = bbool.faux;

   // Predicates
   public static boolean BOOLEANP(Object o)
      {
	 return ((o == bbool.faux) || (o == bbool.vrai));
      }

   // Conversions
   public static boolean CBOOL(bbool b)
      {
	 return (b != bbool.faux);
      }

   public static boolean CBOOL(Object b)
      {
	 return (b != bbool.faux);
      }

   public static bbool BBOOL(boolean b)
      {
	 return (b ? bbool.vrai : bbool.faux);
      }

   // Lib functions
   public static boolean EQ(Object o1, Object o2)
      {
	 return ((o1 == o2) ||
		 ((o1 instanceof bint)
		  && (o2 instanceof bint)
		  && (((bint) o1).value == ((bint) o2).value)));
      }

   public static boolean BOXED_EQ(Object o1, Object o2)
      {
	 return (o1 == o2);
      }

   //////
   // CHARACTER
   //////
   // Predicates
   public static boolean CHARP(Object o)
      {
	 return (o instanceof bchar);
      }

   // Conversions
   public static bchar BCHAR(int cn)
      {
	 return bchar.allocated[cn & 0xFF];
      }

   public static bchar BCHAR(byte c)
      {
	 return bchar.allocated[c & 0xFF];
      }

   public static byte CCHAR(bchar c)
      {
	 return c.value;
      }

   public static int BCHAR_TO_UCHAR(bchar c)
      {
	 return (c.value & 0xFF);
      }

   public static int BCHAR_TO_UBYTE(bchar c)
      {
	 return c.value;
      }

   public static int CHAR_TO_UCHAR(byte c)
      {
	 return (c & 0xFF);
      }

   public static byte UCHAR_TO_CHAR(int c)
      {
	 return (byte) c;
      }

   // Open functions
   public static boolean CHAR_EQ(int cn1, int cn2)
      {
	 return (cn1 == cn2);
      }

   public static boolean CHAR_LT(int cn1, int cn2)
      {
	 return ((cn1 & 0xFF) < (cn2 & 0xFF));
      }

   public static boolean CHAR_GT(int cn1, int cn2)
      {
	 return ((cn1 & 0xFF) > (cn2 & 0xFF));
      }

   public static boolean CHAR_LE(int cn1, int cn2)
      {
	 return ((cn1 & 0xFF) <= (cn2 & 0xFF));
      }

   public static boolean CHAR_GE(int cn1, int cn2)
      {
	 return ((cn1 & 0xFF) >= (cn2 & 0xFF));
      }

   public static int CHAR_OR(int cn1, int cn2)
      {
	 return (cn1 | cn2);
      }

   public static int CHAR_AND(int cn1, int cn2)
      {
	 return (cn1 & cn2);
      }

   public static int CHAR_NOT(int cn)
      {
	 return ~ cn;
      }

   // Lib functions
   public static int toupper(int cn)
      {
	 if ((97 <= cn) && (cn <= 122))
	    return (cn - 32);
	 return cn;
      }

   public static int tolower(int cn)
      {
	 if ((65 <= cn) && (cn <= 90))
	    return (cn + 32);
	 return cn;
      }

   //////
   // INTEGER
   //////
   // Constants
   public static final int SIZEOFLONG = 4;

   public static final int MIN_VALUE_FX = java.lang.Integer.MIN_VALUE;
   public static final int MAX_VALUE_FX = java.lang.Integer.MAX_VALUE;
   public static final long MIN_VALUE_ELONG = java.lang.Long.MIN_VALUE;
   public static final long MAX_VALUE_ELONG = java.lang.Long.MAX_VALUE;

   // Predicates
   public static boolean INTEGERP(Object o)
      {
	 return (o instanceof bint);
      }

   public static boolean ELONGP(Object o)
      {
	 return (o instanceof belong);
      }

   public static boolean LLONGP(Object o)
      {
	 return (o instanceof bllong);
      }

   public static boolean BGL_INT8P(Object o)
      {
	 return (o instanceof bint8);
      }
   public static boolean BGL_INT16P(Object o)
      {
	 return (o instanceof bint16);
      }
   public static boolean BGL_INT32P(Object o)
      {
	 return (o instanceof bint32);
      }
   public static boolean BGL_INT64P(Object o)
      {
	 return (o instanceof bint64);
      }
   public static boolean BIGNUMP(Object o)
      {
	 return (o instanceof bignum);
      }


   // Conversions
   public static int CHAR_TO_INT(int cn)
      {
	 return (cn & 0xFF);
      }

   public static int INT_TO_CHAR(int n)
      {
	 return n;
      }

   public static byte INT_TO_BYTE(int n)
      {
	 return (byte)n;
      }

   public static byte BYTE_TO_UBYTE(byte n)
      {
	 return (byte)n;
      }
   
   public static int BYTE_TO_LONG(byte n)
      {
	 return n;
      }
   
   public static int BYTE_TO_INT(byte n)
      {
	 return n;
      }
   
   public static int BYTE_TO_ULONG(byte n)
      {
	 return n;
      }
   
   public static int UBYTE_TO_INT(byte n)
      {
	 return n;
      }
   
   public static int UBYTE_TO_LONG(byte n)
      {
	 return n;
      }
   
   public static int UBYTE_TO_ULONG(byte n)
      {
	 return n;
      }
   
   public static short INT_TO_SHORT(int n)
      {
	 return (short)n;
      }

   public static int SHORT_TO_INT(short n)
      {
	 return n;
      }

   public static int SHORT_TO_LONG(short n)
      {
	 return n;
      }
   public static int USHORT_TO_LONG(short n)
      {
	 return n;
      }

   public static int BINT_TO_ULONG(bint n)
      {
	 return n.value;
      }
   public static byte BINT_TO_UBYTE(bint n)
      {
	 return (byte)n.value;
      }
   public static byte BINT_TO_BYTE(bint n)
      {
	 return (byte)n.value;
      }
   public static short BINT_TO_SHORT(bint n)
      {
	 return (short)n.value;
      }
   public static short BINT_TO_USHORT(bint n)
      {
	 return (short)n.value;
      }
   public static byte BINT_TO_INT8(bint n)
      {
	 return (byte)n.value;
      }
   public static short BINT_TO_INT16(bint n)
      {
	 return (short)n.value;
      }
   public static int BINT_TO_INT32(bint n)
      {
	 return (int)n.value;
      }
   public static long BINT_TO_INT64(bint n)
      {
	 return (long)n.value;
      }
   public static short LONG_TO_SHORT(int n)
      {
	 return (short)n;
      }
   public static short LONG_TO_USHORT(int n)
      {
	 return (short)n;
      }

   public static int LONG_TO_INT(int n)
      {
	 return n;
      }

   public static int LONG_TO_ULONG(int n)
      {
	 return n;
      }

   public static byte LONG_TO_BYTE(int n)
      {
	 return (byte)n;
      }

   public static byte LONG_TO_UBYTE(int n)
      {
	 return (byte)n;
      }

   public static byte LONG_TO_INT8(int n)
      {
	 return (byte)n;
      }

   public static short LONG_TO_INT16(int n)
      {
	 return (short)n;
      }

   public static int LONG_TO_INT32(int n)
      {
	 return (int)n;
      }

   public static long LONG_TO_INT64(int n)
      {
	 return (long)n;
      }

   public static byte ULONG_TO_BYTE(int n)
      {
	 return (byte)n;
      }
   public static byte ULONG_TO_UBYTE(int n)
      {
	 return (byte)n;
      }
   public static int ULONG_TO_INT(int n)
      {
	 return n;
      }
   public static int ULONG_TO_LONG(int n)
      {
	 return n;
      }

   public static bllong LLONG_TO_BLLONG(long n)
      {
	 return new bllong(n);
      }

   public static bllong ULLONG_TO_BLLONG(long n)
      {
	 return new bllong(n);
      }

   public static long LONG_TO_LLONG(int n)
      {
	 return n;
      }

   public static long ELONG_TO_LLONG(long n)
      {
	 return n;
      }

   public static long LONG_TO_ULLONG(int n)
      {
	 return n;
      }

   public static byte ELONG_TO_INT8(long n)
      {
	 return (byte)n;
      }
   public static long INT8_TO_ELONG(byte n)
      {
	 return (long)n;
      }

   public static short ELONG_TO_INT16(long n)
      {
	 return (short)n;
      }
   public static long INT16_TO_ELONG(short n)
      {
	 return (long)n;
      }

   public static int ELONG_TO_INT32(long n)
      {
	 return (int)n;
      }
   public static long INT32_TO_ELONG(int n)
      {
	 return (long)n;
      }
   public static long INT32_TO_LLONG(int n)
      {
	 return (long)n;
      }
   public static int LLONG_TO_LONG(long n)
      {
	 return (int)n;
      }

   public static int LLONG_TO_ULONG(long n)
      {
	 return (int)n;
      }

   public static long BLLONG_TO_LLONG(bllong n)
      {
	 return n.value;
      }

   public static long BLLONG_TO_ULLONG(bllong n)
      {
	 return n.value;
      }

   public static long BLLONG_TO_LONG(bllong n)
      {
	 return (long)n.value;
      }

   public static byte LLONG_TO_INT8(long n)
      {
	 return (byte)n;
      }

   public static short LLONG_TO_INT16(long n)
      {
	 return (short)n;
      }

   public static int LLONG_TO_INT32(long n)
      {
	 return (int)n;
      }

   public static long INT64_TO_ELONG(long n)
      {
	 return (long)n;
      }
   public static long ELONG_TO_INT64(long n)
      {
	 return (long)n;
      }

   public static long INT64_TO_LLONG(long n)
      {
	 return (long)n;
      }
   public static long LLONG_TO_INT64(long n)
      {
	 return (long)n;
      }

   public static belong LONG_TO_BELONG(int n)
      {
	 return new belong(n);
      }

   public static long LONG_TO_ELONG(int n)
      {
	 return n;
      }

   public static int ELONG_TO_LONG(long n)
      {
	 return (int)n;
      }

   public static long LLONG_TO_ELONG(long n)
      {
	 return n;
      }

   public static int BELONG_TO_LONG(belong n)
      {
	 return (int)(n.value);
      }

   public static long UELONG_TO_ELONG(long n)
      {
	 return n;
      }

   public static long ELONG_TO_UELONG(long n)
      {
	 return n;
      }

   public static long BELONG_TO_ELONG(belong n)
      {
	 return n.value;
      }

   public static belong ELONG_TO_BELONG(long n)
      {
	 return new belong(n);
      }

   public static bllong LONG_TO_BLLONG(int n)
      {
	 return new bllong(n);
      }

   public static long ULLONG_TO_LLONG(long n)
      {
	 return n;
      }

   public static long LLONG_TO_ULLONG(long n)
      {
	 return n;
      }

   public static bignum LONG_TO_BIGNUM(int n)
      {
	 return new bignum(n);
      }

   public static bint8 BGL_INT8_TO_BINT8( byte n )
      {
	 return new bint8( n );
      }
      
   public static byte BGL_BINT8_TO_INT8( bint8 n )
      {
	 return n.value;
      }
      
   public static bint16 BGL_INT16_TO_BINT16( short n )
      {
	 return new bint16( n );
      }
      
   public static short BGL_BINT16_TO_INT16( bint16 n )
      {
	 return n.value;
      }
      
   public static bint32 BGL_INT32_TO_BINT32( int n )
      {
	 return new bint32( n );
      }
      
   public static int BGL_BINT32_TO_INT32( bint32 n )
      {
	 return n.value;
      }
      
   public static bint64 BGL_INT64_TO_BINT64( long n )
      {
	 return new bint64( n );
      }

   public static long BGL_BINT64_TO_INT64( bint64 n )
      {
	 return n.value;
      }
      
   public static byte BGL_INT8_ID( byte n )
      { 
	 return n;
      }
      
   public static short BGL_INT16_ID( short n )
      { 
	 return n;
      }
      
   public static int BGL_INT32_ID( int n )
      { 
	 return n;
      }
      
   public static long BGL_INT64_ID( long n )
      { 
	 return n;
      }
      
   public static int bgl_bignum_to_long(bignum n)
      {
	 return n.value.intValue();
      }

   public static long bgl_bignum_to_llong(bignum n)
      {
	 return n.value.longValue();
      }

   public static bignum ELONG_TO_BIGNUM(long n)
      {
	 return new bignum(n);
      }

   public static bignum LLONG_TO_BIGNUM(long n)
      {
	 return new bignum(n);
      }

   public static bignum FLONUM_TO_BIGNUM(double n)
      {
	 return new bignum(n);
      }

   public static double BIGNUM_TO_FLONUM(bignum n)
      {
	 return n.value.doubleValue();
      }

   public static bint BINT(long v)
      {
	 if ((-100 <= v) && (v < 2018))
	    return bint_allocated[(int) v + 100];
	 return new bint((int) v);
      }

   public static bint BINT(int v)
      {
	 if ((-100 <= v) && (v < 2018))
	    return bint_allocated[v + 100];
	 return new bint(v);
      }
   public static bint BINT(byte v)
      {
	 return BINT( (int)v );
      }
   public static bint BINT(short v)
      {
	 return BINT( (int)v );
      }

   public static int CINT(bint v)
      {
	 return v.value;
      }

   public static byte[] bgl_bignum_to_string(final bignum n, final int r)
      {
	 return n.value.toString(r).getBytes();
      }

   public static bignum bgl_string_to_bignum(final byte[] s, final int r)
      {
	 return new bignum(new java.math.BigInteger(new String(s), r));
      }

   public static bignum bgl_string_to_bignum(final String s)
      {
	 return new bignum(new java.math.BigInteger(s));
      }

   public static bignum bgl_string_to_bignum(final String s, final int r)
      {
	 return new bignum(new java.math.BigInteger(s, r));
      }

   public static Object bgl_string_to_integer_obj(final byte[] s, final int r)
      {
	 final String str = ((s.length > 0) && s[ 0 ] == '+') ?
	    new String(s, 1, s.length - 1) :
	    new String(s);
	 Object res;
      
	 try {
	    res = new bint(Integer.parseInt(str, r));
	 } catch (NumberFormatException e) {
	    res = new bignum(new java.math.BigInteger(str, r));
	 }

	 return res;
      }

   // Open functions
   public static boolean EQ_FX(int n1, int n2)
      {
	 return (n1 == n2);
      }

   public static boolean EQ_ELONG(long n1, long n2)
      {
	 return (n1 == n2);
      }

   public static boolean EQ_LLONG(long n1, long n2)
      {
	 return (n1 == n2);
      }

   public static boolean EQ_INT8(byte n1, byte n2)
      {
	 return (n1 == n2);
      }
   public static boolean EQ_INT16(short n1, short n2)
      {
	 return (n1 == n2);
      }
   public static boolean EQ_INT32(int n1, int n2)
      {
	 return (n1 == n2);
      }
   public static boolean EQ_INT64(long n1, long n2)
      {
	 return (n1 == n2);
      }

   public static boolean EQ_BIGNUM(bignum n1, bignum n2)
      {
	 return n1.value.compareTo(n2.value) == 0;
      }

   public static boolean LT_FX(int n1, int n2)
      {
	 return (n1 < n2);
      }

   public static boolean LT_ELONG(long n1, long n2)
      {
	 return (n1 < n2);
      }

   public static boolean LT_LLONG(long n1, long n2)
      {
	 return (n1 < n2);
      }

   public static boolean LT_INT8(byte n1, byte n2)
      {
	 return (n1 < n2);
      }
   public static boolean LT_INT16(short n1, short n2)
      {
	 return (n1 < n2);
      }
   public static boolean LT_INT32(int n1, int n2)
      {
	 return (n1 < n2);
      }
   public static boolean LT_INT64(long n1, long n2)
      {
	 return (n1 < n2);
      }

   public static boolean LT_BIGNUM(bignum n1, bignum n2)
      {
	 return n1.value.compareTo(n2.value) == -1;
      }

   public static boolean LE_FX(int n1, int n2)
      {
	 return (n1 <= n2);
      }

   public static boolean LE_ELONG(long n1, long n2)
      {
	 return (n1 <= n2);
      }

   public static boolean LE_LLONG(long n1, long n2)
      {
	 return (n1 <= n2);
      }

   public static boolean LE_INT8(byte n1, byte n2)
      {
	 return (n1 <= n2);
      }
   public static boolean LE_INT16(short n1, short n2)
      {
	 return (n1 <= n2);
      }
   public static boolean LE_INT32(int n1, int n2)
      {
	 return (n1 <= n2);
      }
   public static boolean LE_INT64(long n1, long n2)
      {
	 return (n1 <= n2);
      }

   public static boolean LE_BIGNUM(bignum n1, bignum n2)
      {
	 return n1.value.compareTo(n2.value) <= 0;
      }

   public static boolean GT_FX(int n1, int n2)
      {
	 return (n1 > n2);
      }

   public static boolean GT_ELONG(long n1, long n2)
      {
	 return (n1 > n2);
      }

   public static boolean GT_LLONG(long n1, long n2)
      {
	 return (n1 > n2);
      } 

   public static boolean GT_INT8(byte n1, byte n2)
      {
	 return (n1 > n2);
      }
   public static boolean GT_INT16(short n1, short n2)
      {
	 return (n1 > n2);
      }
   public static boolean GT_INT32(int n1, int n2)
      {
	 return (n1 > n2);
      }
   public static boolean GT_INT64(long n1, long n2)
      {
	 return (n1 > n2);
      }

   public static boolean GT_BIGNUM(bignum n1, bignum n2)
      {
	 return n1.value.compareTo(n2.value) == 1;
      }

   public static boolean GE_FX(int n1, int n2)
      {
	 return (n1 >= n2);
      }

   public static boolean GE_ELONG(long n1, long n2)
      {
	 return (n1 >= n2);
      }

   public static boolean GE_LLONG(long n1, long n2)
      {
	 return (n1 >= n2);
      }

   public static boolean GE_INT8(byte n1, byte n2)
      {
	 return (n1 >= n2);
      }
   public static boolean GE_INT16(short n1, short n2)
      {
	 return (n1 >= n2);
      }
   public static boolean GE_INT32(int n1, int n2)
      {
	 return (n1 >= n2);
      }
   public static boolean GE_INT64(long n1, long n2)
      {
	 return (n1 >= n2);
      }

   public static boolean GE_BIGNUM(bignum n1, bignum n2)
      {
	 return n1.value.compareTo(n2.value) >= 0;
      }

   public static int CMP_BIGNUM(bignum n1, bignum n2)
      {
	 return n1.value.compareTo(n2.value);
      }

   public static boolean ZEROP_BIGNUM(bignum n)
      {
	 return n.value.signum() == 0;
      }

   public static boolean POSITIVEP_BIGNUM(bignum n)
      {
	 return n.value.signum() == 1;
      }

   public static boolean NEGATIVEP_BIGNUM(bignum n)
      {
	 return n.value.signum() == -1;
      }

   public static boolean EVENP_FX(int n)
      {
	 return ((n & 1) == 0);
      }

   public static boolean EVENP_BIGNUM(bignum n)
      {
	 return ! n.value.testBit(0);
      }

   public static boolean ODDP_FX(int n)
      {
	 return ((n & 1) != 0);
      }

   public static boolean ODDP_BIGNUM(bignum n)
      {
	 return n.value.testBit(0);
      }

   public static bignum ABS_BIGNUM(bignum n)
      {
	 return new bignum(n.value.abs());
      }

   public static int PLUS_FX(int n1, int n2)
      {
	 return (n1 + n2);
      }

   public static long PLUS_ELONG(long n1, long n2)
      {
	 return (n1 + n2);
      }

   public static long PLUS_LLONG(long n1, long n2)
      {
	 return (n1 + n2);
      }

   public static byte PLUS_S8(byte n1, byte n2)
      {
	 return (byte)(n1 + n2);
      }

   public static short PLUS_S16(short n1, short n2)
      {
	 return (short)(n1 + n2);
      }

   public static int PLUS_S32(int n1, int n2)
      {
	 return (n1 + n2);
      }

   public static long PLUS_S64(long n1, long n2)
      {
	 return (n1 + n2);
      }

   public static bignum PLUS_BIGNUM(bignum n1, bignum n2)
      {
	 return new bignum(n1.value.add(n2.value));
      }

   public static Object SAFE_PLUS_FX(int n1, int n2)
      {
	 int x = n1 + n2;
	 if (Integer_signum(n1) == Integer_signum(n2) &&
	     Integer_signum(n1) != Integer_signum(x))
	    return PLUS_BIGNUM(new bignum(n1), new bignum(n2));
	 else
	    return new bint(x);
      }

   public static Object SAFE_PLUS_ELONG(long n1, long n2)
      {
	 long x = n1 + n2;
	 if (Long_signum(n1) == Long_signum(n2) &&
	     Long_signum(n1) != Long_signum(x))
	    return PLUS_BIGNUM(new bignum(n1), new bignum(n2));
	 else
	    return new belong(x);
      }

   public static Object SAFE_PLUS_LLONG(long n1, long n2)
      {
	 long x = n1 + n2;
	 if (Long_signum(n1) == Long_signum(n2) &&
	     Long_signum(n1) != Long_signum(x))
	    return PLUS_BIGNUM(new bignum(n1), new bignum(n2));
	 else
	    return new bllong(x);
      }

   public static int MINUS_FX(int n1, int n2)
      {
	 return (n1 - n2);
      }

   public static long MINUS_ELONG(long n1, long n2)
      {
	 return (n1 - n2);
      }

   public static long MINUS_LLONG(long n1, long n2)
      {
	 return (n1 - n2);
      }

   public static byte MINUS_S8(byte n1, byte n2)
      {
	 return (byte)(n1 - n2);
      }

   public static short MINUS_S16(short n1, short n2)
      {
	 return (short)(n1 - n2);
      }

   public static int MINUS_S32(int n1, int n2)
      {
	 return (n1 - n2);
      }

   public static long MINUS_S64(long n1, long n2)
      {
	 return (n1 - n2);
      }

   public static bignum MINUS_BIGNUM(bignum n1, bignum n2)
      {
	 return new bignum(n1.value.subtract(n2.value));
      }

   public static Object SAFE_MINUS_FX(int n1, int n2)
      {
	 int x = n1 - n2;
	 if (Integer_signum(n1) == Integer_signum(n2) &&
	     Integer_signum(n1) != Integer_signum(x) &&
	     Integer_signum(x) != 0)
	    return MINUS_BIGNUM(new bignum(n1), new bignum(n2));
	 else
	    return new bint(x);
      }

   public static Object SAFE_MINUS_ELONG(long n1, long n2)
      {
	 long x = n1 - n2;
	 if (Long_signum(n1) == Long_signum(n2) &&
	     Long_signum(n1) != Long_signum(x) &&
	     Long_signum(x) != 0)
	    return MINUS_BIGNUM(new bignum(n1), new bignum(n2));
	 else
	    return new belong(x);
      }

   public static Object SAFE_MINUS_LLONG(long n1, long n2)
      {
	 long x = n1 - n2;
	 if (Long_signum(n1) == Long_signum(n2) &&
	     Long_signum(n1) != Long_signum(x) &&
	     Long_signum(x) != 0)
	    return MINUS_BIGNUM(new bignum(n1), new bignum(n2));
	 else
	    return new bllong(x);
      }

   public static int MUL_FX(int n1, int n2)
      {
	 return (n1 * n2);
      }

   public static long MUL_ELONG(long n1, long n2)
      {
	 return (n1 * n2);
      }

   public static long MUL_LLONG(long n1, long n2)
      {
	 return (n1 * n2);
      }

   public static byte MUL_S8(byte n1, byte n2)
      {
	 return (byte)(n1 * n2);
      }

   public static short MUL_S16(short n1, short n2)
      {
	 return (short)(n1 * n2);
      }

   public static int MUL_S32(int n1, int n2)
      {
	 return (n1 * n2);
      }

   public static long MUL_S64(long n1, long n2)
      {
	 return (n1 * n2);
      }

   public static bignum MUL_BIGNUM(bignum n1, bignum n2)
      {
	 return new bignum(n1.value.multiply(n2.value));
      }

   public static Object SAFE_MUL_FX(int n1, int n2)
      {
	 if( n2 == 0 ) {
	    return BINT( 0 );
	 } else {
	    int x = n1 * n2;

	    if (x / n2 == n1)
	       return BINT( x );
	    else
	       return MUL_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

   public static Object SAFE_MUL_ELONG(long n1, long n2)
      {
	 if( n2 == 0 ) {
	    return ELONG_TO_BELONG( 0 );
	 } else {
	    long x = n1 * n2;

	    if (x / n2 == n1)
	       return ELONG_TO_BELONG( x );
	    else
	       return MUL_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

   public static Object SAFE_MUL_LLONG(long n1, long n2)
      {
	 if( n2 == 0 ) {
	    return LLONG_TO_BLLONG( 0 );
	 } else {
	    long x = n1 * n2;
	    if (x / n2 == n1)
	       return LLONG_TO_BLLONG( x );
	    else
	       return MUL_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

   public static int DIV_FX(int n1, int n2)
      {
	 return (n1 / n2);
      }

   public static long DIV_ELONG(long n1, long n2)
      {
	 return (n1 / n2);
      }

   public static long DIV_LLONG(long n1, long n2)
      {
	 return (n1 / n2);
      }

   public static byte DIV_S8(byte n1, byte n2)
      {
	 return (byte)(n1 / n2);
      }

   public static short DIV_S16(short n1, short n2)
      {
	 return (short)(n1 / n2);
      }

   public static int DIV_S32(int n1, int n2)
      {
	 return (n1 / n2);
      }

   public static long DIV_S64(long n1, long n2)
      {
	 return (n1 / n2);
      }

   public static Object BGL_SAFE_BX_TO_FX(Object o)
      {
	 if( ((bignum)o).value.bitLength() < 32 ) {
	    return BINT( ((bignum)o).value.intValue() );
	 }
	 else
	    return o;
      }
   
   public static Object SAFE_DIV_FX(int n1, int n2)
      {
	 if (n1 == Integer.MIN_VALUE && n2 == -1)
	    return NEG_BIGNUM(new bignum(n1));
	 else
	    return new bint(n1 / n2);
      }

   public static Object SAFE_DIV_ELONG(long n1, long n2)
      {
	 if (n1 == Long.MIN_VALUE && n2 == -1)
	    return NEG_BIGNUM(new bignum(n1));
	 else
	    return new belong(n1 / n2);
      }

   public static Object SAFE_DIV_LLONG(long n1, long n2)
      {
	 if (n1 == Long.MIN_VALUE && n2 == -1)
	    return NEG_BIGNUM(new bignum(n1));
	 else
	    return new bllong(n1 / n2);
      }

   public static int NEG_FX(int n)
      {
	 return -n;
      }

   public static long NEG_ELONG(long n)
      {
	 return -n;
      }

   public static long NEG_LLONG(long n)
      {
	 return -n;
      }

   public static bignum NEG_BIGNUM(bignum n)
      {
	 return new bignum(n.value.negate());
      }

   public static int QUOTIENT_FX(int n1, int n2)
      {
	 return (n1 / n2);
      }

   public static long QUOTIENT_ELONG(long n1, long n2)
      {
	 return (n1 / n2);
      }

   public static long QUOTIENT_LLONG(long n1, long n2)
      {
	 return (n1 / n2);
      }
   
   public static byte QUOTIENT_S8(byte n1, byte n2)
      {
	 return (byte)(n1 / n2);
      }

   public static short QUOTIENT_S16(short n1, short n2)
      {
	 return (short)(n1 / n2);
      }

   public static int QUOTIENT_S32(int n1, int n2)
      {
	 return (n1 / n2);
      }

   public static long QUOTIENT_S64(long n1, long n2)
      {
	 return (n1 / n2);
      }

   public static bignum QUOTIENT_BIGNUM(bignum n1, bignum n2)
      {
	 return new bignum(n1.value.divide(n2.value));
      }

   public static int REMAINDER_FX(int n1, int n2)
      {
	 return (n1 % n2);
      }

   public static long REMAINDER_ELONG(long n1, long n2)
      {
	 return (n1 % n2);
      }

   public static long REMAINDER_LLONG(long n1, long n2)
      {
	 return (n1 % n2);
      }

   public static byte REMAINDER_S8(byte n1, byte n2)
      {
	 return (byte)(n1 % n2);
      }

   public static short REMAINDER_S16(short n1, short n2)
      {
	 return (short)(n1 % n2);
      }

   public static int REMAINDER_S32(int n1, int n2)
      {
	 return (n1 % n2);
      }

   public static long REMAINDER_S64(long n1, long n2)
      {
	 return (n1 % n2);
      }

   public static bignum REMAINDER_BIGNUM(bignum n1, bignum n2)
      {
	 return new bignum(n1.value.remainder(n2.value));
      }

   public static bignum DIVREM_BIGNUM(bignum n1, bignum n2)
      {
	 bgldynamic env = BGL_CURRENT_DYNAMIC_ENV();
	 final java.math.BigInteger[] xy = n1.value.divideAndRemainder(n2.value);
	 env.mvalues_number = 2;
	 
	 env.mvalues_values[ 1 ] = new bignum(xy[1]);

	 return new bignum(xy[0]);
      }

   public static bignum GCD_BIGNUM(bignum n1, bignum n2)
      {
	 return new bignum(n1.value.gcd(n2.value));
      }

   public static bignum LCM_BIGNUM(bignum n1, bignum n2)
      {
	 return new bignum(n1.value.divide(n1.value.gcd(n2.value)).multiply(n2.value).abs());
      }

   public static int BITOR(int a1, int a2)
      {
	 return (a1 | a2);
      }

   public static long BITORELONG(long a1, long a2)
      {
	 return (a1 | a2);
      }

   public static long BITORLLONG(long a1, long a2)
      {
	 return (a1 | a2);
      }

   public static byte BITORINT8(byte a1, byte a2)
      {
	 return (byte)(a1 | a2);
      }

   public static short BITORINT16(short a1, short a2)
      {
	 return (short)(a1 | a2);
      }

   public static int BITORINT32(int a1, int a2)
      {
	 return (int)(a1 | a2);
      }

   public static long BITORINT64(long a1, long a2)
      {
	 return (long)(a1 | a2);
      }

   public static int BITAND(int a1, int a2)
      {
	 return (a1 & a2);
      }

   public static long BITANDELONG(long a1, long a2)
      {
	 return (a1 & a2);
      }

   public static long BITANDLLONG(long a1, long a2)
      {
	 return (a1 & a2);
      }

   public static byte BITANDINT8(byte a1, byte a2)
      {
	 return (byte)(a1 & a2);
      }

   public static short BITANDINT16(short a1, short a2)
      {
	 return (short)(a1 & a2);
      }

   public static int BITANDINT32(int a1, int a2)
      {
	 return (int)(a1 & a2);
      }

   public static long BITANDINT64(long a1, long a2)
      {
	 return (long)(a1 & a2);
      }

   public static int BITXOR(int a1, int a2)
      {
	 return (a1 ^ a2);
      }

   public static long BITXORELONG(long a1, long a2)
      {
	 return (a1 ^ a2);
      }

   public static long BITXORLLONG(long a1, long a2)
      {
	 return (a1 ^ a2);
      }

   public static byte BITXORINT8(byte a1, byte a2)
      {
	 return (byte)(a1 ^ a2);
      }

   public static short BITXORINT16(short a1, short a2)
      {
	 return (short)(a1 ^ a2);
      }

   public static int BITXORINT32(int a1, int a2)
      {
	 return (int)(a1 ^ a2);
      }

   public static long BITXORINT64(long a1, long a2)
      {
	 return (long)(a1 ^ a2);
      }

   public static int BITNOT(int a)
      {
	 return ~a;
      }

   public static long BITNOTELONG(long a)
      {
	 return ~a;
      }

   public static long BITNOTLLONG(long a)
      {
	 return ~a;
      }

   public static byte BITNOTINT8(byte a)
      {
	 return (byte)~a;
      }

   public static short BITNOTINT16(short a)
      {
	 return (short)~a;
      }

   public static int BITNOTINT32(int a)
      {
	 return (int)~a;
      }

   public static long BITNOTINT64(long a)
      {
	 return (long)~a;
      }

   public static int BITRSH(int a1, int a2)
      {
	 return (a1 >> a2);
      }

   public static int BITURSH(int a1, int a2)
      {
	 return (a1 >>> a2);
      }

   public static long BITRSHELONG(long a1, int a2)
      {
	 return (a1 >> a2);
      }

   public static long BITURSHELONG(long a1, int a2)
      {
	 return (a1 >>> a2);
      }

   public static long BITRSHLLONG(long a1, int a2)
      {
	 return (a1 >> a2);
      }

   public static long BITURSHLLONG(long a1, int a2)
      {
	 return (a1 >>> a2);
      }

   public static int BITLSH(int a1, int a2)
      {
	 return (a1 << a2);
      }

   public static long BITLSHELONG(long a1, int a2)
      {
	 return (a1 << a2);
      }

   public static long BITLSHLLONG(long a1, int a2)
      {
	 return (a1 << a2);
      }

   public static byte BITLSHINT8(byte a1, int a2)
      {
	 return (byte)(a1 << a2);
      }

   public static short BITLSHINT16(short a1, int a2)
      {
	 return (short)(a1 << a2);
      }

   public static int BITLSHINT32(int a1, int a2)
      {
	 return (int)(a1 << a2);
      }

   public static long BITLSHINT64(long a1, int a2)
      {
	 return (long)(a1 << a2);
      }

   public static byte BITRSHINT8(byte a1, int a2)
      {
	 return (byte)(a1 >> a2);
      }

   public static short BITRSHINT16(short a1, int a2)
      {
	 return (short)(a1 >> a2);
      }

   public static int BITRSHINT32(int a1, int a2)
      {
	 return (int)(a1 >> a2);
      }

   public static long BITRSHINT64(long a1, int a2)
      {
	 return (a1 >> a2);
      }

   public static byte BITURSHINT8(byte a1, int a2)
      {
	 return (byte)(a1 >>> a2);
      }

   public static short BITURSHINT16(short a1, int a2)
      {
	 return (short)(a1 >>> a2);
      }

   public static int BITURSHINT32(int a1, int a2)
      {
	 return (int)(a1 >>> a2);
      }

   public static long BITURSHINT64(long a1, int a2)
      {
	 return (a1 >>> a2);
      }

   //////
   // FLOAT
   //////
   // Predicates
   public static final double BGL_NAN = Double.NaN;
   public static final double BGL_INFINITY = Double.POSITIVE_INFINITY;

   public static boolean REALP(Object o)
      {
	 return (o instanceof real);
      }

   // Conversions
   public static double REAL_TO_DOUBLE(real n)
      {
	 return n.value;
      }

   public static float REAL_TO_FLOAT(real n)
      {
	 return (float) n.value;
      }

   public static real DOUBLE_TO_REAL(double n)
      {
	 return new real(n);
      }

   public static real FLOAT_TO_REAL(float n)
      {
	 return new real((double) n);
      }

   public static float DOUBLE_TO_FLOAT(double n)
      {
	 return (float) n;
      }

   public static double FLOAT_TO_DOUBLE(float n)
      {
	 return (double) n;
      }

   public static double FIXNUM_TO_FLONUM(int n)
      {
	 return (double) n;
      }

   public static int FLONUM_TO_FIXNUM(double n)
      {
	 return (int) n;
      }

   public static double ELONG_TO_FLONUM(long n)
      {
	 return (double) n;
      }

   public static long FLONUM_TO_ELONG(double n)
      {
	 return (long) n;
      }

   public static double LLONG_TO_FLONUM(long n)
      {
	 return (double) n;
      }

   public static long FLONUM_TO_LLONG(double n)
      {
	 return (long) n;
      }

   public static long BGL_DOUBLE_TO_INT64(double n)
      {
	 return (long) n;
      }

   public static double BGL_INT64_TO_DOUBLE(long n)
      {
	 return (long) n;
      }

   public static long DOUBLE_TO_LLONG_BITS(double n)
      {
	 return Double.doubleToLongBits( n );
      }
   public static double LLONG_BITS_TO_DOUBLE(long n)
      {
	 return Double.longBitsToDouble( n );
      }

   public static int FLOAT_TO_INT_BITS(float n)
      {
	 return Float.floatToIntBits( n );
      }
   public static float INT_BITS_TO_FLOAT(int n)
      {
	 return Float.intBitsToFloat( n );
      }

   public static double RANDOMFL()
      {
	 return (double) randg.nextDouble();
      }
   
   // Open functions
   public static boolean EQ_FL(double n1, double n2)
      {
	 return (n1 == n2);
      }

   public static boolean LT_FL(double n1, double n2)
      {
	 return (n1 < n2);
      }

   public static boolean LE_FL(double n1, double n2)
      {
	 return (n1 <= n2);
      }

   public static boolean GT_FL(double n1, double n2)
      {
	 return (n1 > n2);
      }

   public static boolean GE_FL(double n1, double n2)
      {
	 return (n1 >= n2);
      }

   public static double PLUS_FL(double n1, double n2)
      {
	 return (n1 + n2);
      }

   public static double MINUS_FL(double n1, double n2)
      {
	 return (n1 - n2);
      }

   public static double MUL_FL(double n1, double n2)
      {
	 return (n1 * n2);
      }

   public static double DIV_FL(double n1, double n2)
      {
	 return (n1 / n2);
      }

   public static double NEG_FL(double n)
      {
	 return -n;
      }

   public static double fmod(double n1, double n2)
      {
	 return (n1 % n2);
      }

   public static double floor(double n)
      {
	 return Math.floor(n);
      }

   public static double ceil(double n)
      {
	 return Math.ceil(n);
      }

   public static double exp(double n)
      {
	 return Math.exp(n);
      }

   public static double log(double n)
      {
	 return Math.log(n);
      }

   public static double sin(double n)
      {
	 return Math.sin(n);
      }

   public static double cos(double n)
      {
	 return Math.cos(n);
      }

   public static double tan(double n)
      {
	 return Math.tan(n);
      }

   public static double asin(double n)
      {
	 return Math.asin(n);
      }

   public static double acos(double n)
      {
	 return Math.acos(n);
      }

   public static double atan(double n)
      {
	 return Math.atan(n);
      }

   public static double atan2(double n1, double n2)
      {
	 return Math.atan2(n1, n2);
      }

   public static double sqrt(double n)
      {
	 return Math.sqrt(n);
      }

   public static double pow(double n1, double n2)
      {
	 return Math.pow(n1, n2);
      }

   public static double abs(double n)
      {
	 return Math.abs(n);
      }

   public static double max(double n1, double n2)
      {
	 return Math.max(n1, n2);
      }

   public static double min(double n1, double n2)
      {
	 return Math.min(n1, n2);
      }

   public static int BGL_SIGNBIT(double n)
      {
	 return (int) (Double.doubleToLongBits(n) >> 63);
      }

   public static double round(double n)
      {
	 return Math.round(n);
      }

   public static boolean isfinite(double n)
      {
	 return !( Double.isInfinite( n ) || Double.isNaN( n ) );
      }

   public static boolean isinf(double n)
      {
	 return Double.isInfinite(n);
      }

   public static boolean isnan(double n)
      {
	 return Double.isNaN(n);
      }

   //////
   // OBJECT CONSTANTS
   //////
   public static final Object BUNSPEC = unspecified.unspecified;
   public static final Object BNIL = nil.nil;
   public static final Object BEOA = BNIL;
   public static final Object BEOF = eof.eof;
   public static final Object BOPTIONAL = optional.optional;
   public static final Object BREST = rest.rest;
   public static final Object BKEY = key.key;
   public static final int BDB_LIBRARY_MAGIC_NUMBER = 0x1024;

   public static boolean EOF_OBJECTP(Object o)
      {
	 return (o == BEOF);
      }

   public static boolean NULLP(Object o)
      {
	 return (o == BNIL);
      }

   public static int CCNST(cnst o)
      {
	 return o.value;
      }

   public static int CCNST(Object o)
      {
	 // CARE !! J'arrive pas a changer ce !@?# type dans intext et symbol
	 return ((cnst) o).value;
      }

   public static Object BCNST(int v)
      {
	 return new cnst(v);
      }

   public static boolean CNSTP(Object o)
      {
	 return ((o instanceof bchar) || (o instanceof cnst));
      }

   //////
   // STANGE TYPES (no constructors invoked)

   public static boolean POINTERP(Object o)
      {
	 return true;
      }

   public static boolean OPAQUEP(Object o)
      {
	 return false;
      }

   public static Object BGL_OPAQUE_NIL() 
      {
	 return null;
      }

   public static boolean BGL_OBJECTP(Object o)
      {
	 return (o instanceof object);
      }

   //////
   // Unicode characters
   //////
   public static boolean UCS2P(Object o)
      {
	 return (o instanceof bucs2);
      }

   public static bucs2 BUCS2(char c)
      {
	 return new bucs2(c);
      }

   public static char CUCS2(bucs2 c)
      {
	 return c.value;
      }

   public static int CUCS2(char c)
      {
	 return (c & 0xFFFF);
      }

   public static char BGL_INT_TO_UCS2(int n)
      {
	 return (char) n;
      }

   public static boolean UCS2_EQ(char c1, char c2)
      {
	 return (c1 == c2);
      }

   public static boolean UCS2_GT(char c1, char c2)
      {
	 return (c1 > c2);
      }

   public static boolean UCS2_LT(char c1, char c2)
      {
	 return (c1 < c2);
      }

   public static boolean UCS2_GE(char c1, char c2)
      {
	 return (c1 >= c2);
      }

   public static boolean UCS2_LE(char c1, char c2)
      {
	 return (c1 <= c2);
      }

   public static boolean ucs2_letterp(char c)
      {
	 return Character.isLetter(c);
      }

   public static boolean ucs2_digitp(char c)
      {
	 return Character.isDigit(c);
      }

   public static boolean ucs2_whitespacep(char c)
      {
	 return Character.isSpaceChar(c);
      }

   public static boolean ucs2_lowerp(char c)
      {
	 return Character.isLowerCase(c);
      }

   public static boolean ucs2_upperp(char c)
      {
	 return Character.isUpperCase(c);
      }

   public static boolean ucs2_definedp(int c)
      {
	 return Character.isDefined((char) c);
      }

   public static char ucs2_toupper(char c)
      {
	 return Character.toUpperCase(c);
      }

   public static char ucs2_tolower(char c)
      {
	 return Character.toLowerCase(c);
      }

   //////
   // Unicode strings
   //////
   public static boolean UCS2_STRINGP(Object o)
      {
	 return (o instanceof char[]);
      }

   public static int UCS2_STRING_LENGTH(char[]o)
      {
	 return o.length;
      }

   public static char[] make_ucs2_string(int len, char c)
      {
	 final char[] result = new char[len];

	 for (int i = 0; i < len; ++i)
	    result[i] = c;

	 return result;
      }

   public static char UCS2_STRING_REF(char[]o, int i)
      {
	 return o[i];
      }

   public static Object UCS2_STRING_SET(char[]o, int i, char c)
      {
	 o[i] = c;
	 return unspecified.unspecified;
      }

   public static boolean ucs2_strcmp(char[]o1, char[]o2)
      {
	 return (new String(o1)).equals(new String(o2));
      }

   public static boolean ucs2_strcicmp(char[]o1, char[]o2)
      {
	 return (new String(o1)).equalsIgnoreCase(new String(o2));
      }

   public static boolean ucs2_string_lt(char[]o1, char[]o2)
      {
	 return ((new String(o1)).compareTo(new String(o2)) < 0);
      }

   public static boolean ucs2_string_gt(char[]o1, char[]o2)
      {
	 return ((new String(o1)).compareTo(new String(o2)) > 0);
      }

   public static boolean ucs2_string_le(char[]o1, char[]o2)
      {
	 return ((new String(o1)).compareTo(new String(o2)) <= 0);
      }

   public static boolean ucs2_string_ge(char[]o1, char[]o2)
      {
	 return ((new String(o1)).compareTo(new String(o2)) >= 0);
      }

   public static boolean ucs2_string_cilt(char[]o1, char[]o2)
      {
	 // !!!!! JDK 1.2:  return ((new String( o1 )).compareToIgnoreCase( new String( o2 ) ) < 0);
	 return ((new String(o1)).toLowerCase().compareTo((new String(o2)).
							  toLowerCase()) < 0);
      }

   public static boolean ucs2_string_cigt(char[]o1, char[]o2)
      {
	 // !!!!! JDK 1.2:  return ((new String( o1 )).compareToIgnoreCase( new String( o2 ) ) > 0);
	 return ((new String(o1)).toLowerCase().compareTo((new String(o2)).
							  toLowerCase()) > 0);
      }

   public static boolean ucs2_string_cile(char[]o1, char[]o2)
      {
	 // !!!!! JDK 1.2:  return ((new String( o1 )).compareTo( new String( o2 ) ) <= 0);
	 return ((new String(o1)).toLowerCase().compareTo((new String(o2)).
							  toLowerCase()) <= 0);
      }

   public static boolean ucs2_string_cige(char[]o1, char[]o2)
      {
	 // !!!!! JDK 1.2:  return ((new String( o1 )).compareToIgnoreCase( new String( o2 ) ) >= 0);
	 return ((new String(o1)).toLowerCase().compareTo((new String(o2)).
							  toLowerCase()) >= 0);
      }

   public static char[] c_subucs2_string(char[]o, int min, int max)
      {
	 return (new String(o)).substring(min, max).toCharArray();
      }

   public static char[] c_ucs2_string_copy(char[]o)
      {
	 return (new String(o)).toCharArray();
      }

   public static char[] c_ucs2_string_append(char[]o1, char[]o2)
      {
	 final char[] result = new char[o1.length + o2.length];

	 System.arraycopy(o1, 0, result, 0, o1.length);
	 System.arraycopy(o2, 0, result, o1.length, o2.length);

	 return result;
      }

   public static byte[] ucs2_string_to_utf8_string(char[]o)
      {
	 return ucs2_string_to_utf8_string(new String(o));
      }

   public static byte[] ucs2_string_to_utf8_string(String s)
      {
	 try {
	    final ByteArrayOutputStream bout = new ByteArrayOutputStream();
	    final DataOutputStream out = new DataOutputStream(bout);

	    out.writeUTF(s);

	    final byte[] tmp = bout.toByteArray();
	    final byte[] result = new byte[tmp.length - 2];

	    System.arraycopy(tmp, 2, result, 0, result.length);
	    bout.close();

	    return result;
	 } catch(Exception e) {
	    fail("ucs2_string_to_utf8_string", e.getMessage(), s);
	    return null;
	 }
      }

   public static int utf8length(byte[]bytes, int nb)
      {
	 int result = 0;
	 int i = 0;

	 for (; i < nb; ++result)
	 {
	    final byte b = bytes[i];

	    if ((b & 0x80) == 0)
	       ++i;
	    else if ((b & 0x20) == 0)
	       i += 2;
	    else
	       i += 3;
	 }

	 if (i != nb)
	    throw new RuntimeException("Bad utf8 string : " + new String(bytes));

	 return result;
      }

   public static char[] utf8_string_to_ucs2_string(byte[]bytes)
      {
	 final int nb = bytes.length;
	 final int nc = utf8length(bytes, nb);
	 final char[] chars = new char[nc];
	 int i = 0;

	 for (int j = 0; j < nc; ++j)
	 {
	    final byte b = bytes[i];

	    if ((b & 0x80) == 0)
	    {
	       chars[j] = (char) b;
	       ++i;
	    }
	    else if ((b & 0x20) == 0)
	    {
	       chars[j] = (char) (((b & 0x1F) << 6) | (bytes[i + 1] & 0x3F));
	       i += 2;
	    }
	    else
	    {
	       chars[j] =
		  (char) (((b & 0x0F) << 12)
			  | ((bytes[i + 1] & 0x3F) << 6) | (bytes[i + 2] & 0x3F));
	       i += 3;
	    }
	 }

	 return chars;
      }

   public static int bgl_utf8_string_locale_compare3( byte[] left, byte[] right ) {
      return 0;
   }

   
   //////
   // PROCESS
   //////
   public static boolean PROCESSP(Object o)
      {
	 return (o instanceof process);
      }

   public static process bgl_process_nil()
      {
	 return new process();
      }

   public static process c_run_process(Object bhost,
				       Object bfork,
				       Object bwaiting,
				       Object binput,
				       Object boutput,
				       Object berror,
				       byte[]bcommand, Object bargs, Object benv) throws IOException
      {
	 return new process(((bhost instanceof byte[])? (byte[])bhost : null),
			    // !!!! can't directly cast to byte[] ?????
			    bfork != bbool.faux,
			    bwaiting != bbool.faux,
			    binput, boutput, berror, bcommand, bargs, benv);
      }

   public static Object c_unregister_process(process o)
      {
	 return bigloo.foreign.BUNSPEC;
      }

   public static int PROCESS_PID(process o)
      {
	 return o.pid();
      }

   public static Object PROCESS_INPUT_PORT(process o)
      {
	 return o.input_port;
      }

   public static Object PROCESS_OUTPUT_PORT(process o)
      {
	 return o.output_port;
      }

   public static Object PROCESS_ERROR_PORT(process o)
      {
	 return o.error_port;
      }

   public static boolean c_process_alivep(process o)
      {
	 return o.alivep();
      }

   public static Object c_process_wait(process o)
      {
	 o.waitfor();
	 return bigloo.foreign.BUNSPEC;
      }

   public static Object c_process_xstatus(process o)
      {
	 return o.xstatus();
      }

   public static Object c_process_send_signal(process o, int s)
      {
	 return o.send_signal(s);
      }

   public static Object c_process_kill(process o)
      {
	 return o.kill();
      }

   public static Object c_process_stop(process o)
      {
	 return o.stop();
      }

   public static Object c_process_continue(process o)
      {
	 return o.cont();
      }

   public static Object c_process_list()
      {
	 return bigloo.process.process_list();
      }

   //////
   // STRING
   //////
   // Predicates
   public static boolean STRINGP(Object o)
      {
	 return (o instanceof byte[]);
      }

   // Conversions
   public static byte[] jstring_to_bstring(String o)
      {
	 final int len = o.length();
	 byte[] res = new byte[ len ];

	 for( int i = 0; i < len; i++ ) {
	    res[ i ] = (byte)o.charAt( i );
	 }

	 return res;
      }
   
   public static byte[] string_to_bstring(byte[]o)
      {
	 return o;
      }

    public static byte[] string_to_bstring_len(byte[] o, int len)
      {
         return o;
      }

   public static byte[] BSTRING_TO_STRING(byte[]o)
      {
	 return o;
      }

   public static int strtol(byte[]s, int i, int radix)
      {
	 final int len = s.length;

	 return ((len == 0) ? 0 : parseint(s, i, len, radix));
      }

   public static int strtoul(byte[]s, int i, int radix)
      {
	 final int len = s.length;

	 return ((len == 0) ? 0 : parseint(s, i, len, radix));
      }

   public static long strtoll(byte[]s, int i, int radix)
      {
	 final int len = s.length;

	 return ((len == 0) ? 0 : parselong(s, i, len, radix));
      }

   public static byte[] integer_to_string(int n, int radix)
      {
	 // CARE open this code to avoid triple allocation...
	 return Integer.toString(n, radix).getBytes();
      }

   public static byte[] unsigned_to_string(int n, int radix)
      {
	 // CARE open this code to avoid triple allocation...
	 switch( radix ) {
	    case 2:
	       return Integer.toBinaryString(n).getBytes();
	    case 8:
	       return Integer.toOctalString(n).getBytes();
	    default:
	       return Integer.toHexString(n).getBytes();
	 }
      }

   public static byte[] uelong_to_string(long n, int radix)
      {
	 // CARE open this code to avoid triple allocation...
	 switch( radix ) {
	    case 2:
	       return Long.toBinaryString(n).getBytes();
	    case 8:
	       return Long.toOctalString(n).getBytes();
	    default:
	       return Long.toHexString(n).getBytes();
	 }
      }

   public static byte[] ullong_to_string(long n, int radix)
      {
	 // CARE open this code to avoid triple allocation...
	 switch( radix ) {
	    case 2:
	       return Long.toBinaryString(n).getBytes();
	    case 8:
	       return Long.toOctalString(n).getBytes();
	    default:
	       return Long.toHexString(n).getBytes();
	 }
      }

   public static byte[] integer_to_string_padding(int n, int padding, int radix)
      {
	 byte[] tmp = Integer.toString(n < 0 ? -n : n, radix).getBytes();
	 if( tmp.length < padding ) {
	    byte[] result = new byte[ padding ];
	    System.arraycopy(tmp, 0, result, (padding-tmp.length), tmp.length);
	 
	    for( int i = (padding - tmp.length) - 1; i >= 0; i-- ) {
	       result[ i ] = '0';
	    }
	 
	    if( n < 0 ) result[ 0 ] = '-';
	 
	    return result;
	 } else {
	    return tmp;
	 }
      }

   public static byte[] elong_to_string(long n, int radix)
      {
	 // CARE open this code to avoid triple allocation...
	 return Long.toString(n, radix).getBytes();
      }

   public static byte[] llong_to_string(long n, int radix)
      {
	 // CARE open this code to avoid triple allocation...
	 return Long.toString(n, radix).getBytes();
      }

   public static double strtod(byte[]s, int i)
      {
	 // !!!!! JDK 1.2:  return Double.parseDouble( new String( s, i, (s.length - i) ) );

	 final int len = s.length;

	 return ((len == 0)
		 ? 0 : Double.valueOf(new String(s, i, (len - i))).doubleValue());
      }

   public static double strtod(byte[]s)
      {
	 // !!!!! JDK 1.2:  return Double.parseDouble( new String( s, i, (s.length - i) ) );

	 return strtod( s, 0 );
      }

   // Open functions
   public static int STRING_REF(byte[]s, int i)
      {
	 return (s[i] & 0xFF);
      }

   public static Object STRING_SET(byte[]s, int i, int cn)
      {
	 s[i] = (byte) cn;
	 return unspecified.unspecified;
      }

   // Lib functions
   public static int STRING_LENGTH(byte[]s)
      {
	 return s.length;
      }

   // Creations
   public static byte[] make_string(int n, int init)
      {
	 final byte[] r = new byte[n];
	 final byte in = (byte) init;

	 for (int i = 0; i < n; ++i)
	    r[i] = in;

	 return r;
      }

   public static byte[] make_string_sans_fill(int n)
      {
	 return new byte[n];
      }

   public static byte[] bgl_string_shrink(byte[]src, int len)
      {
	 return c_substring(src, 0, len);
      }

   public static byte[] c_substring(byte[]src, int min, int max)
      {
	 final int len = max - min;
	 final byte[] result = new byte[len];

	 // CARE Do a bench to find a better constant
	 if (len < 4)
	    for (int i = 0; i < len; ++i)
	       result[i] = src[min + i];
	 else
	    System.arraycopy(src, min, result, 0, len);

	 return result;
      }

   public static Object ill_char_rep(int cn)
      {
	 final byte[] r = new byte[5];

	 r[0] = (byte) '#';
	 r[1] = (byte) 'a';
	 r[2] = (byte) (((byte) '0') + (cn / 100));
	 r[3] = (byte) (((byte) '0') + ((cn / 10) % 10));
	 r[4] = (byte) (((byte) '0') + (cn % 10));

	 return r;
      }

   public static byte[] string_append(byte[]o1, byte[]o2)
      {
	 final int n1 = o1.length;
	 final int n2 = o2.length;
	 final byte[] r = new byte[n1 + n2];

	 System.arraycopy(o1, 0, r, 0, n1);
	 System.arraycopy(o2, 0, r, n1, n2);

	 return r;
      }

   public static byte[] string_append_3(byte[]o1, byte[]o2, byte[]o3)
      {
	 final int n1 = o1.length;
	 final int n2 = o2.length;
	 final int n3 = o3.length;
	 final byte[] r = new byte[n1 + n2 + n3];

	 System.arraycopy(o1, 0, r, 0, n1);
	 System.arraycopy(o2, 0, r, n1, n2);
	 System.arraycopy(o3, 0, r, n1 + n2, n3);

	 return r;
      }

   public static byte[] bgl_escape_C_string(byte[]src, int start, int end)
      {
	 int size = 0;

	 for (int i = start; i < end; ++i, ++size) {
	    if (src[i] == '\\') {
	       if ((i + 3 < end)
		   && isdigit(src[i + 1])
		   && isdigit(src[i + 2]) && isdigit(src[i + 3]))
		  i += 3;
	       else
		  i += 1;
	    }
	 }
	 int utf8shrink = 0;

	 final byte[] result = new byte[size];
	 int j = 0;

	 for (int i = start; i < end; ++i, ++j) {
	    if (src[i] != '\\') {
	       result[j] = src[i];
	    } else {
	       final byte cn = src[++i];

	       switch (cn) {
		  case (byte) '\0':
		     result[j] = (byte) '\\';
		  break;
		  case (byte) 'n':
		     result[j] = (byte) '\n';
		  break;
		  case (byte) 't':
		     result[j] = (byte) '\t';
		  break;
		  case (byte) 'b':
		     result[j] = (byte) '\b';
		  break;
		  case (byte) 'r':
		     result[j] = (byte) '\r';
		  break;
		  case (byte) 'f':
		     result[j] = (byte) '\f';
		  break;
		  case (byte) 'v':
		     result[j] = (byte) 11;
		  break;
		  default: {
		     if (i + 2 < end) {
			final byte s0 = src[i];
			final byte s1 = src[i + 1];
			final byte s2 = src[i + 2];

			if (isdigit(s0) && isdigit(s1) && isdigit(s2)) {
			   result[j] = (byte) (64 * ((int) (s0 - '0'))
					       + 8 * ((int) (s1 - '0'))
					       + ((int) (s2 - '0')));
			   i += 2;
			} else {
			   if (((s0 == (byte) 'x') || (s0 == (byte) 'X'))
			       && isxdigit(s1)
			       && isxdigit(s2) ) {
			      final byte n1 = xdigit_to_byte( s1 );
			      final byte n2 = xdigit_to_byte( s2 );

			      result[j] = (byte) (n1 * 16 + n2);
			      i += 2;
			   } else {
			      if (i + 4 < end) {
				 final byte s3 = src[i + 3];
				 final byte s4 = src[i + 4];
				 
				 if( ((s0 == (byte) 'u') || (s0 == (byte) 'U'))
				      && isxdigit(s1)
				      && isxdigit(s2) 
				      && isxdigit(s3) 
				      && isxdigit(s4) ) {
				    final byte n1 = xdigit_to_byte( s1 );
				    final byte n2 = xdigit_to_byte( s2 );
				    final byte n3 = xdigit_to_byte( s3 );
				    final byte n4 = xdigit_to_byte( s4 );
				    final char u =
				       (char)(n1*4096 + n2*512 + n3*16 + n4);
				    final char[] ucs2 = make_ucs2_string( 1, u );
				    final byte[] utf8 = ucs2_string_to_utf8_string( ucs2 );

				    System.arraycopy(utf8, 0, result, j, STRING_LENGTH( utf8 ));
				    
				    i += 4;
				    utf8shrink += (5 - STRING_LENGTH( utf8 ));
				    j += (STRING_LENGTH( utf8 ) - 1);
				 } else {
				    result[j] = cn;
				 }
			      } else {
				 result[j] = cn;
			      }
			   }
			}
		     } else {
			result[j] = cn;
		     }
		  }
		  break;
	       }
	    }
	 }

	 if( utf8shrink > 0 ) {
	    final byte[] res = new byte[ size - utf8shrink ];
	    System.arraycopy( result, 0, res, 0, size - utf8shrink );
	    return res;
	 } else {
	    return result;
	 }
      }

   static boolean isdigit(byte cn)
      {
	 return (((byte) '0' <= cn) && (cn <= (byte) '9'));
      }

   private static boolean isxdigit( byte b ) {
      if( isdigit( b ) ) {
	 return true;
      } else if( b <= (byte)'F' ) {
	 return b >= (byte)'A';
      } else return (b <= (byte)'f') && (b >= (byte)'a');
   }

   private static byte xdigit_to_byte( byte b ) {
      if( isdigit(b) )
	 return (byte)(b - (byte)'0');
      else if( (byte) 'a' <= b )
	 return (byte)(10 + (b - (byte) 'a'));
      else
	 return (byte)(10 + (b - (byte) 'A'));
   }
   
   public static byte[] bgl_escape_scheme_string(byte[]src, int start, int end)
      {
	 int w = 0;

	 for (int i = start; i < end; ++i)
	 {
	    ++w;
	    if (src[i] == (byte) '\\')
	       ++i;
	 }

	 final byte[] dst = new byte[w];

	 w = 0;
	 for (int i = start; i < end; ++i)
	 {
	    final byte cn = src[i];

	    if (cn != (byte) '\\')
	       dst[w++] = (byte) cn;
	    else
	    {
	       ++i;
	       dst[w++] = ((src[i] == 'n') ? (byte) '\n' : src[i]);
	    }
	 }

	 return dst;
      }

   public static byte[] string_for_read(byte[]src)
      {
	 return create_string_for_read(src, false);
      }

   public static byte[] symbol_for_read(byte[]src)
      {
	 return create_string_for_read(src, true);
      }

   public static byte[] create_string_for_read(byte[]src, boolean symbolp)
      {
	 final int len = src.length;
	 int w = 0;

	 for (int i = 0; i < len; ++i)
	 {
	    final int cn = src[i] & 0xff;

	    switch (cn)
	    {
	       case (byte) '\n':
	       case (byte) '\t':
	       case (byte) '\b':
	       case (byte) '\r':
	       case (byte) '\f':
	       case (byte) 11:
	       case (byte) '"':
	       case (byte) '\\':
		  w += 2;
	       break;
	       case (byte) '|':
		  w += (symbolp ? 2 : 1);
	       break;
	       default:
		  w += ((32 <= cn) ? 1 : 4);
		  break;
	    }
	 }

	 final byte[] res = new byte[w];

	 w = 0;
	 for (int i = 0; i < len; ++i)
	 {
	    final int cn = src[i] & 0xff;

	    switch (cn) {
	       case (byte) '\n':
		  res[w++] = (byte) '\\';
	       res[w++] = (byte) 'n';
	       break;
	       case (byte) '\t':
		  res[w++] = (byte) '\\';
	       res[w++] = (byte) 't';
	       break;
	       case (byte) '\b':
		  res[w++] = (byte) '\\';
	       res[w++] = (byte) 'b';
	       break;
	       case (byte) '\r':
		  res[w++] = (byte) '\\';
	       res[w++] = (byte) 'r';
	       break;
	       case (byte) '\f':
		  res[w++] = (byte) '\\';
	       res[w++] = (byte) 'f';
	       break;
	       case (byte) 11:
		  res[w++] = (byte) '\\';
		  res[w++] = (byte) 'v';
		  break;
	       case (byte) '"':
	       case (byte) '\\':
		  res[w++] = (byte) '\\';
		  res[w++] = (byte)cn;
	       break;
	       case (byte) '|':
		  if (symbolp)
		     res[w++] = (byte) '\\';
	       res[w++] = (byte) '|';
	       break;
	       default:
		  if( 32 <= cn ) {
		     res[w++] = (byte)cn;
		  } else {
		     res[w++] = (byte) '\\';
		     res[w++] = (byte) ('0' + ((cn >> 6) & 0x7));
		     res[w++] = (byte) ('0' + ((cn >> 3) & 0x7));
		     res[w++] = (byte) ('0' + (cn & 0x7));
		  }
		  break;
	    }
	 }

	 return res;
      }

   // Side effects
   public static Object blit_string(byte[]src, int i1, byte[]dst, int i2, int n)
      {
	 System.arraycopy(src, i1, dst, i2, n);
	 return unspecified.unspecified;
      }

   // Comparisons
   public static boolean bigloo_strcmp(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;

	 if (n1 != n2)
	    return false;

	 for (int i = 0; i < n1; ++i)
	    if (s1[i] != s2[i])
	       return false;

	 return true;
      }

   public static boolean bigloo_strcmp_at(byte[] s1, byte[] s2, int d)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;

	 if ((d < 0) || (n1 < n2 + d))
	    return false;

	 for (int i = 0; i < n2; ++i)
	    if (s1[i + d] != s2[i])
	       return false;

	 return true;
      }

   public static boolean bigloo_strcmp_ci_at(byte[] s1, byte[] s2, int d)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;

	 if (d < 0 || (n1 < n2 + d))
	    return false;

	 for (int i = 0; i < n2; ++i)
	    if (toupper(s1[i + d]) != toupper(s2[i]))
	       return false;

	 return true;
      }

   public static boolean bigloo_strncmp_at(byte[] s1, byte[] s2, int d, int l)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int n = (n2 < l ? n2 : l);

	 if (d < 0 || (n1 < n + d) || (n < 0))
	    return false;

	 for (int i = 0; i < n; ++i)
	    if (s1[i + d] != s2[i])
	       return false;

	 return true;
      }

   public static boolean bigloo_strncmp_ci_at(byte[] s1, byte[] s2, int d, int l)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int n = (n2 < l ? n2 : l);

	 if (d < 0 || (n1 < n + d) || (n < 0))
	    return false;

	 for (int i = 0; i < n; ++i)
	    if (toupper(s1[i + d]) != toupper(s2[i]))
	       return false;

	 return true;
      }

   public static boolean bigloo_strncmp(byte[]s1, byte[]s2, int l)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;

	 if ((n1 >= l) && (n2 >= l))
	 {
	    for (int i = 0; i < l; ++i)
	       if (s1[i] != s2[i])
		  return false;
	    return true;
	 }
	 else
	    return false;
      }

   public static boolean bigloo_strncmp_ci(byte[]s1, byte[]s2, int l)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;

	 if ((n1 >= l) && (n2 >= l))
	 {
	    for (int i = 0; i < l; ++i)
	       if (toupper(s1[i]) != toupper(s2[i]))
		  return false;
	    return true;
	 }
	 else
	    return false;
      }

   public static boolean strcicmp(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;

	 if (n1 != n2)
	    return false;
	 for (int i = 0; i < n1; ++i)
	    if (toupper(s1[i]) != toupper(s2[i]))
	       return false;
	 return true;
      }

   public static boolean string_le(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int min = (n1 < n2) ? n1 : n2;

	 for (int i = 0; i < min; ++i)
	 {
	    final int c1 = s1[i] & 0xFF;
	    final int c2 = s2[i] & 0xFF;

	    if (c1 != c2)
	       return (c1 <= c2);
	 }

	 return (n1 <= n2);
      }

   public static boolean string_lt(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int min = (n1 < n2) ? n1 : n2;

	 for (int i = 0; i < min; ++i)
	 {
	    final int c1 = s1[i] & 0xFF;
	    final int c2 = s2[i] & 0xFF;

	    if (c1 != c2)
	       return (c1 < c2);
	 }

	 return (n1 < n2);
      }

   public static boolean string_gt(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int min = (n1 < n2) ? n1 : n2;

	 for (int i = 0; i < min; ++i)
	 {
	    final int c1 = s1[i] & 0xFF;
	    final int c2 = s2[i] & 0xFF;

	    if (c1 != c2)
	       return (c1 > c2);
	 }

	 return (n1 > n2);
      }

   public static boolean string_ge(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int min = (n1 < n2) ? n1 : n2;

	 for (int i = 0; i < min; ++i)
	 {
	    final int c1 = s1[i] & 0xFF;
	    final int c2 = s2[i] & 0xFF;

	    if (c1 != c2)
	       return (c1 >= c2);
	 }

	 return (n1 >= n2);
      }

   public static boolean string_cilt(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int min = (n1 < n2) ? n1 : n2;

	 for (int i = 0; i < min; ++i)
	 {
	    final int c1 = toupper(s1[i] & 0xFF);
	    final int c2 = toupper(s2[i] & 0xFF);

	    if (c1 != c2)
	       return (c1 < c2);
	 }

	 return (n1 < n2);
      }

   public static boolean string_cile(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int min = (n1 < n2) ? n1 : n2;

	 for (int i = 0; i < min; ++i)
	 {
	    final int c1 = toupper(s1[i] & 0xFF);
	    final int c2 = toupper(s2[i] & 0xFF);

	    if (c1 != c2)
	       return (c1 <= c2);
	 }

	 return (n1 <= n2);
      }

   public static boolean string_cigt(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int min = (n1 < n2) ? n1 : n2;

	 for (int i = 0; i < min; ++i)
	 {
	    final int c1 = toupper(s1[i] & 0xFF);
	    final int c2 = toupper(s2[i] & 0xFF);

	    if (c1 != c2)
	       return (c1 > c2);
	 }

	 return (n1 > n2);
      }

   public static boolean string_cige(byte[]s1, byte[]s2)
      {
	 final int n1 = s1.length;
	 final int n2 = s2.length;
	 final int min = (n1 < n2) ? n1 : n2;

	 for (int i = 0; i < min; ++i)
	 {
	    final int c1 = toupper(s1[i] & 0xFF);
	    final int c2 = toupper(s2[i] & 0xFF);

	    if (c1 != c2)
	       return (c1 >= c2);
	 }

	 return (n1 >= n2);
      }

   //////
   // KEYWORD
   //////
   // Predicates
   public static boolean KEYWORDP(Object o)
      {
	 return (o instanceof keyword);
      }

   // Open functions
   public static byte[] KEYWORD_TO_STRING(keyword key)
      {
	 return key.string;
      }

   // Lib functions
   public static keyword string_to_keyword(byte[]s)
      {
	 return keyword.make_keyword(s);
      }

   //////
   // SYMBOL
   //////
   // Predicates
   public static boolean SYMBOLP(Object o)
      {
	 return (o instanceof symbol);
      }

   // Lib functions
   public static boolean symbol_exists_p(byte[] name)
      {
	 return symbol.exists(name);
      }

   public static symbol string_to_symbol(byte[] name)
      {
	 return symbol.make_symbol(name);
      }

   public static symbol string_to_symbol(String name)
      {
	 return symbol.make_symbol(name.getBytes());
      }

   public static byte[] SYMBOL_TO_STRING(symbol o)
      {
	 // CARE why not a correct signature in Scheme
	 return o.string;
      }

   public static Object GET_SYMBOL_PLIST(symbol o)
      {
	 return o.cval;
      }

   public static Object SET_SYMBOL_PLIST(symbol o, Object v)
      {
	 o.cval = v;
	 return unspecified.unspecified;
      }

   public static Object GET_KEYWORD_PLIST(keyword o)
      {
	 return o.cval;
      }

   public static Object SET_KEYWORD_PLIST(keyword o, Object v)
      {
	 o.cval = v;
	 return unspecified.unspecified;
      }

   //////
   // CELL
   //////
   // Predicates
   public static boolean CELLP(Object o)
      {
	 return (o instanceof cell);
      }

   // Open functions
   public static cell MAKE_CELL(Object o)
      {
	 return new cell(o);
      }

   public static Object CELL_SET(cell o, Object v)
      {
	 o.car = v;
	 return unspecified.unspecified;
      }

   public static Object CELL_REF(cell o)
      {
	 return o.car;
      }

   public static Object _EVMEANING_ADDRESS(Object o)
      {
	 // CARE Where is declared this function !!
	 return new cell(o);
      }

   public static Object _EVMEANING_ADDRESS_REF(Object o)
      {
	 return ((cell) o).car;
      }

   public static Object _EVMEANING_ADDRESS_SET(Object o, Object v)
      {
	 ((cell) o).car = v;
	 return unspecified.unspecified;
      }

   //////
   // FOREIGN
   //////
   public static boolean FOREIGNP(Object o)
      {
	 return (!(o instanceof obj) && !(o instanceof object));
      }

   public static boolean FOREIGN_NULLP(Object f)
      {
	 return (f == null);
      }

   public static Object MAKE_VOID_STAR_NULL() {
      return null;
   }
 
   public static foreign void_star_to_obj( Object o ) {
      return new bigloo.foreign();
   }

   public static boolean OBJECT_PTR_NULL(Object o)
      {
	 return (o == null);
      }

   public static byte[] MAKE_STRING_PTR_NULL() {
      return null;
   }
	 
   public static boolean STRING_PTR_NULL(byte[] o)
      {
	 return (o == null);
      }

   public static boolean FOREIGN_EQP(Object f1, Object f2)
      {
	 return (f1 == f2);
      }

   public static symbol FOREIGN_ID(Object f)
      {
	 // CARE where defined?
	 if( f == null ) {
	    return string_to_symbol("null".getBytes());
	 } else {
	    return string_to_symbol(f.getClass().getName().getBytes());
	 }
      }

   public static cobj FOREIGN_TO_COBJ(Object f)
      {
	 return (cobj) f;
      }

   public static cobj obj_to_cobj(Object o)
      {
	 return (cobj) o;
      }

   public static Object COBJ_TO_OBJ(cobj o)
      {
	 return o;
      }

   //////
   // CUSTOM
   //////
   public static boolean CUSTOMP(Object o)
      {
	 return (o instanceof custom);
      }

   public static custom bgl_custom_nil()
      {
	 return new custom();
      }
      
   public static boolean CUSTOM_CMP(custom c1, custom c2)
      {
	 return c1.equal(c2);
      }

   public static int CUSTOM_HASH_NUMBER(custom c)
      {
	 return c.hash();
      }

   public static byte[] CUSTOM_IDENTIFIER(custom c)
      {
	 return c.identifier;
      }

   public static Object CUSTOM_IDENTIFIER_SET(custom c, byte[] s)
      {
	 c.identifier = s;
	 return unspecified.unspecified;
      }

   //////
   // DATE
   //////
   public static boolean DATEP(Object o)
      {
	 return (o instanceof bigloo.date);
      }

   public static date bgl_make_date(long ns, int s,
				    int min, int h, int d, int mon,
				    int y, int tz, boolean istz, int dst)
      {
	 return new bigloo.date(ns, s, min, h, d, mon - 1, y, tz, istz, dst);
      }

   public static date bgl_update_date(date, long ns, int s,
				      int min, int h, int d, int mon,
				      int y, int tz, boolean istz, int dst)
      {
	 date tmp = bgl_make_date( ns, s, min, h, d, mon,
				   y, tz, istz, dst);

	 date.nsec = tmp.nsec;
	 date.calendar = tmp.calendar;
	 date.timezone = tmp.timezone;
	 return date;
      }
   

   public static date bgl_seconds_to_date(long sec)
      {
	 return new bigloo.date(sec);
      }

   public static date bgl_seconds_to_gmtdate(long sec)
      {
	 System.out.println( "bgl_seconds_to_gmtdate not implemented using local time" );
	 return bgl_seconds_to_date( sec );
      }

   public static date bgl_nanoseconds_to_date(long nsec)
      {
	 return new bigloo.date(nsec, true);
      }

   public static date bgl_milliseconds_to_date(long nsec)
      {
	 return new bigloo.date(nsec, false);
      }

   public static date bgl_seconds_to_utc_date(long sec)
      {
	 date d = new bigloo.date(sec);
	 d.calendar.setTimeZone(new SimpleTimeZone(0, "UTC"));
	 return d;
      }

   public static date bgl_milliseconds_to_date(long sec)
      {
	 date d = new bigloo.date(sec * 1000000, true);
	 d.calendar.setTimeZone(new SimpleTimeZone(0, "UTC"));
	 return d;
      }

   public static long bgl_current_seconds()
      {
	 return (new Date().getTime() / 1000);
      }

   public static long bgl_current_microseconds()
      {
	 return (new Date().getTime() * 1000);
      }

   public static long bgl_current_milliseconds()
      {
	 return (new Date().getTime());
      }

   public static long bgl_current_nanoseconds()
      {
	 return (new Date().getTime() * 1000000);
      }

   public static long bgl_date_to_seconds(date d)
      {
	 return (d.calendar.getTime().getTime() / 1000);
      }

   public static long bgl_date_to_nanoseconds(date d)
      {
	 return (d.calendar.getTime().getTime() * 1000000);
      }

   public static long bgl_date_to_milliseconds(date d)
      {
	 return (d.calendar.getTime().getTime() * 1000);
      }

   public static byte[] bgl_seconds_to_string(long sec)
      {
	 return bgl_seconds_to_date(sec).calendar.getTime().toString().getBytes();
      }

   public static long bgl_integer_to_seconds(int i)
      {
	 return i;
      }

   public static byte[] bgl_seconds_to_utc_string(long sec)
      {
	 return bgl_seconds_to_utc_date(sec).calendar.getTime().toString().getBytes();
      }

   public static int BGL_DATE_SECOND(date d)
      {
	 return d.calendar.get(Calendar.SECOND);
      }

   public static long BGL_DATE_NANOSECOND(date d)
      {
	 return d.nsec;
      }

   public static long BGL_DATEMILLISECOND(date d)
      {
	 return d.nsec / 1000000;
      }

   public static int BGL_DATE_MINUTE(date d)
      {
	 return d.calendar.get(Calendar.MINUTE);
      }

   public static int BGL_DATE_HOUR(date d)
      {
	 return d.calendar.get(Calendar.HOUR_OF_DAY);
      }

   public static int BGL_DATE_DAY(date d)
      {
	 return d.calendar.get(Calendar.DAY_OF_MONTH);
      }

   public static int BGL_DATE_WDAY(date d)
      {
	 return d.calendar.get(Calendar.DAY_OF_WEEK);
      }

   public static int BGL_DATE_YDAY(date d)
      {
	 return d.calendar.get(Calendar.DAY_OF_YEAR);
      }

   public static int BGL_DATE_MONTH(date d)
      {
	 return (d.calendar.get(Calendar.MONTH) + 1);
      }

   public static int BGL_DATE_YEAR(date d)
      {
	 return d.calendar.get(Calendar.YEAR);
      }

   public static int BGL_DATE_TIMEZONE(date d)
      {
	 return d.timezone;
      }

   public static int BGL_DATE_ISDST(date d)
      {
	 return ( d.calendar.get(Calendar.DST_OFFSET) > 0 ) ? 1 : -1;
      }

   public static int BGL_DATE_ISGMT(date d)
      {
	 return (d.calendar.get(Calendar.ZONE_OFFSET) == 0);
      }

   private static final byte[][] day_names = { "Sunday".getBytes(),
					       "Monday".getBytes(),
					       "Tuesday".getBytes(),
					       "Wednesday".getBytes(),
					       "Thursday".getBytes(),
					       "Friday".getBytes(),
					       "Saturday".getBytes()
   };

   private static final byte[][] day_anames = { "Sun".getBytes(),
						"Mon".getBytes(),
						"Tue".getBytes(),
						"Wed".getBytes(),
						"Thu".getBytes(),
						"Fri".getBytes(),
						"Sat".getBytes()
   };

   private static final byte[][] month_names = { "January".getBytes(),
						 "February".getBytes(),
						 "March".getBytes(),
						 "April".getBytes(),
						 "May".getBytes(),
						 "June".getBytes(),
						 "July".getBytes(),
						 "August".getBytes(),
						 "September".getBytes(),
						 "October".getBytes(),
						 "November".getBytes(),
						 "December".getBytes()
   };

   private static final byte[][] month_anames = { "Jan".getBytes(),
						  "Feb".getBytes(),
						  "Mar".getBytes(),
						  "Apr".getBytes(),
						  "May".getBytes(),
						  "Jun".getBytes(),
						  "Jul".getBytes(),
						  "Aug".getBytes(),
						  "Sep".getBytes(),
						  "Oct".getBytes(),
						  "Nov".getBytes(),
						  "Dec".getBytes()
   };

   public static byte[] bgl_day_name(int n)
      {
	 return day_names[n - 1];
      }

   public static byte[] bgl_day_aname(int n)
      {
	 return day_anames[n - 1];
      }

   public static byte[] bgl_month_name(int n)
      {
	 return month_names[n - 1];
      }

   public static byte[] bgl_month_aname(int n)
      {
	 return month_anames[n - 1];
      }

   //////
   // PAIR
   //////
   // Predicates
   public static boolean PAIRP(Object o)
      {
	 return (o instanceof pair);
      }

   // Open functions
   public static pair MAKE_PAIR(Object car, Object cdr)
      {
	 // CARE where defined?
	 return new pair(car, cdr);
      }
   public static pair MAKE_STACK_PAIR(Object car, Object cdr)
      {
	 // CARE where defined?
	 return new pair(car, cdr);
      }

   public static Object CAR(pair c)
      {
	 return c.car;
      }

   public static Object CDR(pair c)
      {
	 return c.cdr;
      }

   public static Object SET_CAR(pair c, Object o)
      {
	 c.car = o;
	 return unspecified.unspecified;
      }

   public static Object SET_CDR(pair c, Object o)
      {
	 c.cdr = o;
	 return unspecified.unspecified;
      }

   //////
   // EXTENDED_PAIR
   //////
   public static boolean EPAIRP(Object o) {
      return (o instanceof extended_pair);
   }

   public static extended_pair MAKE_EPAIR(Object car, Object cdr, Object cer) {
      return new extended_pair(car, cdr, cer);
   }

   public static Object CER(extended_pair c) {
      return c.cer;
   }

   public static Object SET_CER(extended_pair c, Object o) {
      c.cer = o;
      return unspecified.unspecified;
   }

   //////
   // VECTOR
   //////
   // Predicates
   public static boolean VECTORP(Object o) {
      return (o instanceof Object[]);
   }

   public static void FREE_VECTOR_UNCOLLECTABLE(Object[]v) {
      ;
   }

   // Open functions
   public static int VECTOR_LENGTH(Object[]v) {
      return v.length;
   }

   public static Object VECTOR_REF(Object[]v, int i) {
      return v[i];
   }

   public static Object VECTOR_SET(Object[]v, int i, Object o) {
      v[i] = o;
      return unspecified.unspecified;
   }

   public static boolean BOUND_CHECK(int n1, int n2) {
      return (n1 < n2);
   }

   public static boolean BOUND_CHECK(long n1, long n2) {
      return (n1 < n2);
   }

   public static Object VECTOR_TAG_SET(Object[]v, int n) {
      return unspecified.unspecified;
   }

   public static int VECTOR_TAG(Object[]v) {
      return 0;
   }

   // Lib functions
   public static Object[] make_vector(int n, Object init) {
      final Object[] r = new Object[ n ];

      for (int i = 0; i < n; ++i)
	 r[i] = init;

      return r;
   }

   public static Object[] make_vector0() {
      return(new Object[]{});
   }

   public static Object[] make_vector1(Object a1) {
      return(new Object[]{a1});
   }

   public static Object[] make_vector2(Object a1, Object a2) {
      return(new Object[]{a1, a2});
   }

   public static Object[] make_vector3(Object a1, Object a2, Object a3) {
      return(new Object[]{a1, a2, a3});
   }

   public static Object[] make_vector4(Object a1, Object a2, Object a3, Object a4) {
      return(new Object[]{a1, a2, a3, a4});
   }

   public static Object[] make_vector5(Object a1, Object a2, Object a3, Object a4, Object a5) {
      return(new Object[]{a1, a2, a3, a4, a5});
   }

   public static Object[] list_to_vector(Object l) {
      int n = list_length(l);
      Object [] r = new Object[n];
      for(int i=0 ; l != nil.nil ; l = ((pair) l).cdr, i++)
	 r[i] = ((pair) l).car;
      return(r);
   }

   static int list_length(Object l) {
      int i = 0;
      for( ; l != nil.nil ;  l = ((pair) l).cdr, i++);
      return(i);
   }

   public static Object[] create_vector(int n) {
      return new Object[n];
   }

   public static Object fill_vector(Object[]v, int start, int len, Object o) {
      for (int i = start; i < len; ++i)
	 v[i] = o;
      return unspecified.unspecified;
   }

   public static Object[] sort_vector(Object[]v, procedure p) {
      final int n = v.length;

      for (int incr = n / 2; incr != 0; incr /= 2)
	 for (int i = incr; i < n; ++i)
	    for (int j = i - incr; j >= 0; j -= incr)
	       if (p.funcall2(v[j], v[j + incr]) != bbool.faux)
		  break;
	       else
	       {
		  final Object tmp = v[j + incr];

		  v[j + incr] = v[j];
		  v[j] = tmp;
	       }

      return v;
   }

   public static Object[] BGL_VECTOR_SHRINK( Object[] v, int nlen ) {
      if( nlen >= 0 && nlen < VECTOR_LENGTH( v ) ) {
	 final Object[] r = new Object[ nlen ];

	 for( int i = 0; i < nlen; ++i ){
	    r[ i ] = v[ i ];
	 }
	 
	 return r;
      } else {
	 return v;
      }
   }

   //////
   // HVECTOR
   //////
   // Predicates
   public static boolean BGL_HVECTORP(Object o) {
      return (o instanceof hvector);
   }
   public static boolean BGL_S8VECTORP(Object o) {
      return (o instanceof s8vector);
   }
   public static boolean BGL_U8VECTORP(Object o) {
      return (o instanceof u8vector);
   }
   public static boolean BGL_S16VECTORP(Object o) {
      return (o instanceof s16vector);
   }
   public static boolean BGL_U16VECTORP(Object o) {
      return (o instanceof u16vector);
   }
   public static boolean BGL_S32VECTORP(Object o) {
      return (o instanceof s32vector);
   }
   public static boolean BGL_U32VECTORP(Object o) {
      return (o instanceof u32vector);
   }
   public static boolean BGL_S64VECTORP(Object o) {
      return (o instanceof s64vector);
   }
   public static boolean BGL_U64VECTORP(Object o) {
      return (o instanceof u64vector);
   }
   public static boolean BGL_F32VECTORP(Object o) {
      return (o instanceof f32vector);
   }
   public static boolean BGL_F64VECTORP(Object o) {
      return (o instanceof f64vector);
   }

   public static int BGL_HVECTOR_LENGTH(Object o) {
      return ((hvector)o).len;
   }
   public static int BGL_HVECTOR_IDENT(Object o) {
      return ((hvector)o).ident();
   }

   public static s8vector BGL_ALLOC_S8VECTOR(int l) {
      return new s8vector(l);
   }
   public static u8vector BGL_ALLOC_U8VECTOR(int l) {
      return new u8vector(l);
   }
   public static s16vector BGL_ALLOC_S16VECTOR(int l) {
      return new s16vector(l);
   }
   public static u16vector BGL_ALLOC_U16VECTOR(int l) {
      return new u16vector(l);
   }
   public static s32vector BGL_ALLOC_S32VECTOR(int l) {
      return new s32vector(l);
   }
   public static u32vector BGL_ALLOC_U32VECTOR(int l) {
      return new u32vector(l);
   }
   public static s64vector BGL_ALLOC_S64VECTOR(int l) {
      return new s64vector(l);
   }
   public static u64vector BGL_ALLOC_U64VECTOR(int l) {
      return new u64vector(l);
   }
   public static f32vector BGL_ALLOC_F32VECTOR(int l) {
      return new f32vector(l);
   }
   public static f64vector BGL_ALLOC_F64VECTOR(int l) {
      return new f64vector(l);
   }

   public static byte BGL_S8VREF(s8vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_S8VSET(s8vector v, int l, byte o) {
      v.objs[l] = o;
   }
   public static byte BGL_U8VREF(u8vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_U8VSET(u8vector v, int l, byte o) {
      v.objs[l] = o;
   }
   
   public static short BGL_S16VREF(s16vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_S16VSET(s16vector v, int l, short o) {
      v.objs[l] = o;
   }
   public static short BGL_U16VREF(u16vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_U16VSET(u16vector v, int l, short o) {
      v.objs[l] = o;
   }
   
   public static int BGL_S32VREF(s32vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_S32VSET(s32vector v, int l, int o) {
      v.objs[l] = o;
   }
   public static int BGL_U32VREF(u32vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_U32VSET(u32vector v, int l, int o) {
      v.objs[l] = o;
   }
   
   public static long BGL_S64VREF(s64vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_S64VSET(s64vector v, int l, long o) {
      v.objs[l] = o;
   }
   public static long BGL_U64VREF(u64vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_U64VSET(u64vector v, int l, long o) {
      v.objs[l] = o;
   }
   
   public static float BGL_F32VREF(f32vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_F32VSET(f32vector v, int l, float o) {
      v.objs[l] = o;
   }
   public static double BGL_F64VREF(f64vector v, int l) {
      return v.objs[l];
   }
   public static void BGL_F64VSET(f64vector v, int l, double o) {
      v.objs[l] = o;
   }
   
   public static int BGL_S16_U8VREF(u8vector v, int l) {
      return v.objs[l]<<8 + v.objs[l+1];
   }
   public static void BGL_S16_U8VSET(u8vector v, int l, int o) {
      v.objs[l] = (byte)(o>>8 & 0xff);
      v.objs[l+1] = (byte)(o & 0xff);
   }

   public static int BGL_S32_U8VREF(u8vector v, int l) {
      return v.objs[l]<<24 + v.objs[l+1]<<16 + v.objs[l+2]<<8 + v.objs[l+3];
   }
   public static void BGL_S32_U8VSET(u8vector v, int l, int o) {
      v.objs[l] = (byte)(o>>24);
      v.objs[l+1] = (byte)(o>>16 & 0xff);
      v.objs[l+2] = (byte)(o>>8 & 0xff);
      v.objs[l+3] = (byte)(o & 0xff);
   }

   public static long BGL_S64_U8VREF(u8vector v, int l) {
      return
	 v.objs[l]<<56 + v.objs[l+1]<<48 + v.objs[l+2]<<40 + v.objs[l+3]<<32 +
	 v.objs[l+4]<<24 + v.objs[l+5]<<16 + v.objs[l+6]<<8 + v.objs[l+7];
   }
   public static void BGL_S64_U8VSET(u8vector v, int l, long o) {
      v.objs[l] = (byte)(o>>56 & 0xff);
      v.objs[l+1] = (byte)(o>>48 & 0xff);
      v.objs[l+2] = (byte)(o>>40 & 0xff);
      v.objs[l+3] = (byte)(o>>32 & 0xff);
      v.objs[l+4] = (byte)(o>>24 & 0xff);
      v.objs[l+5] = (byte)(o>>16 & 0xff);
      v.objs[l+6] = (byte)(o>>8 & 0xff);
      v.objs[l+7] = (byte)(o & 0xff);
   }

   public static float BGL_F32_U8VREF(u8vector v, int l) {
      return Float.intBitsToFloat( BGL_S32_U8VREF( v, l ) );
   }
   
   public static void BGL_F32_U8VSET(u8vector v, int l, float f) {
      int bits = Float.floatToIntBits(f);
      v.objs[l] = (byte)(bits & 0xff);
      v.objs[l+1] = (byte)((bits >> 8) & 0xff);
      v.objs[l+2] = (byte)((bits >> 16) & 0xff);
      v.objs[l+3] = (byte)((bits >> 24) & 0xff);
   }
   
   public static double BGL_F64_U8VREF(u8vector v, int l) {
      return Double.longBitsToDouble( BGL_S64_U8VREF( v, l ) );
   }
   
   public static void BGL_F64_U8VSET(u8vector v, int l, double f) {
      long bits = Double.doubleToLongBits(f);
      v.objs[l] = (byte)(bits & 0xff);
      v.objs[l+1] = (byte)((bits >> 8) & 0xff);
      v.objs[l+2] = (byte)((bits >> 16) & 0xff);
      v.objs[l+3] = (byte)((bits >> 24) & 0xff);
      v.objs[l+4] = (byte)((bits >> 32) & 0xff);
      v.objs[l+5] = (byte)((bits >> 40) & 0xff);
      v.objs[l+6] = (byte)((bits >> 48) & 0xff);
      v.objs[l+7] = (byte)((bits >> 54) & 0xff);
   }
   
   public static void BGL_SU8VECTOR_COPY(u8vector t, int ts, u8vector s, int ss, int se ) {
      int len = se - ss;
      boolean forward = true;
      if (t == s) {
          forward = !((ss < ts) && (ts < (ss + len)));
      }
      if (forward) {
          for( int i = 0; i < len; i++ ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      } else {
          for( int i = len-1; i >= 0; i-- ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      }
   }
   
   public static void BGL_SU16VECTOR_COPY(u16vector t, int ts, u16vector s, int ss, int se ) {
      int len = se - ss;
       boolean forward = true;
      if (t == s) {
          forward = !((ss < ts) && (ts < (ss + len)));
      }
      if (forward) {
          for( int i = 0; i < len; i++ ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      } else {
          for( int i = len-1; i >= 0; i-- ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      }
   }
   
   public static void BGL_SU32VECTOR_COPY(u32vector t, int ts, u32vector s, int ss, int se ) {
      int len = se - ss;
       boolean forward = true;
      if (t == s) {
          forward = !((ss < ts) && (ts < (ss + len)));
      }
      if (forward) {
          for( int i = 0; i < len; i++ ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      } else {
          for( int i = len-1; i >= 0; i-- ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      }
   }
   
   public static void BGL_SU64VECTOR_COPY(u64vector t, int ts, u64vector s, int ss, int se ) {
      int len = se - ss;
       boolean forward = true;
      if (t == s) {
          forward = !((ss < ts) && (ts < (ss + len)));
      }
      if (forward) {
          for( int i = 0; i < len; i++ ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      } else {
          for( int i = len-1; i >= 0; i-- ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      }
   }
   
   public static void BGL_F32VECTOR_COPY(u32vector t, int ts, u32vector s, int ss, int se ) {
      int len = se - ss;
       boolean forward = true;
      if (t == s) {
          forward = !((ss < ts) && (ts < (ss + len)));
      }
      if (forward) {
          for( int i = 0; i < len; i++ ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      } else {
          for( int i = len-1; i >= 0; i-- ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      }
   }
   
   public static void BGL_F64VECTOR_COPY(u64vector t, int ts, u64vector s, int ss, int se ) {
      int len = se - ss;
       boolean forward = true;
      if (t == s) {
          forward = !((ss < ts) && (ts < (ss + len)));
      }
      if (forward) {
          for( int i = 0; i < len; i++ ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      } else {
          for( int i = len-1; i >= 0; i-- ) {
              t.objs[ ts + i ] = s.objs[ ss + i ];
          }
      }
   }
   
   //////
   // TVECTOR
   //////
   public static boolean TVECTORP(Object o)
      {
	 return ((o instanceof double[])
		 ||(o instanceof int[])
		 ||(o instanceof long[])
		 ||(o instanceof boolean[])
		 ||(o instanceof Object[]));
      }

   private static final Object[] desc_table = new Object[4];

   public static Object TVECTOR_DESCR(Object o)
      {
	 if (o instanceof double[])
	    return desc_table[0];
	 if (o instanceof int[])
	    return desc_table[1];
	 if (o instanceof long[])
	    return desc_table[2];
	 if (o instanceof boolean[])
	    return desc_table[2];
	 if (o instanceof Object[])
	    return desc_table[3];
	 return fail("tvector_desc", "Unknown tvec object", o);
      }

   public static Object TVECTOR_DESCR_SET(Object o, Object desc)
      {
	 if (o instanceof double[])
	    desc_table[0] = desc;
	 else if (o instanceof int[])
	    desc_table[1] = desc;
	 else if (o instanceof long[])
	    desc_table[2] = desc;
	 else if (o instanceof boolean[])
	    desc_table[2] = desc;
	 else if (o instanceof Object[])
	    desc_table[3] = desc;

	 return unspecified.unspecified;
      }

   public static int TVECTOR_LENGTH(Object o)
      {
	 if (o instanceof double[])
	    return ((double[]) o).length;
	 if (o instanceof int[])
	    return ((int[]) o).length;
	 if (o instanceof long[])
	    return ((long[]) o).length;
	 if (o instanceof boolean[])
	    return ((boolean[])o).length;
	 if (o instanceof Object[])
	    return ((Object[])o).length;
	 if (o instanceof Object[])
	    return ((Object[])o).length;

	 fail("tvector_length", "Unknown tvec object", o);
	 return 0;
      }

   //////
   // WEAKPTR
   //////
  
   public static boolean BGL_WEAKPTRP(Object o){
      return (o instanceof weakptr);
   }

   public static Object weakptr_data(weakptr p){
      return p.getData();
   }

   public static void weakptr_data_set(weakptr p, Object o){
      p.setData(o);
   }

   public static weakptr make_weakptr(Object o){
      return new weakptr(o);
   }

   //////
   // STRUCT
   //////
   public static boolean STRUCTP(Object o)
      {
	 return (o instanceof struct);
      }

   public static Object STRUCT_KEY(struct o)
      {
	 return o.key;
      }

   public static Object STRUCT_KEY_SET(struct o, Object v)
      {
	 o.key = v;
	 return unspecified.unspecified;
      }

   public static int STRUCT_LENGTH(struct o)
      {
	 return o.values.length;
      }

   public static Object STRUCT_REF(struct o, int i)
      {
	 return o.values[i];
      }

   public static Object STRUCT_SET(struct o, int i, Object v)
      {
	 o.values[i] = v;
	 return unspecified.unspecified;
      }

   public static Object UNSAFE_STRUCT_REF(struct o, int i)
      {
	 return o.values[i];
      }

   public static Object UNSAFE_STRUCT_SET(struct o, int i, Object v)
      {
	 o.values[i] = v;
	 return unspecified.unspecified;
      }

   public static struct create_struct (symbol key, int size)
      {
	 return new struct (key, size);
      }

   public static struct make_struct (symbol key, int size, Object o)
      {
	 return new struct (key, size, o);
      }

   /////
   // OBJECT
   /////

   public static int OBJECT_TYPE = 0;

   public static int MIN_DISPLAY_SIZE = 6;
   
   public static Object BGL_OBJECT_WIDENING_SET(object o, Object v)
      {
	 o.widening = v;
	 return unspecified.unspecified;
      }

   public static Object BGL_OBJECT_WIDENING(object o)
      {
	 return o.widening;
      }

   public static int BGL_OBJECT_CLASS_NUM(object o)
      {
	 return o.header;
      }

   public static Object BGL_OBJECT_CLASS_NUM_SET(object o, int n)
      {
	 o.header = n;
	 return unspecified.unspecified;
      }

   public static procedure bgl_make_generic( procedure p ) {
      return new generic( p.index, p.arity, 3, p );
   }
   
   public static Object BGL_HEAP_DEBUG_MARK_OBJ(Object o)
      {
	 return o;
      }

   ////
   // CLASS
   ////
   public static boolean BGL_CLASSP(Object o)
      {
	 return (o instanceof bclass);
      }

   public static Object bgl_make_class( symbol name, symbol module, int num,
					Object bsuper, Object sub,
					procedure alloc, int hash,
					Object[] fd, Object[] allfd,
					Object constr, Object[] virt,
					Object nw, procedure nil,
					Object shrink,
					int depth,
					Object evdata ) {
      return new bclass( name, module, num,
			 bsuper, sub,
			 alloc, hash,
			 fd, allfd,
			 constr, virt, nw,
			 nil, shrink,
			 depth,
			 evdata );
   }

   public static symbol BGL_CLASS_NAME( bclass c ) {
      return c.name;
   }
   public static symbol BGL_CLASS_NAME( Object c ) {
      return BGL_CLASS_NAME( (bclass)c );
   }

   public static int BGL_CLASS_INDEX( bclass c ) {
      return c.index;
   }
   public static int BGL_CLASS_INDEX( Object c ) {
      return BGL_CLASS_INDEX( (bclass)c );
   }

   public static int BGL_CLASS_DEPTH( bclass c ) {
      return c.depth;
   }
   public static int BGL_CLASS_DEPTH( Object c ) {
      return BGL_CLASS_DEPTH( (bclass)c);
   }

   public static Object BGL_CLASS_SUPER( bclass c ) {
      return c.bsuper;
   }
   public static Object BGL_CLASS_SUPER( Object c ) {
      return BGL_CLASS_SUPER( (bclass)c );
   }

   public static Object BGL_CLASS_ANCESTORS_REF( bclass c, int i ) {
      return c.ancestors[ i ];
   }
   public static Object BGL_CLASS_ANCESTORS_REF( Object c, int i ) {
      return BGL_CLASS_ANCESTORS_REF( (bclass)c, i );
   }

   public static Object BGL_CLASS_SUBCLASSES( bclass c ) {
      return c.subclasses;
   }
   public static Object BGL_CLASS_SUBCLASSES( Object c ) {
      return BGL_CLASS_SUBCLASSES( (bclass)c );
   }

   public static Object BGL_CLASS_SUBCLASSES_SET( bclass c, Object sc ) {
      c.subclasses = sc;
      return c;
   }
   public static Object BGL_CLASS_SUBCLASSES_SET( Object c, Object sc ) {
      return BGL_CLASS_SUBCLASSES_SET( (bclass)c, sc );
   }

   public static Object BGL_CLASS_NIL( bclass c ) {
      return c.nil;
   }
   public static Object BGL_CLASS_NIL( Object c ) {
      return BGL_CLASS_NIL( (bclass)c );
   }

   public static Object BGL_CLASS_NIL_SET( bclass c, Object nil ) {
      c.nil = nil;
      return c;
   }
   public static Object BGL_CLASS_NIL_SET( Object c, Object nil ) {
      return BGL_CLASS_NIL_SET( (bclass)c, nil );
   }

   public static symbol BGL_CLASS_MODULE( bclass c ) {
      return c.module;
   }
   public static symbol BGL_CLASS_MODULE( Object c ) {
      return BGL_CLASS_MODULE( (bclass)c );
   }

   public static procedure BGL_CLASS_ALLOC_FUN( bclass c ) {
      return c.alloc_fun;
   }
   public static procedure BGL_CLASS_ALLOC_FUN( Object c ) {
      return BGL_CLASS_ALLOC_FUN( (bclass) c );
   }

   public static int BGL_CLASS_HASH( bclass c ) {
      return c.hash;
   }
   public static int BGL_CLASS_HASH( Object c ) {
      return BGL_CLASS_HASH( (bclass)c );
   }

   public static procedure BGL_CLASS_NEW_FUN( bclass c ) {
      return (procedure)c.new_fun;
   }
   public static procedure BGL_CLASS_NEW_FUN( Object c ) {
      return BGL_CLASS_NEW_FUN( (bclass)c );
   }

   public static procedure BGL_CLASS_NIL_FUN( bclass c ) {
      return c.nil_fun;
   }
   public static procedure BGL_CLASS_NIL_FUN( Object c ) {
      return BGL_CLASS_NIL_FUN( (bclass)c );
   }

   public static Object BGL_CLASS_CONSTRUCTOR( bclass c ) {
      return c.constructor;
   }
   public static Object BGL_CLASS_CONSTRUCTOR( Object c ) {
      return BGL_CLASS_CONSTRUCTOR( (bclass)c );
   }

   public static Object BGL_CLASS_SHRINK( bclass c ) {
      return c.shrink;
   }
   public static Object BGL_CLASS_SHRINK( Object c ) {
      return BGL_CLASS_SHRINK( (bclass)c );
   }

   public static Object[] BGL_CLASS_VIRTUAL_FIELDS( bclass c ) {
      return c.virtual_fields;
   }
   public static Object[] BGL_CLASS_VIRTUAL_FIELDS( Object c ) {
      return BGL_CLASS_VIRTUAL_FIELDS( (bclass)c );
   }

   public static Object[] BGL_CLASS_DIRECT_FIELDS( bclass c ) {
      return c.direct_fields;
   }
   public static Object[] BGL_CLASS_DIRECT_FIELDS( Object c ) {
      return BGL_CLASS_DIRECT_FIELDS( (bclass)c );
   }

   public static Object BGL_CLASS_DIRECT_FIELDS_SET( bclass c, Object[] df ) {
      c.direct_fields = df;
      return c;
   }
   public static Object BGL_CLASS_DIRECT_FIELDS_SET( Object c, Object[] df ) {
      return BGL_CLASS_DIRECT_FIELDS_SET( (bclass)c, df );
   }

   public static Object[] BGL_CLASS_ALL_FIELDS( bclass c ) {
      return c.all_fields;
   }
   public static Object[] BGL_CLASS_ALL_FIELDS( Object c ) {
      return BGL_CLASS_ALL_FIELDS( (bclass)c );
   }

   public static Object BGL_CLASS_ALL_FIELDS_SET( bclass c, Object[] df ) {
      c.all_fields = df;
      return c;
   }
   public static Object BGL_CLASS_ALL_FIELDS_SET( Object c, Object[] df ) {
      return BGL_CLASS_ALL_FIELDS_SET( (bclass)c, df );
   }

   public static Object BGL_CLASS_EVDATA( bclass c ) {
      return c.evdata;
   }
   public static Object BGL_CLASS_EVDATA( Object c ) {
      return BGL_CLASS_EVDATA( (bclass)c );
   }

   public static Object BGL_CLASS_EVDATA_SET( bclass c, Object d ) {
      c.evdata = d;
      return d;
   }
   public static Object BGL_CLASS_EVDATA_SET( Object c, Object d ) {
      return BGL_CLASS_EVDATA_SET( (bclass)c, d );
   }

   //////
   // PROCEDURE
   //////
   public static boolean PROCEDUREP(Object o)
      {
	 return (o instanceof procedure);
      }

   public static procedure buildproc(procedure p, int i, int a, int n)
      {
	 p.index = i;
	 p.arity = a;
	 p.env = new Object[n];
	 return p;
      }

   public static procedure bgl_make_procedure( Object e, int a, int l )
      {
	 fail( "make-procedure", "not impleemented", e );
	 return new procedure();
      }
   
   public static byte[] bgl_procedure_entry_to_string(procedure p)
      {
	 fail( "procedure-entry->string", "not impleemented", p );
	 return "".getBytes();
      }
   
   public static Object bgl_string_to_procedure_entry(byte [] s)
      {
	 fail( "string->procedure-entry", "not impleemented", s );
	 return unspecified.unspecified;
      }
   
   public static int PROCEDURE_ARITY(procedure p)
      {
	 return p.arity;
      }

   public static int PROCEDURE_LENGTH(procedure p)
      {
	 return p.env.length;
      }

   public static boolean PROCEDURE_CORRECT_ARITYP(procedure p, int i)
      {
	 final int arity = p.arity;

	 return ((arity == i) || ((arity < 0) && (-i - 1 <= arity)));
      }

   public static Object PROCEDURE_SET(procedure p, int i, Object o)
      {
	 p.env[i] = o;
	 return unspecified.unspecified;
      }

   public static Object PROCEDURE_REF(procedure p, int i)
      {
	 return p.env[i];
      }

   public static Object PROCEDURE_ATTR(procedure p)
      {
	 return p.eval;
      }

   public static Object PROCEDURE_ATTR_SET(procedure p, Object v)
      {
	 p.eval = v;
	 return v;
      }

   public static procedure MAKE_EL_PROCEDURE(int n)
      {
	 return new procedure(0, 0, new Object[n]);
      }

   public static Object PROCEDURE_EL_SET(procedure p, int i, Object o)
      {
	 p.env[i] = o;
	 return unspecified.unspecified;
      }

   public static Object PROCEDURE_EL_REF(procedure p, int i)
      {
	 return p.env[i];
      }

   public static Object PROCEDURE_L_REF(procedure p, int i)
      {
	 return p.env[i];
      }

   public static Object PROCEDURE_L_SET(procedure p, int i, Object o)
      {
	 p.env[i] = o;
	 return unspecified.unspecified;
      }

   // CARE ?!?!
   public static Object PUSH_BEFORE(procedure p)
      {
	 return null;
      }

   public static Object POP_BEFORE()
      {
	 return null;
      }

   //////
   // EXCEPTIONS and ERRORS
   //////
   public static final int BGL_ERROR = 1;
   public static final int BGL_LOCATION_ERROR = 2;
   
   public static final int BGL_TYPE_ERROR = 10;
   public static final int BGL_TYPENAME_ERROR = 11;
   public static final int BGL_INDEX_OUT_OF_BOUND_ERROR = 12;
   
   public static final int BGL_IO_ERROR = 20;
   public static final int BGL_IO_PORT_ERROR = 21;
   
   public static final int BGL_IO_READ_ERROR = 31;
   public static final int BGL_IO_WRITE_ERROR = 32;
   public static final int BGL_IO_CLOSED_ERROR = 33;
   public static final int BGL_IO_FILE_NOT_FOUND_ERROR = 34;
   public static final int BGL_IO_UNKNOWN_HOST_ERROR = 35;
   public static final int BGL_IO_PARSE_ERROR = 36;
   public static final int BGL_IO_MALFORMED_URL_ERROR = 37;
   public static final int BGL_IO_SIGPIPE_ERROR = 38;
   public static final int BGL_IO_TIMEOUT_ERROR = 39;
   public static final int BGL_IO_CONNECTION_ERROR = 40;
   public static final int BGL_PROCESS_EXCEPTION = 50;

   private static int throwable_errno( Throwable v ) {
      if( v instanceof FileNotFoundException )
	 return BGL_IO_FILE_NOT_FOUND_ERROR;
      
      if( v instanceof IOException )
	 return BGL_IO_ERROR;

      if( v instanceof UnknownHostException ) {
	 return BGL_IO_UNKNOWN_HOST_ERROR;
      }

      if( v instanceof SocketTimeoutException ) {
	 return BGL_IO_TIMEOUT_ERROR;
      }
      
      if( v instanceof InterruptedIOException ) {
	 return BGL_IO_CONNECTION_ERROR;
      }
      
      if( v instanceof IndexOutOfBoundsException ) {
	 return BGL_INDEX_OUT_OF_BOUND_ERROR;
      }
      
      return BGL_ERROR;
   }

   public static boolean BGL_DYNAMIC_ENVP( Object o )
      {
	 return o instanceof bgldynamic;
      }
   
   public static bgldynamic BGL_CURRENT_DYNAMIC_ENV()
      {
	 return bgldynamic.abgldynamic.get();
      }
   
   public static Object BGL_CURRENT_DISPLAY()
      {
	 return bgldynamic.abgldynamic.get().current_display;
      }

   public static void BGL_CURRENT_DISPLAY_SET(procedure disp)
      {
	 bgldynamic.abgldynamic.get().current_display=disp;
      }

   public static Object BGL_ERROR_HANDLER_GET()
      {
	 return bgldynamic.abgldynamic.get().error_handler;
      }

   public static void BGL_ERROR_HANDLER_SET(Object hdl)
      {
	 bgldynamic.abgldynamic.get().error_handler = hdl;
      }

   public static Object BGL_UNCAUGHT_EXCEPTION_HANDLER_GET()
      {
	 return bgldynamic.abgldynamic.get().uncaught_exception_handler;
      }

   public static void BGL_UNCAUGHT_EXCEPTION_HANDLER_SET(Object hdl)
      {
	 bgldynamic.abgldynamic.get().uncaught_exception_handler = hdl;
      }

   public static Object BGL_INTERRUPT_NOTIFIER_GET()
      {
	 return bgldynamic.abgldynamic.get().interrupt_notifier;
      }

   public static void BGL_INTERRUPT_NOTIFIER_SET(Object hdl)
      {
	 bgldynamic.abgldynamic.get().interrupt_notifier = hdl;
      }

   public static Object BGL_DEBUG_ALIST_GET()
      {
	 return bgldynamic.abgldynamic.get().debug_alist;
      }

   public static void BGL_DEBUG_ALIST_SET(Object al)
      {
	 bgldynamic.abgldynamic.get().debug_alist = al;
      }

   public static Object BGL_LEXICAL_STACK()
      {
	 return bgldynamic.abgldynamic.get().lexical_stack;
      }

   public static void BGL_LEXICAL_STACK_SET(Object al)
      {
	 bgldynamic.abgldynamic.get().lexical_stack = al;
      }

/*    public static Object BGL_ENV_BYTECODE(bgldynamic env)            */
/*       {                                                             */
/* 	 return env.bytecode;                                          */
/*       }                                                             */
/*                                                                     */
/*    public static Object BGL_ENV_BYTECODE_SET(bgldynamic env, Object al) */
/*       {                                                             */
/* 	 env.bytecode = al;                                            */
/* 	 return BUNSPEC;                                               */
/*       }                                                             */

   public static Object BGL_ENV_EVSTATE(bgldynamic env)
      {
	 return env.evstate;
      }

   public static Object BGL_ENV_EVSTATE_SET(bgldynamic env, Object al)
      {
	 env.evstate = al;
	 return BUNSPEC;
      }

   public static Object BGL_MODULE()
      {
	 return bgldynamic.abgldynamic.get().module;
      }

   public static Object BGL_MODULE_SET(Object al)
      {
	 bgldynamic.abgldynamic.get().module = al;
	 return BUNSPEC;
      }

   public static Object BGL_ABASE()
      {
	 return bgldynamic.abgldynamic.get().abase;
      }

   public static Object BGL_ABASE_SET(Object al)
      {
	 bgldynamic.abgldynamic.get().abase = al;
	 return BUNSPEC;
      }

   public static Object BGL_PARAMETERS_SET(Object al)
      {
	 bgldynamic.abgldynamic.get().parameters = al;
	 return al;
      }

   public static Object BGL_PARAMETERS()
      {
	 return bgldynamic.abgldynamic.get().parameters;
      }

   public static Object BGL_THREAD_BACKEND()
      {
	 return bgldynamic.abgldynamic.get().thread_backend;
      }

   public static void BGL_THREAD_BACKEND_SET(Object al)
      {
	 bgldynamic.abgldynamic.get().thread_backend = al;
      }

   public static Object BGL_USER_DATA()
      {
	 return bgldynamic.abgldynamic.get().user_data;
      }

   public static void BGL_USER_DATA_SET(Object al)
      {
	 bgldynamic.abgldynamic.get().user_data = al;
      }

   // Name after the equivalent C function. Its only purpose is to
   // give the JVM based debuggers a chance to stop the execution
   // after an error.
   private static void bigloo_abort() {
      ;
   }
   
   public static final byte[] nomsg = "null".getBytes();

   public static RuntimeException fail(String proc, String msg, Object env)
      {
	 return fail(proc.getBytes(), (msg == null) ? nomsg : msg.getBytes(), env);
      }

   public static void exit(int n)
      {
	 System.exit(n);
      }

   public static Object BGL_EXITD_TOP()
      {
	 return bgldynamic.abgldynamic.get().exitd_top;
      }

   public static boolean BGL_EXITD_BOTTOMP( Object o ) {
      return ((exit)o).prev == null;
   }
      
   public static Object BGL_EXITD_TOP_SET(Object o)
      {
	 bgldynamic.abgldynamic.get().exitd_top = o;
	 return unspecified.unspecified;
      }

   public static Object BGL_EXITD_VAL()
      {
	 return bgldynamic.abgldynamic.get().exitd_val;
      }

   public static Object BGL_EXITD_VAL_SET(Object o)
      {
	 bgldynamic.abgldynamic.get().exitd_val = o;
	 return unspecified.unspecified;
      }

   public static Object setexit()
      {
	 return new exit();
      }

   public static exit EXITD_TO_EXIT(Object o)
      {
	 //print("** self " + o);
	 return (exit) o;
      }

   public static boolean EXITD_USERP(Object o)
      {
	 //print("** userp " + o);
	 return ((exit) o).userp > 0;
      }

   public static boolean EXITD_CALLCCP(Object o)
      {
	 return false;
      }

   public static bint EXITD_STAMP(Object o)
      {
	 //print("** stamp " + o);
	 return bint.BZERO;
      }

   public static Object BGL_EXITD_PROTECT0(exit o)
      {
	 return o.protect0;
      }

   public static void BGL_EXITD_PROTECT0_SET(exit o, Object m)
      {
	 o.protect0 = m;
      }

   public static Object BGL_EXITD_PROTECT1(exit o)
      {
	 return o.protect1;
      }

   public static void BGL_EXITD_PROTECT1_SET(exit o, Object m)
      {
	 o.protect1 = m;
      }

   public static Object BGL_EXITD_PROTECTN(exit o)
      {
	 return o.protectn;
      }

   public static void BGL_EXITD_PROTECTN_SET(exit o, Object m)
      {
	 o.protectn = m;
      }

   public static Object jumpexit(Object excep, Object value)
      {
	 //print("** jump " + excep + " " + value);
	 throw new bexception(excep, value);
      }

   public static int BGL_MVALUES_NUMBER()
      {
	 return bgldynamic.abgldynamic.get().mvalues_number;
      }

   public static int BGL_MVALUES_NUMBER_SET(int n)
      {
	 bgldynamic.abgldynamic.get().mvalues_number = n;
	 return n;
      }

   public static Object BGL_MVALUES_VAL(int n)
      {
	 return bgldynamic.abgldynamic.get().mvalues_values[n];
      }

   public static Object BGL_MVALUES_VAL_SET(int n, Object o)
      {
	 bgldynamic.abgldynamic.get().mvalues_values[n] = o;
	 return unspecified.unspecified;
      }

   public static Object debug_handler(bexception v, exit tag) {
      if (tag.userp == 0)
      {
	 // System.err.println("** PROTECT " + v + " " + tag);
	 return v.value;
      }
      if (v.tag == tag)
      {
	 // System.err.println("** TAG reached " + v + " " + tag);
	 return v.value;
      }
      // System.err.println("** TAG forward " + v + " " + tag);
      throw v;
   }

   private static Boolean err_lock = new Boolean( true );

   public static void notify_exception( Throwable e ) throws Throwable {
      if( e instanceof ClassCastException ) {
	 Pattern p = Pattern.compile( "bigloo.([_A-Za-z0-9]+) cannot be cast to bigloo.([_A-Za-z0-9]+)");
	 Matcher m = p.matcher( e.getMessage() );

	 if( m.matches() ) {
	    String s1 = m.group( 1 );
	    String s2 = m.group( 2 );

	    bigloo.runtime.Llib.error.bgl_system_failure(
	       BGL_TYPENAME_ERROR,
	       stack_trace.get_top(),
	       s2.getBytes(),
	       s1.getBytes() );
	 } else {
	    bigloo.runtime.Llib.error.bgl_system_failure(
	       BGL_TYPE_ERROR,
	       stack_trace.get_top(),
	       e.getMessage().getBytes(),
	       JDK.getExceptionCause( e ) );
	 }
      } else if( e instanceof IndexOutOfBoundsException ) {
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    BGL_INDEX_OUT_OF_BOUND_ERROR,
	    stack_trace.get_top(),
	    e.getMessage().getBytes(),
	    JDK.getExceptionCause( e ) );
      } else {
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    throwable_errno( e ),
	    symbol.make_symbol( e.getClass().getName().getBytes() ),
	    e.getMessage().getBytes(),
	    JDK.getExceptionCause( e ) );
      }
   }
   
   public static Object java_exception_handler(Throwable v, exit tag) {
      if( v instanceof java.lang.StackOverflowError ) {
	 // abort at once because otherwise the handler will crash too!
	 synchronized( err_lock ) {
	    System.err.println( "*** JVM stack overflow error:" );
	    v.printStackTrace( new stackwriter( System.err, true ) );
	 }
      } else {
	 try {
	    notify_exception( v );
	 } catch( bexception be ) {
	    return debug_handler( be, tag );
	 } catch( Throwable _e ) {
	    System.err.println( "Unexpected Java Exception: " +
				v.getClass().getName().getBytes() );
	    v.printStackTrace( new stackwriter( System.err, true ) );
	 }
      }
      
      return unspecified.unspecified;
   }

   public static RuntimeException fail(Object proc, Object msg, Object env) {
      bigloo.runtime.Llib.error.the_failure(proc, msg, env);

      final RuntimeException e = new RuntimeException("bigloo error...");
      final stackwriter sw = new stackwriter(System.err, true);

      e.printStackTrace(sw);
      sw.flush();

      bigloo_abort();
      
      final Object v = bigloo.runtime.Llib.bigloo.bigloo_exit_apply(BINT(1));

      if (v instanceof bint)
	 System.exit(CINT((bint) v));
      else
	 System.exit(1);

      return e;
   }

   public static RuntimeException fail(Object proc, Throwable x, Object env) {
      byte[] msg = (x.getMessage() != null)
	 ? x.getMessage().getBytes() : FOREIGN_TYPE_NAME( x );
      bigloo.runtime.Llib.error.the_failure(proc, msg, env);

      final RuntimeException e = new RuntimeException("bigloo error...");
      final stackwriter sw = new stackwriter(System.err, true);

      e.printStackTrace(sw);
      sw.flush();

      bigloo_abort();
      
      final Object v = bigloo.runtime.Llib.bigloo.bigloo_exit_apply(BINT(1));

      if (v instanceof bint)
	 System.exit(CINT((bint) v));
      else
	 System.exit(1);

      return e;
   }

   public static Throwable fail( Throwable e, Object proc, Object msg, Object env) {
      final stackwriter sw = new stackwriter(System.err, true);

      e.printStackTrace(sw);
      sw.flush();

      bigloo.runtime.Llib.error.the_failure(proc, msg, env);
      bigloo_abort();
      
      final Object v = bigloo.runtime.Llib.bigloo.bigloo_exit_apply(BINT(1));

      if (v instanceof bint)
	 System.exit(CINT((bint) v));
      else
	 System.exit(1);
      
      return e;
   }

   public static void internalerror(Throwable e) throws Throwable {
      try {
	 notify_exception( e );
      } catch( Throwable _t ) {
      } finally {
	 synchronized( err_lock ) {
	    final stackwriter sw = new stackwriter( System.err, true );
	    System.err.println();
	    e.printStackTrace( sw );
	    sw.flush();
	 }
      }
	 
      bigloo_abort();
      System.exit(1);
   }

   public static Object PUSH_EXIT(exit v, int protect) {
//      print("** PUSH " + v + " " + protect + " abgldynamic=" + bgldynamic.abgldynamic.get() + " thread=" + Thread.currentThread());
      v.userp = protect;
      v.prev = (exit) bgldynamic.abgldynamic.get().exitd_top;
      bgldynamic.abgldynamic.get().exitd_top = v;
      return unspecified.unspecified;
   }

   public static Object POP_EXIT() {
//      print("** POP abgldynamic=" + bgldynamic.abgldynamic.get()  + " thread=" + Thread.currentThread());

      try {
	 bgldynamic.abgldynamic.get().exitd_top =
	    ((exit) bgldynamic.abgldynamic.get().exitd_top).prev;
      } catch( Throwable _t ) {
	 System.err.println( "\n\n\n******************* POP_EXIT: " + _t );
      }
      return unspecified.unspecified;
   }

/*    public static Object call_cc(procedure p) {                      */
/*       final exit saved = (exit) bgldynamic.abgldynamic.get().exitd_top; */
/*       final exit me = (exit) setexit();                             */
/*       Object r;                                                     */
/*                                                                     */
/*       PUSH_EXIT(me, 1);                                             */
/*                                                                     */
/*       try {                                                         */
/* 	 if (PROCEDURE_CORRECT_ARITYP(p, 1))                           */
/* 	 {                                                             */
/* 	    r = p.funcall1(new callcc());                              */
/* 	    bgldynamic.abgldynamic.get().exitd_top = saved;            */
/* 	 }                                                             */
/* 	 else                                                          */
/* 	 {                                                             */
/* 	    r = null;                                                  */
/* 	    fail("call/cc", "Wrong arity", p);                         */
/* 	 }                                                             */
/*       } catch(bexception x) {                                       */
/* 	 r = debug_handler(x, me);                                     */
/* 	 bgldynamic.abgldynamic.get().exitd_top = saved;               */
/*       }                                                             */
/*                                                                     */
/*       return r;                                                     */
/*    }                                                                */

   //////
   // EVAL
   //////
   // CARE
   public static final procedure BIGLOO_EXIT_ENV = new procedure();

   public static Object PUSH_TRACE(Object o, Object l)
      {
	 new stack_trace(o,l);
	 return unspecified.unspecified;
      }

   public static Object SET_TRACE_NAME(Object o)
      {
	 stack_trace.set_trace(o);
	 return unspecified.unspecified;
      }

   public static Object POP_TRACE()
      {
	 return stack_trace.pop_trace();
      }

   public static Object BGL_ENV_PUSH_TRACE(bgldynamic env, Object o, Object l)
      {
	 new stack_trace(o,l);
	 return unspecified.unspecified;
      }

   public static Object BGL_ENV_SET_TRACE_NAME(bgldynamic env, Object o)
      {
	 stack_trace.set_trace(o);
	 return unspecified.unspecified;
      }

   public static Object BGL_ENV_SET_TRACE_LOCATION(bgldynamic env, Object o)
      {
	 stack_trace.set_trace_location(o);
	 return unspecified.unspecified;
      }

   public static Object BGL_ENV_POP_TRACE(bgldynamic env)
      {
	 return stack_trace.pop_trace();
      }

   public static Object get_trace_stack(int depth) throws IOException
      {
	 return stack_trace.get(depth);
      }

   public static Object __EVMEANING_ADDRESS_REF(procedure f)
      {
	 return f.funcall0();
      }

   public static Object __EVMEANING_ADDRESS_SET(procedure f, Object v)
      {
	 f.funcall1(v);
	 return unspecified.unspecified;
      }

   public static Object eval_funcall_0(procedure fun)
      {
	 return fun.funcall0();
      }

   public static Object eval_funcall_1(procedure fun, Object a0)
      {
	 return fun.funcall1(a0);
      }

   public static Object eval_funcall_2(procedure fun, Object a0, Object a1)
      {
	 return fun.funcall2(a0, a1);
      }

   public static Object eval_funcall_3(procedure fun,
				       Object a0, Object a1, Object a2)
      {
	 return fun.funcall3(a0, a1, a2);
      }

   public static Object eval_funcall_4(procedure fun,
				       Object a0, Object a1, Object a2, Object a3)
      {
	 return fun.funcall4(a0, a1, a2, a3);
      }

   public static Object eval_apply(procedure fun, Object list)
      {
	 return fun.apply(list);
      }


   //////
   // FILES
   ////

   public static boolean unlink(byte[]file)
      {
	 return !(new File(new String(file)).delete());
      }

   public static boolean rmdir(byte[]file)
      {
	 return !(new File(new String(file)).delete());
      }

   public static boolean is_resourcep(byte[] file) {
      byte[] r = "/resource/".getBytes();

      if( file.length > r.length ) {
	 int i;
	 for( i = 0; i < r.length; i++ ) {
	    if( file[ i ] != r[ i ] )
	       return false;
	 }

	 return true;
      } else {
	 return false;
      }
   }
      
   public static String resource_name(byte[] file) {
      byte[] r = "/resource/".getBytes();
      byte[] name = new byte[ file.length - r.length ];
      
      System.arraycopy( file, r.length, name, 0, file.length - r.length  );

      return new String( name );
   }
      
   public static boolean fexists(byte[]file)
      {
	 if( is_resourcep( file ) ) {
	    return bigloo.input_resource_port.exists( resource_name( file ) );
	 } else {
	    return (new File(new String(file)).exists());
	 }
      }

   public static int rename(byte[]old, byte[]to)
      {
	 return ((new File(new String(old))).
		 renameTo(new File(new String(to))) ? 0 : 69);
      }

   public static boolean truncate(byte[] path, long size)
      {
	 try {
	    FileOutputStream stream = new FileOutputStream(new String(path));
	    try {
	       return JDK.truncate(stream, size);
	    } catch( Exception _e ) {
	       return false;
	    } finally {
	       stream.close();
	    }
	 } catch( Exception _e ) {
	    return false;
	 }
      }

   public static boolean truncate(output_port port, long size)
      {
	 if( port.out instanceof FileOutputStream ) {
	    try {
	       return JDK.truncate((FileOutputStream)port.out, size);
	    } catch( Exception _e ) {
	       return false;
	    }
	 } else {
	    return false;
	 }
      }

   public static boolean mkdir(byte[]path, int mode)
      {
	 return (new File(new String(path))).mkdir();
      }

   public static boolean bgl_directoryp(byte[]file)
      {
	 if( is_resourcep( file ) ) {
	    return bigloo.input_resource_port.bgl_directoryp(resource_name(file));
	 } else {
	    return (new File(new String(file))).isDirectory();
	 }
      }

   public static long bgl_file_size(byte[]file)
      {
	 if( is_resourcep( file ) ) {
	    return bigloo.input_resource_port.file_size( resource_name( file ) );
	 } else {
	    return (long) (new File(new String(file))).length();
	 }
      }

   public static long bgl_last_modification_time(byte[]file)
      {
	 if( is_resourcep( file ) ) {
	    return (long) 0;
	 } else {
	    return (long) (new File(new String(file))).lastModified();
	 }
      }

   public static long bgl_last_change_time(byte[]file)
      {
	 if( is_resourcep( file ) ) {
	    return (long) 0;
	 } else {
	    return (long) 0;
	 }
      }

   public static long bgl_last_access_time(byte[]file)
      {
	 return 0;
      }

   public static int bgl_utime(byte[]file, long atime, long mtime)
      {
	 if( is_resourcep( file ) ) {
	    return 0;
	 } else {
	    if( (new File(new String(file))).setLastModified( mtime ) ) {
	       return 0;
	    } else {
	       return 1;
	    }
	 }
      }

   public static Object bgl_directory_to_list(byte[]name)
      {
	 if( is_resourcep( name ) ) {
	    return bigloo.input_resource_port.bgl_directory_to_list( resource_name( name ) );
	 } else {
	    final String[] files = (new File(new String(name))).list();
	    Object result = BNIL;

	    if (files != null) {
	       final int file_count = files.length;
	    
	       for (int i = 0; i < file_count; ++i)
		  result = new pair(files[file_count - i - 1].getBytes(), result);
	    }
	 
	    return result;
	 }
      }

   public static symbol bgl_file_type( byte[]file )
      {
	 if( is_resourcep( file ) ) {
	    return string_to_symbol( "resource" );
	 } else {
	    File f = (new File(new String(file)));

	    if( !f.exists() ) {
	       return string_to_symbol( "does-not-exist" );
	    }

	    if( f.isDirectory() ) {
	       return string_to_symbol( "directory" );
	    }

	    return string_to_symbol( "regular" );
	 }
      }

   public static int bgl_symlink( byte[] path1, byte[] path2 ) {
      bigloo.runtime.Llib.error.bgl_system_failure(
	 BGL_IO_ERROR,
	 stack_trace.get_top(),
	 "make-symlink",
	 "feature not supported" );
      return 0;
   }

   public static Object bgl_select( int timeout, Object r, Object w, Object e ) {
      BGL_MVALUES_NUMBER_SET( 3 );
      BGL_MVALUES_VAL_SET( 1, BNIL );
      BGL_MVALUES_VAL_SET( 2, BNIL );
      return BNIL;
   }
	 
   public static Object bgl_open_pipes( Object name ) {
      bigloo.runtime.Llib.error.bgl_system_failure(
	 BGL_IO_ERROR,
	 stack_trace.get_top(),
	 "open-pipes", "feature not supported" );
      
      BGL_MVALUES_NUMBER_SET( 1 );
      BGL_MVALUES_VAL_SET( 1, BFALSE );
      
      return BFALSE;
   }
	 
   //////
   // SYSTEM and OS
   //////
   public static final int PTR_ALIGNMENT = 2;
   public static final int SIGHUP = 1;
   public static final int SIGQUIT = 2;
   public static final int SIGINT = 3;
   public static final int SIGILL = 4;
   public static final int SIGTRAP = 5;
   public static final int SIGABRT = 6;
   public static final int SIGKILL = 9;
   public static final int SIGFPE = 8;
   public static final int SIGBUS = 7;
   public static final int SIGSEGV = 11;
   public static final int SIGPIPE = 13;
   public static final int SIGALRM = 14;
   public static final int SIGTERM = 15;
   public static final int SIGUSR1 = 16;
   public static final int SIGUSR2 = 17;
   public static final int SIGWINCH = 20;

   public static int sigsetmask(int n)
      {
	 return n;
      }

   public static Object bgl_signal(int n, Object p)
      {
	 return unspecified.unspecified;
      }

   public static Object bgl_get_signal_handler(int n)
      {
	 return BFALSE;
      }

   public static void bgl_restore_signal_handlers()
      {
	 ;
      }
   
   public static Object reset_console(Object o)
      {
	 return unspecified.unspecified;
      }

   static byte[] get_property(String name, String def)
      {
	 // !!!!! JDK 1.1: System.getProperty( name, def ) ?????
	 final Properties p = System.getProperties();
	 final String s = p.getProperty(name);

	 if (s == null)
	    if (def == null)
	       return null;
	    else
	       return def.getBytes();
	 else
	    return s.getBytes();
      }

   public static byte[] getenv(byte[]name) {
      final String sname = new String(name);

      if (sname.equals("HOME") || sname.equals("USERPROFILE") )
	 return get_property("user.home", null);
      if (sname.equals("USER"))
	 return get_property("user.name", null);
      if (sname.equals("CLASSPATH"))
	 return get_property("java.library.path", null);
      if (sname.equals("TMPDIR"))
	 return get_property("java.io.tmpdir", null);
      if (sname.equals("BIGLOOSTACKDEPTH") || sname.equals("BIGLOOLIVEPROCESS"))
	 return get_property("bigloo." + new String(name), null);
      return get_property("bigloo." + new String(name), null);
   }

   public static boolean getenv_exists( byte[]name ) {
      final String sname = new String( name );
      
      return (sname.equals("HOME")
	      || sname.equals("USERPROFILE")
	      || sname.equals("USER")
	      || sname.equals("CLASSPATH")
	      || sname.equals("TMPDIR")
	      || (get_property("bigloo." + sname, null) != null) );
   }

   public static Object getenv_all() {
      Object res = BNIL;

      res = MAKE_PAIR( 
	 MAKE_PAIR( "HOME".getBytes(),get_property("user.home", null) ),
	 res );
      res = MAKE_PAIR(
	 MAKE_PAIR( "USER".getBytes(),get_property("user.name", null) ),
	 res );
      res = MAKE_PAIR(
	 MAKE_PAIR( "CLASSPATH".getBytes(),get_property("java.library.path", null) ),
	 res );
      res = MAKE_PAIR(
	 MAKE_PAIR( "TMPDIR".getBytes(),get_property("java.io.tmpdir", null) ),
	 res );
      res = MAKE_PAIR(
	 MAKE_PAIR( "BIGLOOSTACKDEPTH".getBytes(),get_property("bigloo.BIGLOOSTACKDEPTH", null) ),
	 res );
      res = MAKE_PAIR(
	 MAKE_PAIR( "BIGLOOLIVEPROCESS".getBytes(),get_property("bigloo.BIGLOOLIVEPROCESS", null) ),
	 res );
      
      return res;
   }

   public static int bgl_setenv(byte[]name, byte[]val)
      {
	 return 0;
      }

   public static Object bgl_time( procedure p ) {
      bgldynamic env = BGL_CURRENT_DYNAMIC_ENV();
      long start, end;
      Object res;
      
      start = System.currentTimeMillis();
      res = p.funcall0();
      end = System.currentTimeMillis();

      env.mvalues_number = 1;
      env.mvalues_values[ 1 ] = BINT( end - start );
      env.mvalues_values[ 2 ] = BINT( 0 );
      env.mvalues_values[ 3 ] = BINT( 0 );
      
      return res;
   }
   
   public static byte[] getcwd(byte[]path, int i)
      {
	 return get_property("user.dir", ".");
      }

   public static int bgl_file_mode(byte[] f) throws IOException
      {
	 return 0;
      }

   public static int bgl_file_uid(byte[] f) throws IOException
      {
	 return 0;
      }

   public static int bgl_file_gid(byte[] f) throws IOException
      {
	 return 0;
      }

   public static boolean bgl_chmod(byte[]f, boolean r, boolean w, boolean x)
      throws IOException
      {
	 if (java.util.Arrays.equals(bigloo.os.OS_CLASS, "unix".getBytes()))
	 {
	    final StringBuffer cmd = new StringBuffer("chmod a");

	    cmd.append(r ? "+r " : "-r ");
	    cmd.append(w ? "+w " : "-w ");
	    cmd.append(x ? "+x " : "-x ");

	    cmd.append(f);

	    final Process process = Runtime.getRuntime().exec(cmd.toString());

	    try {
	       process.waitFor();
	    } catch  (InterruptedException _i) {
	    }

	    return (process.exitValue() == 0);
	 }
	 else
	    return false;
      }

   public static boolean bgl_chmod(byte[]f, int v)
      throws IOException
      {
	 if (java.util.Arrays.equals(bigloo.os.OS_CLASS, "unix".getBytes()))
	 {
	    final String cmd = "chmod " + Integer.toString(v, 8) + " ";
	    final Process process =
	       Runtime.getRuntime().exec(cmd + new String(f));

	    try {
	       process.waitFor();
	    } catch  (InterruptedException _i) {
	    }

	    return (process.exitValue() == 0);
	 }
	 else
	    return false;
      }

   public static boolean chdir(byte[]path)
      {
	 final Properties p = System.getProperties();

	 System.out.println("***WARNING: JVM chdir is not implemented yet");	// !!!!! ?????

	 // !!!!! JDK 1.2:  System.setProperty( "user.dir", new String( path ) );
	 p.put("user.dir", new String(path));
	 return false;
      }

   public static Object command_line = BNIL;

   public static final byte[] executable_name= bigloo.os.BGL_DEFAULT_A_OUT;

   public static int system(byte[]cmd)
      throws IOException, InterruptedException
      {
	 final process p = new process(null, false, true, null,
				       bigloo.foreign.BUNSPEC,
				       bigloo.foreign.BUNSPEC,
				       cmd, null, null);
	  
	 return ((p.xstatus()instanceof bint) ? ((bint) p.xstatus()).value : -1);
      }

   public static long getpid() {
      return 0;
   }

   public static int umask( int m ) {
      return 0;
   }
   
   public static byte[] c_date()
      {
	 return (new Date()).toString().getBytes();
      }

   public static Object BIGLOO_EXIT(Object n)
      {
	 final Object exitv = bigloo.runtime.Llib.bigloo.bigloo_exit_apply(n);

	 trace_exit();

	 System.exit((exitv instanceof bint) ? ((bint) exitv).value : 0);

	 return null;
      }

   public static final byte[] BGL_DYNAMIC_LOAD_INIT =
      "BGL_DYNAMIC_LOAD_INIT".getBytes();

   public static Object bgl_dload( byte[] filename, byte[] init_sym, byte[] mod_sym )
      {
	 return bigloo.dlopen.dload( filename, init_sym, mod_sym );
      }

   public static int bgl_dunload( byte[] filename )
      {
	 return 1;
      }

   public static byte[] bgl_dload_error()
      {
	 return bigloo.dlopen.dload_error();
      }

   public static custom bgl_dlsym( byte[] filename, byte[] id, byte[] mod ) {
      // MS 12mar2019: TODO
      return bgl_custom_nil();
   }

   public static Object bgl_dlsym_get( custom obj ) {
      // MS 12mar2019: TODO
      return BFALSE;
   }

   public static Object bgl_dlsym_set( custom obj, Object val ) {
      // MS 12mar2019: TODO
      return BFALSE;
   }

   
   //////
   // SOCKET
   //////
   public static byte[] bgl_host( byte[] hostname ) {
      try {
	 return java.net.InetAddress.getByName( new String( hostname ) ).getHostAddress().getBytes();
      }
      catch( Exception _e ) {
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    BGL_IO_UNKNOWN_HOST_ERROR,
	    "host".getBytes(),
	    "unknown or misspelled host name".getBytes(),
	    hostname );
	 return "error".getBytes();
      }
   }
   
   public static Object bgl_hostinfo( byte[] hostname ) {
      pair p = MAKE_PAIR( bgl_host( hostname ), BNIL );
      return MAKE_PAIR( MAKE_PAIR( "addresses", p ), BNIL );
   }
   
   public static boolean SOCKETP(Object o)
      {
	 return (o instanceof socket);
      }

   public static boolean BGL_SOCKET_SERVERP(Object o)
      {
	 return (o instanceof server_socket);
      }

   public static boolean BGL_SOCKET_CLIENTP(Object o)
      {
	 return (o instanceof client_socket);
      }

   public static socket bgl_make_client_socket( byte[] hostname,
						int port,
						int timeo,
						byte[] inbuf,
						byte[] outbuf )
      {
	 return new client_socket(hostname, port, inbuf, outbuf );
      }

   public static socket bgl_make_server_socket(Object name,
					       int port,
					       int backlog,
					       boolean ipv6)
      {
	 return new server_socket(name, port);
      }

   public static Object SOCKET_HOSTNAME(socket s)
      {
	 return s.HOSTNAME();
      }

   public static Object SOCKET_HOSTIP(socket s)
      {
	 return s.HOSTIP();
      }

   public static boolean SOCKET_DOWNP(socket s)
      {
	 return s.DOWNP();
      }

   public static int SOCKET_PORT(socket s)
      {
	 return s.PORT();
      }

   public static input_port SOCKET_INPUT(socket s)
      {
	 return (input_port)s.input;
      }

   public static output_port SOCKET_OUTPUT(socket s)
      {
	 return (output_port)s.output;
      }

   public static socket bgl_socket_accept(socket s, boolean errp,
					  byte[] inbuf, byte[] outbuf)
      throws IOException, SecurityException
      {
	 return ((server_socket) s).accept(inbuf, outbuf, errp);
      }

   public static Object socket_host_addr(socket s)
      {
	 return s.HOSTIP();
      }

   public static byte[] socket_local_addr(socket s)
      {
	 return s.local_addr();
      }

   public static int socket_shutdown(socket s, int how) {

	 return s.shutdown(how);
      }

   public static Object socket_close(socket s) {
      s.close();
      return BUNSPEC;
   }

   public static Object bgl_getprotoents()
      {
	 return BNIL;
      }

   public static Object bgl_getprotobyname( byte[] name )
      {
	 return BFALSE;
      }
      
   public static Object bgl_getprotobynumber( int n )
      {
	 return BFALSE;
      }
      
   public static Object bgl_getsockopt( socket s, keyword se )
      {
	 try {
	    return s.getsockopt( se );
	 } catch( IOException _i ) {
	    return BFALSE;
	 }
      }


   public static Object bgl_setsockopt( socket s, keyword se, Object flag )
      {
	 try {
	    return s.setsockopt( se, flag );
	 } catch( IOException _i ) {
	    return BUNSPEC;
	 }
      }

   //////
   // DATAGRAM_SOCKET
   //////
   public static boolean BGL_DATAGRAM_SOCKETP(Object o) {
      return (o instanceof datagram_socket);
   }

   public static boolean BGL_DATAGRAM_SOCKET_SERVERP(Object o) {
      return (o instanceof datagram_server_socket);
   }

   public static boolean BGL_DATAGRAM_SOCKET_CLIENTP(Object o) {
      return (o instanceof datagram_client_socket);
   }

   public static datagram_socket bgl_make_datagram_client_socket( byte[] hostname,
								  int port,
								  boolean bcast ) {
      return new datagram_client_socket( hostname, port, bcast );
   }

   public static datagram_socket bgl_make_datagram_server_socket( int port ) {
      return new datagram_server_socket( port );
   }

   public static datagram_socket bgl_make_datagram_unbound_socket( symbol family ) {
      return new datagram_server_socket( family );
   }

   public static Object BGL_DATAGRAM_SOCKET_HOSTNAME( datagram_socket s ) {
      return s.HOSTNAME();
   }

   public static Object BGL_DATAGRAM_SOCKET_HOSTIP( datagram_socket s) {
      return s.HOSTIP();
   }

   public static obj BGL_DATAGRAM_SOCKET_PORT( datagram_socket s ) {
      return s.PORT();
   }

   public static int BGL_DATAGRAM_SOCKET_PORTNUM( datagram_socket s ) {
      return s.PORTNUM();
   }
   
   public static Object bgl_datagram_socket_close( datagram_socket s ) {
      s.close();
      return BUNSPEC;
   }

   public static Object bgl_datagram_socket_receive( datagram_socket s, int len ) {
      return s.receive( len );
   }

   public static int bgl_datagram_socket_send( datagram_socket s, byte[] string, byte[] host, int port ) {
      try {
	 return s.send( string, host, port );
      } catch( IOException e ) {
	 fail("send", e.getMessage(), s);
	 return -1;
      }
   }

   public static Object bgl_dgetsockopt( datagram_socket s, keyword se )
      {
	 try {
	    return s.getsockopt( se );
	 } catch( IOException _i ) {
	    return BFALSE;
	 }
      }


   public static Object bgl_dsetsockopt( datagram_socket s, keyword se, Object flag )
      {
	 try {
	    return s.setsockopt( se, flag );
	 } catch( IOException _i ) {
	    return BUNSPEC;
	 }
      }

   //////
   // REGEXP
   //////
   public static boolean BGL_REGEXPP(Object o) {
      return o instanceof regexp;
   }

   public static regexp bgl_make_regexp(byte[] pat) {
      return new regexp(pat,false);
   }
   
   public static byte[] BGL_REGEXP_PAT(regexp o) {
      return o.pat;
   }
   
   public static int BGL_REGEXP_CAPTURE_COUNT(regexp o) {
      return -1;
   }
   
   public static Object BGL_REGEXP_PREG(regexp o) {
      return o.preg;
   }
   
   public static Object BGL_REGEXP_PREG_SET(regexp o, Object v) {
      o.preg = v;
      return o;
   }

   public static regexp bgl_regcomp(byte[] pat, Object opt_args, boolean finalize ) {
      return new regexp(pat,true);
   }

   public static Object bgl_regmatch(regexp o, byte[] string, boolean stringp, int beg, int end) {
      return o.match(new String(string, beg, end-beg), stringp, beg);
   }
   
   public static int bgl_regmatch_n(regexp o, byte[] string, Object[] v, int beg, int end) {
      return o.match_n(new String(string, beg, end-beg), v, beg);
   }
   
   public static Object bgl_regfree(regexp o) {
      return o;
   }
   //////
   // INPUT
   //////
   public static final int default_io_bufsiz = 1024;

   public static boolean INPUT_PORTP(Object o)
      {
	 return (o instanceof input_port);
      }

   public static boolean INPUT_STRING_PORTP(Object o)
      {
	 return (o instanceof input_string_port);
      }

   public static boolean INPUT_PROCEDURE_PORTP(Object o)
      {
	 return (o instanceof input_procedure_port);
      }

   public static boolean INPUT_GZIP_PORTP(Object o)
      {
	 return (o instanceof input_gzip_port);
      }

   public static input_port getCurrentInputPort( bgldynamic env )
      {
	 return env.current_input_port;
      }

   public static void setCurrentInputPort(bgldynamic env, input_port o)
      {
	 env.current_input_port = o;
      }

   public static Object OUTPUT_PORT_CHOOK(output_port o)
      {
	 return o.chook;
      }
      
   public static void OUTPUT_PORT_CHOOK_SET(output_port o, procedure p)
      {
	 o.chook = p;
      }
      
   public static Object OUTPUT_PORT_FHOOK(output_port o)
      {
	 return o.fhook;
      }
      
   public static void OUTPUT_PORT_FHOOK_SET(output_port o, Object p)
      {
	 o.fhook = p;
      }
      
   public static Object OUTPUT_PORT_FLUSHBUF(output_port o)
      {
	 return o.flushbuf;
      }
      
   public static void OUTPUT_PORT_FLUSHBUF_SET(output_port o, Object p)
      {
	 o.flushbuf = p;
      }
      
   public static Object INPUT_PORT_CHOOK(input_port o)
      {
	 return o.chook;
      }

   public static void INPUT_PORT_CHOOK_SET(input_port o, procedure p)
      {
	 o.chook = p;
      }
      
   public static Object INPUT_PORT_USEEK(input_port o)
      {
	 return o.userseek;
      }

   public static void INPUT_PORT_USEEK_SET(input_port o, procedure p)
      {
	 o.userseek = p;
      }
      
   public static byte[] BGL_INPUT_PORT_BUFFER(input_port o)
      {
	 return o.buffer;
      }
   public static void bgl_input_port_buffer_set(input_port o, byte[] b)
      {
	 o.buffer = b;
	 
	 if( (o instanceof input_string_port) ) {
	    o.length = b.length;
	 }
      }
   
   public static byte[] BGL_OUTPUT_PORT_BUFFER(output_port o)
      {
	 return new byte[1];
      }
   public static void bgl_output_port_buffer_set(output_port o, byte[] b)
      {
	 return;
      }
   
   public static Object bgl_open_input_file(byte[]s, byte[] b)
      {
	 try {
	    return (input_pipe_port.pipe_name_p(s)
		    ? (Object) new input_pipe_port(s, b)
		    : (Object) new input_file_port(s, b));
	 } catch(IOException _i) {
	    return BFALSE;
	 }
      }

   public static Object bgl_open_input_pipe(byte[]s, byte[] b)
      {
	 return new input_pipe_port(new String(s), b);
      }

   public static Object bgl_open_input_resource(byte[]s, byte[] b)
      throws IOException
      {
	 return new input_resource_port(new String(s), b);
      }

   public static int bgl_input_port_bufsiz(input_port p)
      {
	 return p.buffer.length;
      }

   public static input_port bgl_open_input_string(byte[]s, int start)
      {
	 return new input_string_port(s, start, s.length);
      }

   public static input_port bgl_open_input_substring(byte[]s, int start, int end)
      {
	 return new input_string_port(s, start, end);
      }

   public static input_port bgl_open_input_substring_bang(byte[]s, int start, int end)
      {
	 return new input_string_port( s, start, end, true );
      }

   public static Object bgl_open_input_procedure(procedure p, byte[] b)
      {
	 return new input_procedure_port( p, b );
      }

   public static Object bgl_open_input_gzip_port(procedure p, input_port ip, byte[] b ) {
      return new input_gzip_port( p, ip, b );
   }

   public static input_port BGL_INPUT_GZIP_PORT_INPUT_PORT( input_port p ) {
      if( p instanceof input_gzip_port )
	 return ((input_gzip_port)p).input_port;
      else {
	 fail( "input-gzip-port-input-port", "Illegal input port", p );
	 return p;
      }
   }

   public static Object bgl_open_input_c_string(byte[]s)
      {
	 return new input_string_port(s, 0, s.length);
      }

   public static Object bgl_reopen_input_c_string(input_port p, byte[]s)
      {
	 ((input_string_port) p).reopen_input_c_string(s);
	 return p;
      }

   public static boolean bgl_input_port_timeout_set(input_port p, int to) {
      return p.timeout_set( to / 1000 );
   }
   
   public static boolean bgl_output_port_timeout_set(output_port p, int to) {
      return false;
   }
   
   public static void bgl_input_port_seek(input_port p, int pos)
      {
	 try {
	    p.bgl_input_port_seek(pos);
	 } catch( Exception e ) {
	    fail("set-input-port-position!", e.getMessage(), p);
	 }
      }

   public static Object bgl_input_port_reopen(input_port p) throws IOException
      {
	 return p.bgl_input_port_reopen();
      }

   public static Object bgl_input_port_clone(input_port dst, input_port src) throws IOException
      {
	 return dst.bgl_input_port_clone( src );
      }

   public static Object bgl_output_port_seek(output_port p,
					     int pos) throws IOException
      {
	 return p.bgl_output_port_seek(pos);
      }

   public static boolean CLOSED_RGC_BUFFER(input_port o)
      {
	 return o.other_eof;
      }

   public static boolean CLOSED_OUTPUT_PORT(output_port o)
      {
	 return o.isclosed;
      }

   public static int INPUT_PORT_FILEPOS(input_port p)
      {
	 return p.filepos;
      }

   public static int INPUT_PORT_FILLBARRIER(input_port p)
      {
	 return p.pseudoeof;
      }

   public static void INPUT_PORT_FILLBARRIER_SET(input_port p, int e)
      {
	 p.pseudoeof = e;
      }

   public static long BGL_INPUT_PORT_LENGTH(input_port p)
      {
	 return p.length;
      }

   public static void BGL_INPUT_PORT_LENGTH_SET(input_port p, long e)
      {
	 p.length = e;
      }

   public static int INPUT_PORT_TOKENPOS(input_port p)
      {
	 return INPUT_PORT_FILEPOS(p)-RGC_BUFFER_MATCH_LENGTH(p);
      }

   public static int OUTPUT_PORT_FILEPOS(output_port p)
      {
	 return 0;
      }

   public static byte[] INPUT_PORT_NAME(input_port p)
      {
	 return p.name.getBytes();
      }

   public static void INPUT_PORT_NAME_SET(input_port p, byte[] v)
      {
	 p.name = new String( v );
      }

   public static byte[] OUTPUT_PORT_NAME(output_port p)
      {
	 return p.name;
      }

   public static void OUTPUT_PORT_NAME_SET(output_port p, byte[] v)
      {
	 p.name = v;
      }

   public static int RGC_BUFFER_POSITION(input_port p, int forward)
      {
	 return (forward - p.matchstart);
      }

   public static int RGC_BUFFER_FORWARD(input_port p)
      {
	 return p.forward;
      }

   public static int RGC_BUFFER_BUFPOS(input_port p)
      {
	 return p.bufpos;
      }

    public static int RGC_BUFFER_GET_CHAR(input_port p, int offset)
      {
	 return (int)(p.buffer[offset] & 0xFF);
      }

   public static int rgc_buffer_unget_char(input_port p, int c)
      {
	 p.filepos--;
      
	 if (0 < p.matchstop) { 
	    --p.matchstop;
	 } else {
	    p.buffer[0] = (byte) c;
	 }
	 return c;
      }

   private static void rgc_buffer_reserve_space(input_port p, int amount)
      {
	 int bufsize = p.buffer.length;
	 int bufpos = p.bufpos;
	 int matchstop = p.matchstop;

	 if ( matchstop >= amount ) return;

	 if ( (matchstop + (bufsize - (bufpos))) >= amount ) {
	    // shift the buffer to the right
	    int diff = amount - matchstop;

	    System.arraycopy(p.buffer, matchstop, p.buffer, amount, bufpos - matchstop);

	    p.bufpos += diff;
	    p.matchstop += diff;
	 } else {
	    p.rgc_double_buffer();
	    rgc_buffer_reserve_space(p, amount);
	 }
      }
   public static boolean rgc_buffer_insert_substring(input_port p, byte[] s, int from, int to)
      {
	 if ( from < 0 ) return false;
	 if ( to > s.length ) return false;
	 if ( (p.buffer.length == 2) && !(p instanceof input_string_port))
	    return false; // unbuffered port
	 if ( CLOSED_RGC_BUFFER( p )) return false;
	 if ( from >= to ) return true;

	 int len = to - from;

	 rgc_buffer_reserve_space(p, len);

	 int matchstop = p.matchstop;

	 System.arraycopy(s, from, p.buffer, (matchstop - len), len);

	 if ( p.filepos >= len )
	    p.filepos -= len;
	 else
	    p.filepos = 0;

	 p.matchstop -= len;
	 p.forward    = p.matchstop;
	 p.matchstart = p.matchstop;
	 return true;
      }
   public static boolean rgc_buffer_insert_char(input_port p, int c)
      {
	 if ( (p.buffer.length == 2) && !(p instanceof input_string_port))
	    return false;
	    
	 rgc_buffer_reserve_space(p, 1);

	 int matchstop = p.matchstop;

	 p.buffer[matchstop - 1] = (byte) c;

	 if ( p.filepos > 0 )
	    p.filepos--;
	 else
	    p.filepos = 0;

	 p.matchstop--;
	 p.forward    = p.matchstop;
	 p.matchstart = p.matchstop;
	 return true;
      }

   public static int RGC_START_MATCH(input_port p)
      {
	 return (p.forward = p.matchstart = p.matchstop);
      }

   public static int RGC_STOP_MATCH(input_port p, int forward)
      {
	 return (p.matchstop = forward);
      }

   public static int RGC_SET_FILEPOS(input_port p)
      {
	 return (p.filepos += (p.matchstop - p.matchstart));
      }

   public static int RGC_BUFFER_MATCH_LENGTH(input_port p)
      {
	 return (p.matchstop - p.matchstart);
      }

   public static boolean bgl_rgc_charready(input_port p)
      {
	 return p.rgc_charready();
      }

   public static byte[] bgl_password( byte[] prompt )
      {
	 return JDK.password( prompt );
      }
   
   public static boolean rgc_buffer_eof_p(input_port p)
      {
	 return ((p.buffer.length == p.forward) && (p.forward == p.bufpos));
      }

   public static boolean rgc_buffer_eof2_p(input_port p, int forward, int bufpos)
      {
	 if( forward < bufpos ) {
	    p.forward = forward;
	    p.bufpos = bufpos;
	    return false;
	 } else {
	    if( p.eof ) {
	       p.forward = forward;
	       p.bufpos = bufpos;
	       return true;
	    } else {
	       boolean r = rgc_fill_buffer( p );
	       return !r;
	    }
	 }
      }

   private static int rgc_do_blit(input_port p, byte[]s, int o, int l)
      throws IOException
      {
	 RGC_START_MATCH(p);

	 while (((p.bufpos - p.matchstart) <= l) && !p.eof) {
	    // MS fix 6 Juillet 2005
	    int fsize = (p.buffer.length - p.bufpos);
	    p.forward = p.bufpos;
	    rgc_fill_buffer(p);
	    // less characters are available
	    if( (p.bufpos - p.forward) < fsize ) break;
	 }

	 if ((p.bufpos - p.matchstart) <= l)
	    l = (p.bufpos - p.matchstart);

	 p.forward = p.matchstart + l;
	 RGC_STOP_MATCH(p,p.forward);
	 RGC_SET_FILEPOS(p);
	 System.arraycopy(p.buffer, p.matchstart, s, o, l);

	 return l;
      }

   public static int bgl_rgc_blit_string(input_port p, byte[]s, int o, int l)
      throws IOException
      {
	 final int bs = p.buffer.length;

	 if( CLOSED_RGC_BUFFER( p ) ) {
	    bigloo.runtime.Llib.error.bgl_system_failure(
	       BGL_IO_CLOSED_ERROR,
	       "rgc-blit-string".getBytes(),
	       "input-port closed".getBytes(),
	       p );
	 }
      
	 if (l <= bs)
	    return rgc_do_blit(p, s, o, l);
	 else {
	    int result = 0;
	    while (bs < l) {
	       int r = rgc_do_blit(p, s, o, bs);
	       result += r;
	       o += bs;
	       l -= bs;
	       if( r < l ) break;
	    }
	    result += rgc_do_blit(p, s, o, l);
	    return result;
	 }
      }

   public static boolean rgc_buffer_bof_p(input_port p)
      {
	 return (p.filepos == 0);
      }

   public static boolean rgc_buffer_bol_p(input_port p)
      {
	 if (p.matchstart > 0)
	    return (p.buffer[p.matchstart - 1] == '\n');
	 else
	    return (p.lastchar == (byte) '\n');
      }

   public static boolean rgc_buffer_eol_p(input_port p, int forward, int bufpos ) throws IOException
      {
	 if( forward == bufpos ) {
	    if (rgc_fill_buffer(p))
	       return rgc_buffer_eol_p(p, p.forward, p.bufpos );
	    else
	       return false;
	 } else {
	    p.forward = forward;
	    p.bufpos = bufpos;
	    return RGC_BUFFER_GET_CHAR(p, forward) == (byte)'\n';
	 }
      }

   public static boolean rgc_fill_buffer(input_port p) {
      p.forward = p.bufpos;
      
      if (p.eof) { 
	 return false;
      }

      try {
	 return p.rgc_fill_buffer();
      } catch( Exception e ) {
	 String msg = e.getMessage();

	 final stackwriter sw = new stackwriter( System.out, true );
	 e.printStackTrace( sw );
	 
	 bigloo.runtime.Llib.error.bgl_system_failure(
	    (e instanceof java.net.SocketTimeoutException ?
	     BGL_IO_TIMEOUT_ERROR : BGL_IO_READ_ERROR),
	    "read".getBytes(),
	    (msg != null ? msg.getBytes() : FOREIGN_TYPE_NAME( e )),
	    p );
	 return false;
      }
   }
      
   public static byte[] rgc_buffer_substring(input_port p, int o, int e)
      {
	 return c_substring(p.buffer, p.matchstart + o, p.matchstart + e);
      }

   public static byte[] rgc_buffer_escape_substring(input_port p, int o, int e, boolean strict )
      {
	 if( strict ) {
	    return bgl_escape_scheme_string(p.buffer, p.matchstart + o, p.matchstart + e);
	 } else {
	    return bgl_escape_C_string(p.buffer, p.matchstart + o, p.matchstart + e);
	 }
      }

   public static byte RGC_BUFFER_CHARACTER(input_port p)
      {
	 return p.buffer[p.matchstart];
      }

   public static int RGC_BUFFER_BYTE(input_port p)
      {
	 return ((int)(char)p.buffer[p.matchstart]) & 0xFF;
      }

   public static int RGC_BUFFER_BYTE_REF(input_port p, int offset)
      {
	 return ((int)(char)p.buffer[p.matchstart + offset]) & 0xFF;
      }

   public static symbol rgc_buffer_symbol(input_port p)
      {
	 int start = p.matchstart;
	 final int stop = p.matchstop;
	 final int n = stop - start;
	 final byte[] name = new byte[n];

	 for (int i = 0; i < n; ++i, ++start)
	    name[i] = (byte) (p.buffer[start] & 0xFF);

	 return symbol.make_symbol(name);
      }

   public static symbol rgc_buffer_subsymbol(input_port p, int o, int e)
      {
	 int start = p.matchstart + o;
	 final int stop = p.matchstart + e;
	 final int n = stop - start;
	 final byte[] name = new byte[n];

	 for (int i = 0; i < n; ++i, ++start)
	    name[i] = (byte) (p.buffer[start] & 0xFF);

	 return symbol.make_symbol(name);
      }

   public static symbol rgc_buffer_upcase_subsymbol(input_port p, int o, int e)
      {
	 int start = p.matchstart + o;
	 final int stop = p.matchstart + e;
	 final int n = stop - start;
	 final byte[] name = new byte[n];

	 for (int i = 0; i < n; ++i, ++start)
	    name[i] = (byte) toupper(p.buffer[start] & 0xFF);

	 return symbol.make_symbol(name);
      }

   public static symbol rgc_buffer_downcase_subsymbol(input_port p, int o, int e)
      {
	 int start = p.matchstart + o;
	 final int stop = p.matchstart + e;
	 final int n = stop - start;
	 final byte[] name = new byte[n];

	 for (int i = 0; i < n; ++i, ++start)
	    name[i] = (byte) tolower(p.buffer[start] & 0xFF);

	 return symbol.make_symbol(name);
      }

   public static keyword rgc_buffer_upcase_keyword(input_port p)
      {
	 int start = p.matchstart;
	 final int stop = p.matchstop;
	 final int n = stop - start - 1;
	 final byte[] name = new byte[n];

	 if( p.buffer[start] == ':' ) start++;

	 for (int i = 0; i < n; ++i, ++start)
	    name[i] = (byte) toupper(p.buffer[start] & 0xFF);

	 return keyword.make_keyword(name);
      }

   public static keyword rgc_buffer_downcase_keyword(input_port p)
      {
	 int start = p.matchstart;
	 final int stop = p.matchstop;
	 final int n = stop - start - 1;
	 final byte[] name = new byte[n];

	 if( p.buffer[start] == ':' ) start++;

	 for (int i = 0; i < n; ++i, ++start)
	    name[i] = (byte) tolower(p.buffer[start] & 0xFF);

	 return keyword.make_keyword(name);
      }

   public static keyword rgc_buffer_keyword(input_port p)
      {
	 int start = p.matchstart;
	 final int stop = p.matchstop;
	 final int n = stop - start - 1;
	 final byte[] name = new byte[n];

	 if( p.buffer[start] == ':' ) start++;

	 for (int i = 0; i < n; ++i, ++start)
	    name[i] = (byte) (p.buffer[start] & 0xFF);

	 return keyword.make_keyword(name);
      }

   public static int rgc_buffer_fixnum(input_port p)
      {
	 return parseint(p.buffer, p.matchstart, p.matchstop, 10);
      }

   public static Object rgc_buffer_integer(input_port p)
      {
	 return parseinteger(p.buffer, p.matchstart, p.matchstop, 10);
      }

   public static double rgc_buffer_flonum(input_port p)
      {
	 // !!!!! JDK 1.2:  return Double.parseDouble( new String( c_substring( p.buffer, p.matchstart, p.matchstop ) ) );
	 return Double.valueOf(new String(c_substring(p.buffer, p.matchstart,
						      p.matchstop))).doubleValue();
      }

   public static Object bgl_close_input_port(input_port p) throws IOException
      {
	 p.close();
	 return p;
      }

   public static boolean reset_eof(Object p)
      {
	 //print("RES_EOF " + p);
	 return ((input_port) p).reset_eof();
      }

   //////
   // BINARY
   //////

   public static boolean BINARY_PORTP(Object o)
      {
	 return (o instanceof binary_port);
      }

   public static boolean BINARY_PORT_INP(binary_port p)
      {
	 return (p.stream instanceof InputStream);
      }

   public static boolean BINARY_PORT_OUTP(binary_port p)
      {
	 return (p.stream instanceof OutputStream);
      }

   public static Object BINARY_PORT_TO_FILE(binary_port p)
      {
	 return p.stream;
      }

   public static Object open_output_binary_file(byte[]file)
      throws IOException
      {
	 return new binary_port(new FileOutputStream(new String(file)));
      }

   public static Object append_output_binary_file(byte[]file)
      throws IOException
      {
	 return new binary_port(new FileOutputStream(new String(file), true));
      }

   public static Object open_input_binary_file(byte[]file)
      throws FileNotFoundException
      {
	 String s = new String( file );
	 if( s.startsWith( "/resource/" ) ) {
	    String r = s.substring( 10 ).replace( '\\', '/' );
	    InputStream in = foreign.class.getClassLoader().getResourceAsStream( r );
	    if( in == null ) {
	       return BFALSE;
	    } else {
	       return new binary_port( in );
	    }
	 } else {
	    try {
	       return new binary_port(new FileInputStream(new String(file)));
	    } catch( Exception _e ) {
	       return BFALSE;
	    }
	 }
      }

   public static Object close_binary_port(binary_port p)
      throws IOException
      {
	 return p.close();
      }

   public static Object bgl_flush_binary_port(binary_port p)
      throws IOException
      {
	 return p.flush();
      }

   public static Object output_obj(binary_port p, Object obj)
      throws IOException
      {
	 return p.output_obj(obj);
      }

   public static Object input_obj(binary_port p)
      throws IOException
      {
	 return p.input_obj();
      }

   public static int BGL_INPUT_CHAR(binary_port p)
      throws IOException
      {
	 return ((FileInputStream) p.stream).read();
      }

   public static boolean BGL_INT_EOFP(int i)
      {
	 return (i == -1);
      }

   public static Object BGL_OUTPUT_CHAR(binary_port p, byte c)
      throws IOException
      {
	 ((FileOutputStream) p.stream).write(c);
	 return p;
      }

   public static byte[] bgl_input_string(binary_port p, int len)
      throws IOException
      {
	 byte[] buf = new byte[ len ];
	 int l = ((FileInputStream) p.stream).read(buf);

	 if( l < len )
	    return bgl_string_shrink( buf, l );
	 else
	    return buf;
      }

   public static int bgl_input_fill_string(binary_port p, byte[] buf)
      throws IOException
      {
	 return ((InputStream) p.stream).read(buf);
      }

   public static int bgl_output_string(binary_port p, byte[] buf)
      throws IOException
      {
	 ((OutputStream) p.stream).write(buf);
	 return buf.length;
      }

   //////
   // OUTPUT
   //////
   public static output_port getCurrentOutputPort( bgldynamic env )
      {
	 return env.current_output_port;
      }

   public static void setCurrentOutputPort(bgldynamic env, output_port o)
      {
	 env.current_output_port = o;
      }

   public static output_port getCurrentErrorPort( bgldynamic env )
      {
	 return env.current_error_port;
      }

   public static void setCurrentErrorPort(bgldynamic env, output_port o)
      {
	 env.current_error_port = o;
      }

   public static boolean OUTPUT_PORTP(Object o)
      {
	 return (o instanceof output_port);
      }

   public static boolean OUTPUT_STRING_PORTP(Object o)
      {
	 return (o instanceof output_string_port);
      }

   public static boolean OUTPUT_PROCEDURE_PORTP(Object o)
      {
	 return (o instanceof output_procedure_port);
      }

   public static Object FLUSH_OUTPUT_PORT(output_port p) throws IOException
      {
	 return p.flush();
      }

   public static Object bgl_reset_output_string_port(output_port p) throws IOException
      {
	 return ((output_string_port)p).reset();
      }

   public static Object bgl_reset_output_port_error(output_port p) {
      return p;
   }

   public static Object bgl_open_output_file(byte[]file, byte[] buf) throws IOException
      {
	 if( output_pipe_port.pipe_name_p(file) )
	    return (Object) new output_pipe_port( file, buf );
	 
	 if (bigloo_strcmp( file, "null:".getBytes() ))
	    return output_buffered_port.make_output_buffered_port( (foreign.bigloo_strcmp( os.OS_CLASS, "unix".getBytes() )
					      ? "/dev/null"
					      : "NUL:").getBytes(), buf );
	 else
	    return output_buffered_port.make_output_buffered_port(file, buf);
      }

   public static Object bgl_append_output_file(byte[]file, byte[] buf) throws IOException
      {
	 return output_buffered_port.make_output_buffered_port(file, buf, true);
      }

   public static output_port bgl_open_output_string(byte[] buf)
      {
	 return new output_string_port();
      }

   public static Object bgl_open_output_procedure(procedure p, procedure f, procedure c, byte[] buf)
      {
	 return new output_procedure_port(p, f, c);
      }

   public static byte[] get_output_string(output_port p)
      {
	 // CARE why not a correct signature in Scheme
	 return ((output_string_port) p).get_string();
      }

   public static Object bgl_close_output_port(output_port p) throws IOException
      {
	 return p.close();
      }

   public static Object display_char(int cn, output_port p)
      {
	 p.write(cn);
	 return p;
      }

   public static Object display_char(byte cn, output_port p)
      {
	 p.write(cn);
	 return p;
      }

   private static final String[] char_name = { "", "", "", "", "", "", "", "",
					       "", "tab", "newline", "", "", "return", "", "",
					       "", "", "", "", "", "", "", "",
					       "", "", "", "", "", "", "", "",
					       "space", "!", "\"", "#", "$", "%", "&", "'",
					       "(", ")", "*", "+", ",", "-", ".", "/",
					       "0", "1", "2", "3", "4", "5", "6", "7",
					       "8", "9", ":", ";", "<", " =", ">", "?",
					       "@", "A", "B", "C", "D", "E", "F", "G",
					       "H", "I", "J", "K", "L", "M", "N", "O",
					       "P", "Q", "R", "S", "T", "U", "V", "W",
					       "X", "Y", "Z", "[", "\\", "]", "^", "_",
					       "`", "a", "b", "c", "d", "e", "f", "g",
					       "h", "i", "j", "k", "l", "m", "n", "o",
					       "p", "q", "r", "s", "t", "u", "v", "w",
					       "x", "y", "z", "{", "|", "}", "~", ""
   };

   public static Object write_char(bchar c, output_port p) {
      final int cn = c.value & 0xFF;

      if ((0 < cn) && (cn < 128))
      {
	 final String rep = char_name[cn];

	 if (rep.length() != 0)
	 {
	    p.write("#\\");
	    p.write(rep);
	    return p;
	 }
      }

      p.write("#\\x");
      p.write(hexa[cn >> 4]);
      p.write(hexa[cn & 0xf]);

      return p;
   }

   private static final byte[] hexa = {
      (byte) '0', (byte) '1', (byte) '2', (byte) '3',
      (byte) '4', (byte) '5', (byte) '6', (byte) '7',
      (byte) '8', (byte) '9', (byte) 'a', (byte) 'b',
      (byte) 'c', (byte) 'd', (byte) 'e', (byte) 'f'
   };

   public static Object write_ucs2(bucs2 s, output_port p) {
      final int value = s.value;

      p.write("#u");
      p.write(hexa[(value & 0xf000) >> 12]);
      p.write(hexa[(value & 0x0f00) >> 8]);
      p.write(hexa[(value & 0x00f0) >> 4]);
      p.write(hexa[(value & 0x000f)]);

      return p;
   }

   public static Object display_ucs2(bucs2 s, output_port p)
      {
	 p.write(s.value);
	 return p;
      }

   public static Object write_object(Object o, output_port p)
      {
	 if (o == null)
	    p.write("#<jvm:null>");
	 else if (o instanceof obj)
	    ((obj) o).write(p);
	 else
	    p.write(o.toString());
	 return p;
      }

   public static Object write_string(byte[]s, boolean b, output_port p)
      {
	 if (b)
	    p.write((byte) '#');
	 p.write((byte) '\"');
	 p.write(s);
	 p.write((byte) '\"');
	 return p;
      }

   public static Object display_string(byte[]s, output_port p)
      {
	 p.write(s);
	 return p;
      }

   public static Object display_substring(byte[]s, int start, int end, output_port p)
      {
	 p.write(s, start, end);
	 return p;
      }

   public static Object display_fixnum(bint n, output_port p)
      {
	 p.write(Integer.toString(n.value));
	 return p;
      }

   public static Object display_flonum(real n, output_port p)
      {
	 p.write(Double.toString(n.value));
	 return p;
      }

   public static Object write_elong(long n, output_port p)
      {
	 p.write("#e".getBytes());
	 p.write(Long.toString(n));
	 return p;
      }

   public static Object display_elong(long n, output_port p)
      {
	 p.write(Long.toString(n));
	 return p;
      }

   public static Object write_llong(long n, output_port p)
      {
	 p.write("#l".getBytes());
	 p.write(Long.toString(n));
	 return p;
      }

   public static Object display_llong(long n, output_port p)
      {
	 p.write(Long.toString(n));
	 return p;
      }

   public static Object bgl_write_bignum(bignum n, output_port p)
   {
      p.write("#z".getBytes());
      p.write(n.value.toString().getBytes());
      return p;
   }

   public static Object bgl_display_bignum(bignum n, output_port p)
   {
      p.write(n.value.toString().getBytes());
      return p;
   }

   public static Object write_utf8string(byte[]s, output_port p)
      {
	 p.write(ucs2_string_to_utf8_string("#u\"" + new String(s) + "\""));
	 return p;
      }

   public static Object display_ucs2string(char[]s, output_port p)
      {
	 p.write(new String(s));
	 return p;
      }

   //////
   // HASH
   //////
   private static final byte[] hash_random_table =
   { (byte) 1, (byte) 14, (byte) 110, (byte) 25, (byte) 97, (byte) 174,
     (byte) 132,
     (byte) 119, (byte) 138, (byte) 170, (byte) 125, (byte) 118, (byte) 27,
     (byte) 233, (byte) 140, (byte) 51,
     (byte) 87, (byte) 197, (byte) 177, (byte) 107, (byte) 234, (byte) 169,
     (byte) 56, (byte) 68, (byte) 30, (byte) 7, (byte) 173, (byte) 73,
     (byte) 188,
     (byte) 40, (byte) 36, (byte) 65,
     (byte) 49, (byte) 213, (byte) 104, (byte) 190, (byte) 57, (byte) 211,
     (byte) 148, (byte) 223, (byte) 48, (byte) 115, (byte) 15, (byte) 2,
     (byte) 67,
     (byte) 186, (byte) 210, (byte) 28,
     (byte) 12, (byte) 181, (byte) 103, (byte) 70, (byte) 22, (byte) 58,
     (byte) 75,
     (byte) 78, (byte) 183, (byte) 167, (byte) 238, (byte) 157, (byte) 124,
     (byte) 147, (byte) 172, (byte) 144,
     (byte) 176, (byte) 161, (byte) 141, (byte) 86, (byte) 60, (byte) 66,
     (byte) 128,
     (byte) 83, (byte) 156, (byte) 241, (byte) 79, (byte) 46, (byte) 168,
     (byte) 198,
     (byte) 41, (byte) 254,
     (byte) 178, (byte) 85, (byte) 253, (byte) 237, (byte) 250, (byte) 154,
     (byte) 133, (byte) 88, (byte) 35, (byte) 206, (byte) 95, (byte) 116,
     (byte) 252, (byte) 192, (byte) 54, (byte) 221,
     (byte) 102, (byte) 218, (byte) 255, (byte) 240, (byte) 82, (byte) 106,
     (byte) 158, (byte) 201, (byte) 61, (byte) 3, (byte) 89, (byte) 9,
     (byte) 42,
     (byte) 155, (byte) 159, (byte) 93,
     (byte) 166, (byte) 80, (byte) 50, (byte) 34, (byte) 175, (byte) 195,
     (byte) 100,
     (byte) 99, (byte) 26, (byte) 150, (byte) 16, (byte) 145, (byte) 4,
     (byte) 33,
     (byte) 8, (byte) 189,
     (byte) 121, (byte) 64, (byte) 77, (byte) 72, (byte) 208, (byte) 245,
     (byte) 130,
     (byte) 122, (byte) 143, (byte) 55, (byte) 105, (byte) 134, (byte) 29,
     (byte) 164, (byte) 185, (byte) 194,
     (byte) 193, (byte) 239, (byte) 101, (byte) 242, (byte) 5, (byte) 171,
     (byte) 126, (byte) 11, (byte) 74, (byte) 59, (byte) 137, (byte) 228,
     (byte) 108, (byte) 191, (byte) 232, (byte) 139,
     (byte) 6, (byte) 24, (byte) 81, (byte) 20, (byte) 127, (byte) 17,
     (byte) 91,
     (byte) 92, (byte) 251, (byte) 151, (byte) 225, (byte) 207, (byte) 21,
     (byte) 98, (byte) 113, (byte) 112,
     (byte) 84, (byte) 226, (byte) 18, (byte) 214, (byte) 199, (byte) 187,
     (byte) 13, (byte) 32, (byte) 94, (byte) 220, (byte) 224, (byte) 212,
     (byte) 247, (byte) 204, (byte) 196, (byte) 43,
     (byte) 249, (byte) 236, (byte) 45, (byte) 244, (byte) 111, (byte) 182,
     (byte) 153, (byte) 136, (byte) 129, (byte) 90, (byte) 217, (byte) 202,
     (byte) 19, (byte) 165, (byte) 231, (byte) 71,
     (byte) 230, (byte) 142, (byte) 96, (byte) 227, (byte) 62, (byte) 179,
     (byte) 246, (byte) 114, (byte) 162, (byte) 53, (byte) 160, (byte) 215,
     (byte) 205, (byte) 180, (byte) 47, (byte) 109,
     (byte) 44, (byte) 38, (byte) 31, (byte) 149, (byte) 135, (byte) 0,
     (byte) 216, (byte) 52, (byte) 63, (byte) 23, (byte) 37, (byte) 69,
     (byte) 39,
     (byte) 117, (byte) 146, (byte) 184,
     (byte) 163, (byte) 200, (byte) 222, (byte) 235, (byte) 248, (byte) 243,
     (byte) 219, (byte) 10, (byte) 152, (byte) 131, (byte) 123, (byte) 229,
     (byte) 203, (byte) 76, (byte) 120, (byte) 209
   };

   public static int get_hash_number(byte[]s)
      {
	 byte hash = 0;

	 for (int i = 0; i < s.length; ++i)
	    hash = hash_random_table[(hash ^ s[i]) & 0xFF];
	 return (hash & 0xFF);
      }

   public static int get_hash_power_number(byte[]str, int power)
      {
	 int result = 0;

	 for (int i = 0; i < str.length; ++i)
	    result += (result << 3) + str[i];
	 return (result & ((1 << power) - 1));
      }

   public static int get_hash_number_from_int(int i)
      {
	 byte hash = 0;

	 while (i != 0)
	 {
	    hash = hash_random_table[(int) ((hash ^ i) & 0xFF)];
	    i >>>= 8;
	 }
	 return (hash & 0xFF);
      }

   public static int get_hash_number_from_int(Object o)
      {
	 return get_hash_number_from_int(o.hashCode());
      }

   public static int get_hash_power_number_from_int(int i, int power)
      {
	 int result = 0;

	 while (i != 0)
	 {
	    result += (result << 3) + (i & 0xFF);
	    i >>>= 8;
	 }
	 return (result & ((1 << power) - 1));
      }

   public static int get_hash_power_number_from_int(Object i, int power)
      {
	 final int hash_code = i.hashCode();

	 return (hash_code & ((1 << power) - 1));
      }

   public static int bgl_string_hash_number(byte[]s)
      {
	 int result = 5381;

	 for (int i = 0; i < s.length; ++i)
	    result += (result << 5) + s[i];

	 return result & ((1 << 29) - 1);
      }

   public static int bgl_string_hash(byte[]s, int start, int len)
      {
	 int result = 5381;

	 for (int i = start; i < len; ++i, i++)
	    result += (result << 5) + s[i];
	 return result & ((1 << 29) - 1);
      }

   public static int bgl_symbol_hash_number(symbol obj)
      {
	 return (1 + bgl_string_hash_number(SYMBOL_TO_STRING(obj)));
      }

   public static int bgl_keyword_hash_number(keyword obj)
      {
	 return (2 + bgl_string_hash_number(KEYWORD_TO_STRING(obj)));
      }

   public static int bgl_obj_hash_number(Object obj)
      {
	 return obj.hashCode();
      }

   public static int bgl_foreign_hash_number(Object obj)
      {
	 return obj.hashCode();
      }

   public static int bgl_pointer_hash_number(Object obj, int power)
      {
	 return obj.hashCode() % power;
      }

   public static int bgl_elong_hash_number(long n)
      {
	 return (int)n;
      }

   public static int bgl_llong_hash_number(long n)
      {
	 return ((int)n);
      }

   public static byte[] bgl_double_to_ieee_string(double v) throws IOException
      {
	 final ByteArrayOutputStream bout = new ByteArrayOutputStream();
	 final DataOutputStream out = new DataOutputStream(bout);

	 out.writeDouble(v);

	 final byte[] res = bout.toByteArray();

	 bout.close();
	 return res;
      }

   public static double bgl_ieee_string_to_double(byte[]s) throws IOException
      {
	 final ByteArrayInputStream bint = new ByteArrayInputStream(s);
	 final DataInputStream in = new DataInputStream(bint);
	 final double res = in.readDouble();

	 bint.close();

	 return res;
      }

   public static int rand()
      {
	 return (int) randg.nextInt();
      }

   public static bignum bgl_rand_bignum(bignum range)
   {
      bignum b = new bignum(new java.math.BigInteger(range.value.bitLength(), randg));
      if (b.value.compareTo(range.value) >= 0)
	 return new bignum( b.value.mod(range.value) );
      else
	 return b;
   }

   public static void srand( int seed )
      {
	 randg = new Random( seed );
      }

   public static void bgl_sleep( int microsecs ) {
      try {
	 Thread.sleep( (long)(microsecs / 1000),
		       (int)(1000*(microsecs % 1000)) );
      } catch( Exception e ) {
          if ( e instanceof InterruptedException ) {
              Thread.currentThread().interrupt();
          }
      }
   }

   public static byte[] bgl_gethostname() {
      try {
	 InetAddress addr = java.net.InetAddress.getLocalHost();
	 return addr.getHostName().getBytes();
      } catch( Exception _e ) {
	 return "localhost".getBytes();
      }
   }

   public static byte[] bgl_gethostname_by_address(byte[] ip) {
      try {
	 InetAddress addr = java.net.InetAddress.getByName( new String( ip ) );
	 return addr.getHostName().getBytes();
      } catch( Exception _e ) {
	 return "".getBytes();
      }
   }

   public static Object bgl_gethostinterfaces() {
      // to be implemented
      System.err.println( "foreign.java: bgl_hostinterfaces not implemented" );
      return BNIL;
   }

   
   //////
   // MUTEX and CONDITION VARIABLE
   //////
   public static boolean BGL_MUTEXP(Object o) {
      return (o instanceof mutex);
   }

   public static mutex bigloo_generic_mutex = bgl_make_mutex( BFALSE );
   
   public static mutex bgl_make_mutex(Object o) {
      return mutex.make(o);
   }
   
   public static mutex bgl_make_nil_mutex() {
      return mutex.nil_mutex;
   }
   
   public static Object BGL_MUTEX_NAME(mutex o)
      {
	 return o.name;
      }

   public static Object BGL_MUTEX_BACKEND(mutex o)
      {
	 return o.backend;
      }

   public static int bgl_mutex_lock(mutex o)
      {
	 return o.acquire_lock();
      }
   
   public static int bgl_mutex_lock_prelock(mutex o, Object l)
      {
	 return o.acquire_lock();
      }
   
   public static int bgl_mutex_timed_lock(mutex o, int tmt)
      {
	 return o.acquire_timed_lock(tmt);
      }
   
   public static int bgl_mutex_unlock(mutex o)
      {
	 return o.release_lock();
      }
   
   public static Object bgl_mutex_state(mutex o)
      {
	 return o.state();
      }
   
   public static boolean BGL_CONDVARP(Object o)
      {
	 return (o instanceof condvar);
      }
   
   public static condvar bgl_make_condvar(Object o) {
      return condvar.make(o);
   }
   
   public static condvar bgl_make_nil_condvar() {
      return condvar.nil_condvar;
   }
   
   public static Object BGL_CONDVAR_NAME(condvar o) {
      return o.name;
   }

   public static boolean bgl_condvar_wait(condvar c, mutex o) {
      return c.wait( o );
   }

   public static boolean bgl_condvar_timed_wait(condvar c, mutex o, int ms) {
      return c.timed_wait( o, ms );
   }

   public static boolean bgl_condvar_broadcast(condvar c) {
      return c.broadcast();
   }
   
   public static boolean bgl_condvar_signal(condvar c) {
      return c.signal();
   }

   //////
   // Semaphore
   //////
   public static boolean BGL_SEMAPHOREP(Object o) {
      return (o instanceof semaphore);
   }

   //////
   // MMAP
   //////
   public static boolean BGL_MMAPP( Object o ) {
      return (o instanceof mmap);
   }

   public static mmap bgl_open_mmap( byte[] fname, boolean r, boolean w ) {
      if( is_resourcep( fname ) ) {
	 if( w ) {
	    fail( "mmap", "Cannot mmap resource file for write", fname);
	 } else {
	    if( !r ) {
	       fail( "mmap", "resource file has been mmap for reading", fname);
	    }
	 }
	 return new mmap( resource_name( fname ) );
      } else {
	 return new mmap( fname, r, w );
      }
   }

   public static mmap bgl_string_to_mmap( byte[] s, boolean r, boolean w ) {
      return new mmaps( s, r, w );
   }

   public static byte[] bgl_mmap_to_string( mmap m ) {
      if (m instanceof mmaps) {
         return m.name;
      } else {
         return m.map.array();
      }
   }

   public static Object bgl_close_mmap( mmap o ) {
      return o.close();
   }

   public static Object bgl_sync_mmap( mmap o ) {
      o.map.force();
      return o;
   }

   public static int BGL_MMAP_REF( mmap o, long i ) {
      if( o.map == null ) {
	 return o.get( i );
      } else {
	 return o.map.get( (int)i ) & 0xff;
      }
   }
   
   public static Object BGL_MMAP_SET( mmap o, long i, int c ) {
      if( o.map == null ) {
	 o.put( i, (byte)(c & 0xff) );
      } else {
	 o.map.put( (int)i, (byte)(c & 0xff) );
      }
      return o;
   }

   public static long BGL_MMAP_LENGTH( mmap o ) {
      return o.len;
   }
      
   public static Object BGL_MMAP_NAME( mmap o ) {
      return o.name;
   }

   public static long BGL_MMAP_RP_GET( mmap o ) {
      return o.rp;
   }
   
   public static void BGL_MMAP_RP_SET( mmap o, long i ) {
      o.rp = i;
   }
   
   public static long BGL_MMAP_WP_GET( mmap o ) {
      return o.wp;
   }
   
   public static void BGL_MMAP_WP_SET( mmap o, long i ) {
      o.wp = i;
   }
}
