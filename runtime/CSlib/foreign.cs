using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;
using System.Net;
// using Mono.Math;

namespace bigloo
{
   public sealed class foreign
   {
      private static bint[] bint_allocated;

      private static Random randg = new Random( 1 );

      public static callback __cb__= null;

      //public static readonly bool running_on_ms_vm;
      //public static readonly bool running_on_pnet_vm;
      //public static readonly bool running_on_mono_vm;
      //public static bool vm_has_processes= true;

      static foreign()
	 {
	    /*
	      try
	      {
	      if (Double.Parse( "69e69" ) == 0)
	      {
	      Console.Error.WriteLine( "Running on PNet VM 0.4.8..." );
	      running_on_pnet_vm= true;
	      vm_has_processes= false;
	      }
	      else
	      {
	      try
	      {
	      if (Environment.Version.Major == 1)
	      {
              Console.Error.WriteLine( "Running on Microsoft VM {0}.{1}.{2}.{3} ?",
	      Environment.Version.Major,
	      Environment.Version.Minor,
	      Environment.Version.Build,
	      Environment.Version.Revision );
              running_on_ms_vm= true;
	      }
	      else if (   (Environment.Version.Major == 0)
	      && (Environment.Version.Minor == 5))
	      {
              Console.Error.WriteLine( "Running on PNet VM {0}.{1}.{2} ?",
	      Environment.Version.Major,
	      Environment.Version.Minor,
	      Environment.Version.Build );
              running_on_pnet_vm= true;
              if (   (Environment.Version.Major == 0)
	      && (Environment.Version.Major <= 5)
	      && (Environment.Version.Build <= 6))
	      vm_has_processes= false;
	      }
	      else
	      {
              Console.Error.WriteLine( "Running on Mono VM ?" );
              running_on_mono_vm= true;
	      }
	      }
	      catch (Exception)
	      {
	      Console.Error.WriteLine( "Running on PNet VM {0}.{1}.{2}",
	      Environment.Version.Major,
	      Environment.Version.Minor,
	      Environment.Version.Build );
	      running_on_pnet_vm= true;
	      vm_has_processes= false;
	      }
	      }
	      }
	      catch( Exception )
	      {
	      Console.Error.WriteLine( "Running on PNet VM 0.5.0..." );
	      running_on_pnet_vm= true;
	      }
	    */

	    bint_allocated = new bint[2148];
	    for ( int i = -100 ; i < 2048 ; ++i)
	       bint_allocated[i+100] = new bint( i );
	    if (__cb__ != null)
	       __cb__ = new callback();
	 }

      public static byte[] getbytes( String s )
	 {
	    int len = s.Length;
	    byte[] r = new byte[len];

	    for ( int i= 0 ; i < len ; ++i )
	       r[i]= (byte)s[i];
	    return r;
	 }

      public static byte[] bigloo_backend() {
	 return getbytes( "bigloo-dotnet" );
      }
   
      public static String newstring( Object  o )
	 {
	    if (o is byte[])
	       return newstring( (byte[])o );
	    else
	       return o.ToString();
	 }

      public static String newstring( byte[]  v )
	 {
	    int len = v.Length;
	    char[] chars = new char[len];

	    for ( int i = 0 ; i < len ; ++i )
	       chars[i] = (char)v[i];

	    return new String( chars );
	 }

      public static String newstring( byte[] v, int offset, int length )
	 {
	    char[] chars = new char[length];

	    for ( int i= 0 ; i < length ; ++i )
	       chars[i]= (char)v[offset+i];

	    return new String( chars );
	 }

      // CARE no foreign functions may throws something.. CATCH it

      public static void trace_exit()
	 {
	 }

      public static void dump_stack()
	 {
	    Exception o = new Exception( "jop" );

	    Console.WriteLine( o.StackTrace );
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
      public static void printint( int n )
	 {
	    Console.Out.WriteLine( "PI "+n );
	 }

      public static void print( String  msg )
	 {
	    Console.Out.WriteLine( msg );
	 }

      public static void oprint( Object  msg )
	 {
	    Console.Out.WriteLine( "OPRINT = " + msg );
	 }

      public static void Error( String  msg )
	 {
	    Console.Error.WriteLine( msg );
	    Environment.Exit( -1 );
	 }

      public static byte[] FOREIGN_TYPE_NAME( Object  o )
	 {
	    return foreign.getbytes( o.GetType().ToString() );
	 }

      public static Object listargv( String[]  argv )
	 {
	    int len = argv.Length;
	    Object result = BNIL;

	    for ( int i = 0 ; i < len ; ++i )
	       result = new pair( getbytes( argv[len-i-1] ), result );
	    result = new pair( executable_name, result );
	    command_line = result;

	    return result;
	 }

      public static int parseint( byte[] buf, int pos, int bout, int radix )
	 {
	    int result = 0;
	    bool neg = false;
	    byte cn;

	    if (buf[pos] == (byte) '-')
	    {
	       ++pos;
	       neg= true;
	    }
	    if (buf[pos] == (byte) '+')
	    {
	       ++pos;
	       neg= false;
	    }

	    while (pos < bout)
	    {
	       cn= buf[pos++];
	       if  (((byte)'0' <= cn) && (cn <= (byte)'9'))
		  result= result * radix + (cn - (byte) '0');
	       else if (cn <= (byte)'Z')
		  result= result * radix + 10 + (cn - (byte) 'A');
	       else
		  result= result * radix + 10 + (cn - (byte) 'a');
	    }

	    return (neg ? -result : result);
	 }

      public static long parselong( byte[] buf, int pos, int bout, int radix )
	 {
	    long result = 0;
	    bool neg = false;
	    byte cn;

	    if (buf[pos] == (byte) '-')
	    {
	       ++pos;
	       neg= true;
	    }
	    if (buf[pos] == (byte) '+')
	    {
	       ++pos;
	       neg= false;
	    }

	    while (pos < bout)
	    {
	       cn= buf[pos++];
	       if  (((byte)'0' <= cn) && (cn <= (byte)'9'))
		  result= result * radix + (cn - (byte) '0');
	       else if (cn <= (byte)'Z')
		  result= result * radix + 10 + (cn - (byte) 'A');
	       else
		  result= result * radix + 10 + (cn - (byte) 'a');
	    }

	    return (neg ? -result : result);
	 }

      private static int charDigit2Val(byte cn)
	 {
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
	    bool neg = false;
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

	       if( result > MAX_VALUE_FX / radix - cn ) {
		  long lresult = result; 
		  while (pos < bout)
		  {
		     cn = charDigit2Val( buf[pos++] );
		     if ( lresult > MAX_VALUE_ELONG / radix - cn ) {
			byte[] sbuf = new byte[ bout - initPos ];
		        return bgl_string_to_bignum( newstring(buf, initPos, bout - initPos), radix );
		     }

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

      /////
      // BOOLEAN
      /////
      // Constants
      public static readonly bbool BTRUE= bbool.vrai;
      public static readonly bbool BFALSE= bbool.faux;

      // Predicates
      public static bool BOOLEANP ( Object  o )
	 {
	    return ((o == bbool.faux) || (o == bbool.vrai));
	 }

      // Conversions
      public static bool CBOOL( bbool  b )
	 {
	    return (b != bbool.faux);
	 }

      public static bool CBOOL( Object  b )
	 {
	    return (b != bbool.faux);
	 }

      public static bbool BBOOL( bool  b )
	 {
	    return (b ? bbool.vrai : bbool.faux);
	 }

      // Lib functions
      public static bool EQ( Object  o1,
			     Object  o2 )
	 {
	    return (   (o1 == o2)
		       || (   (o1 is bint)
			      && (o2 is bint)
			      && (((bint)o1).value == ((bint)o2).value)));
	 }

      public static bool BOXED_EQ( Object  o1,
				   Object  o2 )
	 {
	    return (o1 == o2);
	 }

      //////
      // CHARACTER
      //////
      // Predicates
      public static bool CHARP( Object  o )
	 {
	    return (o is bchar);
	 }

      // Conversions
      public static bchar BCHAR( int  cn )
	 {
	    return bchar.allocated[cn & 0xFF];
	 }

      public static bchar BCHAR( byte  c )
	 {
	    return bchar.allocated[c & 0xFF];
	 }

      public static byte CCHAR( bchar  c ) 
	 {
	    return c.value;
	 }

      public static int BCHAR_TO_UCHAR( bchar c )
	 {
	    return c.value;
	 }

      public static byte BCHAR_TO_UBYTE( bchar c )
	 {
	    return c.value;
	 }

      public static int CHAR_TO_UCHAR( byte  c )
	 {
	    return (c & 0xFF);
	 }

      public static byte UCHAR_TO_CHAR( int  c )
	 {
	    return (byte)c;
	 }

      // Open functions
      public static bool CHAR_EQ( int  cn1,
				  int  cn2 )
	 {
	    return (cn1 == cn2);
	 }

      public static bool CHAR_LT( int  cn1,
				  int  cn2 )
	 {
	    return ((cn1 & 0xFF) < (cn2 & 0xFF));
	 }

      public static bool CHAR_GT( int  cn1,
				  int  cn2 )
	 {
	    return ((cn1 & 0xFF) > (cn2 & 0xFF));
	 }

      public static bool CHAR_LE( int  cn1,
				  int  cn2 )
	 {
	    return ((cn1 & 0xFF) <= (cn2 & 0xFF));
	 }

      public static bool CHAR_GE( int  cn1,
				  int  cn2 )
	 {
	    return ((cn1 & 0xFF) >= (cn2 & 0xFF));
	 }

      public static int CHAR_OR( int  cn1,
				 int  cn2 )
	 {
	    return (cn1 | cn2);
	 }

      public static int CHAR_AND( int  cn1,
				  int  cn2 )
	 {
	    return (cn1 & cn2);
	 }

      public static int CHAR_NOT( int  cn )
	 {
	    return ~cn;
	 }

      // Lib functions
      public static int toupper( int  cn )
	 {
	    if ((97 <= cn) && (cn <= 122))
	       return (cn-32);
	    return cn;
	 }

      public static int tolower( int  cn )
	 {
	    if ((65 <= cn) && (cn <= 90))
	       return (cn+32);
	    return cn;
	 }

      //////
      // INTEGER
      //////
      // Constants
      public static int SIZEOFLONG= 4;   // !!!!! devrait etre const => maj bytecode

      public static readonly int MIN_VALUE_FX = int.MinValue;
      public static readonly int MAX_VALUE_FX = int.MaxValue;
      public static readonly long MIN_VALUE_ELONG = long.MinValue;
      public static readonly long MAX_VALUE_ELONG = long.MaxValue;

      

      // Predicates
      public static bool INTEGERP( Object  o )
	 {
	    return (o is bint);
	 }

      public static bool ELONGP( Object  o )
	 {
	    return (o is belong);
	 }

      public static bool LLONGP( Object  o )
	 {
	    return (o is bllong);
	 }

      public static bool BIGNUMP( Object  o )
	 {
	    return (o is bignum);
	 }

      // Conversions
      public static int CHAR_TO_INT( int  cn )
	 {
	    return (cn & 0xFF);
	 }

      public static int INT_TO_CHAR( int  n )
	 {
	    return n;
	 }

      public static short INT_TO_SHORT(int n)
	 {
	    return (short)n;
	 }

      public static byte BYTE_TO_UBYTE(sbyte n)
	 {
	    return (byte)n;
	 }
      
      public static int BYTE_TO_LONG(sbyte n)
	 {
	    return n;
	 }
      
      public static int BYTE_TO_INT(sbyte n)
	 {
	    return n;
	 }
      public static int UBYTE_TO_INT(byte n)
	 {
	    return n;
	 }
      
      public static uint BYTE_TO_ULONG(sbyte n)
	 {
	    return (uint)n;
	 }
      
      public static int UBYTE_TO_LONG(byte n)
	 {
	    return n;
	 }
      
      public static uint UBYTE_TO_ULONG(byte n)
	 {
	    return (uint)n;
	 }
      
      public static int SHORT_TO_INT(short n)
	 {
	    return n;
	 }

      public static int SHORT_TO_LONG(short n)
	 {
	    return n;
	 }
      public static int USHORT_TO_LONG(ushort n)
	 {
	    return n;
	 }

      public static sbyte INT_TO_BYTE( int n )
	 {
	    return (sbyte)n;
	 }
      public static byte INT_TO_UBYTE( int n )
	 {
	    return (byte)n;
	 }
      public static sbyte BINT_TO_BYTE( bint n )
	 {
	    return (sbyte)n.value;
	 }
      public static byte BINT_TO_UBYTE( bint n )
	 {
	    return (byte)n.value;
	 }
      public static short BINT_TO_SHORT( bint  n )
	 {
	    return (short)n.value;
	 }
      public static ushort BINT_TO_USHORT( bint  n )
	 {
	    return (ushort)n.value;
	 }
      public static uint BINT_TO_ULONG( bint n )
	 {
	    return (uint)(n.value);
	 }

      public static short LONG_TO_SHORT(int n)
	 {
	    return (short)n;
	 }
      public static ushort LONG_TO_USHORT(int n)
	 {
	    return (ushort)n;
	 }

      public static int LONG_TO_INT( int n )
	 {
	    return n;
	 }
      public static uint LONG_TO_ULONG( int n )
	 {
	    return (uint)n;
	 }

      public static sbyte LONG_TO_BYTE( int n )
	 {
	    return (sbyte)n;
	 }
      public static byte LONG_TO_UBYTE( int n )
	 {
	    return (byte)n;
	 }

      public static sbyte ULONG_TO_BYTE( uint n )
	 {
	    return (sbyte)n;
	 }
      public static byte ULONG_TO_UBYTE( uint n )
	 {
	    return (byte)n;
	 }
      public static int ULONG_TO_INT( uint n )
	 {
	    return (int)n;
	 }
      public static int ULONG_TO_LONG( uint n )
	 {
	    return (int)n;
	 }

      public static bllong LLONG_TO_BLLONG( long  n )
	 {
	    return new bllong( n );
	 }

      public static bllong ULLONG_TO_BLLONG( ulong  n )
	 {
	    return new bllong( (long)n );
	 } 

      public static long LONG_TO_LLONG( int  n )
	 {
	    return (long)n;
	 }

      public static long ELONG_TO_LLONG( long  n )
	 {
	    return (long)n;
	 }

      public static ulong LONG_TO_ULLONG( int  n )
	 {
	    return (ulong)n;
	 }

      public static int LLONG_TO_LONG( long  n )
	 {
	    return (int)n;
	 }

      public static int ULLONG_TO_LONG( ulong  n )
	 {
	    return (int)n;
	 }

      public static long BLLONG_TO_LLONG( bllong  n )
	 {
	    return n.value;
	 }

      public static ulong BLLONG_TO_ULLONG( bllong  n )
	 {
	    return (ulong)n.value;
	 }

      public static long BLLONG_TO_LONG( bllong  n )
	 {
	    return (long)n.value;
	 }

      public static belong LONG_TO_BELONG( int  n )
	 {
	    return new belong( n );
	 }

      public static long LONG_TO_ELONG( int  n )
	 {
	    return n;
	 }

      public static int ELONG_TO_LONG( long  n )
	 {
	    return (int)n;
	 }
 
      public static long LLONG_TO_ELONG(long n)
        {
	   return n;
	}

      public static long UELONG_TO_ELONG(ulong n)
	 {
	    return (long)n;
	 }

      public static ulong ELONG_TO_UELONG(long n)
	 {
	    return (ulong)n;
	 }

      public static long BELONG_TO_LONG( belong  n )
	 {
	    return n.value;
	 }
   
      public static long BELONG_TO_ELONG( belong  n )
	 {
	    return n.value;
	 }

      public static belong ELONG_TO_BELONG( long  n )
	 {
	    return new belong( n );
	 }

      public static bllong LONG_TO_BLLONG( int  n )
	 {
	    return new bllong( n );
	 }

      public static long ULLONG_TO_LLONG(ulong n)
	 {
	    return (long)n;
	 }

      public static ulong LLONG_TO_ULLONG(long n)
	 {
	    return (ulong)n;
	 }

      public static bignum LONG_TO_BIGNUM( int  n )
	 {
	    return new bignum( n );
	 }

      public static int bgl_bignum_to_long( bignum n )
	 {
	    return n.value.IntValue();
	 }

      public static bignum ELONG_TO_BIGNUM( long  n )
	 {
	    return new bignum( n );
	 }

      public static bignum LLONG_TO_BIGNUM( long  n )
	 {
	    return new bignum( n );
	 }

      public static bignum FLONUM_TO_BIGNUM( double  n )
	 {
	    return new bignum( n );
	 }

      public static double BIGNUM_TO_FLONUM( bignum  n )
	 {
	    return n.DoubleValue();
	 }

      public static bint BINT( long  v )
	 {
	    if ((-100 <= v) && (v < 2018))
	       return bint_allocated[(int)v+100];
	    return new bint( (int)v );
	 }
    
      public static bint BINT( int  v )
	 {
	    if ((-100 <= v) && (v < 2018))
	       return bint_allocated[v+100];
	    return new bint( v );
	 }
      public static bint BINT(sbyte v)
	 {
	    return BINT( (int)v );
	 }
      public static bint BINT(byte v)
	 {
	    return BINT( (int)v );
	 }
      public static bint BINT(uint v)
	 {
	    return BINT( (int)v );
	 }
      public static bint BINT(short v)
	 {
	    return BINT( (int)v );
	 }
      public static bint BINT(ushort v)
	 {
	    return BINT( (int)v );
	 }


      public static int CINT( bint  v )
	 {
	    return v.value;
	 }

      public static byte[] bgl_bignum_to_string(bignum n, int r)
      {
	 return getbytes(n.value.ToString(r));
      }

      public static bignum bgl_string_to_bignum(byte[] s, int r)
      {
	 return new bignum(new BigInteger(newstring(s), r));
      }

      public static bignum bgl_string_to_bignum(String s)
      {
	 return new bignum(new BigInteger(s, 10));
      }

      public static bignum bgl_string_to_bignum(String s, int r)
      {
	 return new bignum(new BigInteger(s, r));
      }

      public static Object bgl_string_to_integer_obj(byte[] s, int r)
      {
	 String str = newstring(s);
	 Object res;

	 try
	 {
	    res = BINT(Convert.ToInt32(str, r));
	 }
	 catch (OverflowException e)
	 {
	    res = new bignum(new BigInteger(str, r));
	 }
	 
	 return res;
      }

      // Open functions
      public static bool EQ_FX( int  n1,
				int  n2 )
	 {
	    return (n1 == n2);
	 }

      public static bool EQ_ELONG( long  n1,
				   long  n2 )
	 {
	    return (n1 == n2);
	 }

      public static bool EQ_LLONG( long  n1,
				   long  n2 )
	 {
	    return (n1 == n2);
	 }

      public static bool EQ_BIGNUM( bignum  n1,
				    bignum  n2 )
	 {
	    return (n1.value == n2.value);
	 }

      public static bool LT_FX( int  n1,
				int  n2 )
	 {
	    return (n1 < n2);
	 }

      public static bool LT_ELONG( long  n1,
				   long  n2 )
	 {
	    return (n1 < n2);
	 }

      public static bool LT_LLONG( long  n1,
				   long  n2 )
	 {
	    return (n1 < n2);
	 }

      public static bool LT_BIGNUM( bignum  n1,
				    bignum  n2 )
	 {
	    return (n1.value < n2.value);
	 }

      public static bool LE_FX( int  n1,
				int  n2 )
	 {
	    return (n1 <= n2);
	 }

      public static bool LE_ELONG( long  n1,
				   long  n2 )
	 {
	    return (n1 <= n2);
	 }

      public static bool LE_LLONG( long  n1,
				   long  n2 )
	 {
	    return (n1 <= n2);
	 }

      public static bool LE_BIGNUM( bignum  n1,
				    bignum  n2 )
	 {
	    return (n1.value <= n2.value);
	 }

      public static bool GT_FX( int  n1,
				int  n2 )
	 {
	    return (n1 > n2);
	 }

      public static bool GT_ELONG( long  n1,
				   long  n2 )
	 {
	    return (n1 > n2);
	 }

      public static bool GT_LLONG( long  n1,
				   long  n2 )
	 {
	    return (n1 > n2);
	 }

      public static bool GT_BIGNUM( bignum  n1,
				    bignum  n2 )
	 {
	    return (n1.value > n2.value);
	 }

      public static bool GE_FX( int  n1,
				int  n2 )
	 {
	    return (n1 >= n2);
	 }

      public static bool GE_ELONG( long  n1,
				   long  n2 )
	 {
	    return (n1 >= n2);
	 }

      public static bool GE_LLONG( long  n1,
				   long  n2 )
	 {
	    return (n1 >= n2);
	 }

      public static bool GE_BIGNUM( bignum  n1,
				    bignum  n2 )
	 {
	    return (n1.value >= n2.value);
	 }

      public static int CMP_BIGNUM( bignum  n1, bignum  n2 )
	 {
	    return (n1.value < n2.value) ? -1 : (n1.value == n2.value) ? 0 : 1;
	 }

      public static bool ZEROP_BIGNUM( bignum n )
	 {
	    return n.value == 0;
	 }

      public static bool POSITIVEP_BIGNUM( bignum n )
	 {
	    return n.value > 0;
	 }

      public static bool NEGATIVEP_BIGNUM( bignum n )
	 {
	    return n.value < 0;
	 }

      public static bool EVENP_FX( int  n )
	 {
	    return ((n & 1) == 0);
	 }

      public static bool EVENP_BIGNUM( bignum  n )
	 {
	    return ((n.value & 1) == 0);
	 }

      public static bool ODDP_FX( int  n )
	 {
	    return ((n & 1) != 0);
	 }

      public static bool ODDP_BIGNUM( bignum  n )
	 {
	    return ((n.value & 1) != 0);
	 }

      public static bignum ABS_BIGNUM( bignum  n )
	 {
	    return new bignum(n.value.abs());
	 }

      public static int PLUS_FX( int  n1,
				 int  n2 )
	 {
	    return (n1 + n2);
	 }

      public static long PLUS_ELONG( long  n1,
				     long  n2 )
	 {
	    return (n1 + n2);
	 }

      public static long PLUS_LLONG( long  n1,
				     long  n2 )
	 {
	    return (n1 + n2);
	 }

      public static bignum PLUS_BIGNUM( bignum  n1,
					bignum  n2 )
	 {
	    return new bignum(n1.value + n2.value);
	 }

      public static Object BGL_SAFE_BX_TO_FX( Object n )
	 {
	    bignum o = (bignum)n;
	    if( o.value.bitCount() < 32 ) {
	       return BINT( o.value.IntValue() );
	    } else {
	       return n;
	    }
	 }
	       
      public static Object SAFE_PLUS_FX( int n1, int n2 )
      {
	 try
	 {
	    return BINT(checked(n1 + n2));
	 }
	 catch (OverflowException e)
	 {
	    return PLUS_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static Object SAFE_PLUS_ELONG( long n1, long n2 )
      {
	 try
	 {
	    return new belong(checked(n1 + n2));
	 }
	 catch (OverflowException e)
	 {
	    return PLUS_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static Object SAFE_PLUS_LLONG( long n1, long n2 )
      {
	 try
	 {
	    return new bllong(checked(n1 + n2));
	 }
	 catch (OverflowException e)
	 {
	    return PLUS_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static int MINUS_FX( int  n1,
				  int  n2 )
	 {
	    return (n1 - n2);
	 }

      public static long MINUS_ELONG( long  n1,
				      long  n2 )
	 {
	    return (n1 - n2);
	 }

      public static long MINUS_LLONG( long  n1,
				      long  n2 )
	 {
	    return (n1 - n2);
	 }

      public static bignum MINUS_BIGNUM( bignum  n1,
					 bignum  n2 )
	 {
	    return new bignum(n1.value - n2.value);
	 }

      public static Object SAFE_MINUS_FX( int n1, int n2 )
      {
	 try
	 {
	    return BINT(checked(n1 - n2));
	 }
	 catch (OverflowException e)
	 {
	    return MINUS_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static Object SAFE_MINUS_ELONG( long n1, long n2 )
      {
	 try
	 {
	    return new belong(checked(n1 - n2));
	 }
	 catch (OverflowException e)
	 {
	    return MINUS_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static Object SAFE_MINUS_LLONG( long n1, long n2 )
      {
	 try
	 {
	    return new bllong(checked(n1 - n2));
	 }
	 catch (OverflowException e)
	 {
	    return MINUS_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static int MUL_FX( int  n1,
				int  n2 )
	 {
	    return (n1 * n2);
	 }

      public static long MUL_ELONG( long  n1,
				    long  n2 )
	 {
	    return (n1 * n2);
	 }

      public static long MUL_LLONG( long  n1,
				    long  n2 )
	 {
	    return (n1 * n2);
	 }

      public static bignum MUL_BIGNUM( bignum  n1,
				       bignum  n2 )
	 {
	    return new bignum(n1.value * n2.value);
	 }

      public static Object SAFE_MUL_FX( int n1, int n2 )
      {
	 try
	 {
	    return BINT(checked(n1 * n2));
	 }
	 catch (OverflowException e)
	 {
	    return MUL_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static Object SAFE_MUL_ELONG( long n1, long n2 )
      {
	 try
	 {
	    return new belong(checked(n1 * n2));
	 }
	 catch (OverflowException e)
	 {
	    return MUL_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static Object SAFE_MUL_LLONG( long n1, long n2 )
      {
	 try
	 {
	    return new bllong(checked(n1 * n2));
	 }
	 catch (OverflowException e)
	 {
	    return MUL_BIGNUM(new bignum(n1), new bignum(n2));
	 }
      }

      public static int DIV_FX( int  n1,
				int  n2 )
	 {
	    return (n1 / n2);
	 }

      public static long DIV_ELONG( long  n1,
				    long  n2 )
	 {
	    return (n1 / n2);
	 }

      public static long DIV_LLONG( long  n1,
				    long  n2 )
	 {
	    return (n1 / n2);
	 }

      public static Object SAFE_DIV_FX( int n1, int n2 )
      {
	 if (n1 == int.MinValue && n2 == -1)
	 {
	    return new bignum(- new BigInteger(n1));
	 }
	 else
	 {
	    return BINT(n1 / n2);
	 }
      }

      public static Object SAFE_DIV_ELONG( long n1, long n2 )
      {
	 if (n1 == long.MinValue && n2 == -1)
	 {
	    return new bignum(- new BigInteger(n1));
	 }
	 else
	 {
	    return new belong(n1 / n2);
	 }
      }

      public static Object SAFE_DIV_LLONG( long n1, long n2 )
      {
	 if (n1 == long.MinValue && n2 == -1)
	 {
	    return new bignum(- new BigInteger(n1));
	 }
	 else
	 {
	    return new bllong(n1 / n2);
	 }
      }

      public static int NEG_FX( int  n )
	 {
	    return -n;
	 }

      public static long NEG_ELONG( long  n )
	 {
	    return -n;
	 }

      public static long NEG_LLONG( long  n )
	 {
	    return -n;
	 }

      public static bignum NEG_BIGNUM( bignum  n )
	 {
	    return new bignum(-n.value);
	 }

      public static int QUOTIENT_FX( int  n1,
				     int  n2 )
	 {
	    return (n1 / n2);
	 }

      public static long QUOTIENT_ELONG( long  n1,
					 long  n2 )
	 {
	    return (n1 / n2);
	 }

      public static long QUOTIENT_LLONG( long  n1,
					 long  n2 )
	 {
	    return (n1 / n2);
	 }

      public static bignum QUOTIENT_BIGNUM( bignum  n1,
					    bignum  n2 )
	 {
	    return new bignum(n1.value / n2.value);
	 }

      public static int REMAINDER_FX( int  n1,
				      int  n2 )
	 {
	    return (n1 % n2);
	 }

      public static long REMAINDER_ELONG( long  n1,
					  long  n2 )
	 {
	    return (n1 % n2);
	 }

      public static long REMAINDER_LLONG( long  n1,
					  long  n2 )
	 {
	    return (n1 % n2);
	 }

      public static bignum REMAINDER_BIGNUM( bignum  n1,
					     bignum  n2 )
	 {
	    return new bignum(n1.value % n2.value);
	 }

      public static bignum DIVREM_BIGNUM( bignum  n1,
					bignum  n2 )
	 {
	    bgldynamic env = BGL_CURRENT_DYNAMIC_ENV();

	    env.mvalues_number = 2;
	    env.mvalues_values[ 1 ] = REMAINDER_BIGNUM(n1, n2);

	    return QUOTIENT_BIGNUM(n1, n2);
	 }

      public static bignum GCD_BIGNUM( bignum  n1,
				       bignum  n2 )
	 {
	    return new bignum(n1.value.gcd(n2.value));
	 }

      public static bignum LCM_BIGNUM( bignum  n1,
				       bignum  n2 )
	 {
	    return new bignum(((n1.value / n1.value.gcd(n2.value)) * n2.value).abs());
	 }

      public static int BITOR( int  a1,
			       int  a2 )
	 {
	    return (a1 | a2);
	 }

      public static long BITORELONG( long  a1,
				     long  a2 )
	 {
	    return (a1 | a2);
	 }

      public static long BITORLLONG( long  a1,
				     long  a2 )
	 {
	    return (a1 | a2);
	 }

      public static int BITAND( int  a1,
				int  a2 )
	 {
	    return (a1 & a2);
	 }
    
      public static long BITANDELONG( long  a1,
				      long  a2 )
	 {
	    return (a1 & a2);
	 }
    
      public static long BITANDLLONG( long  a1,
				      long  a2 )
	 {
	    return (a1 & a2);
	 }
    
      public static int BITXOR( int  a1,
				int  a2 )
	 {
	    return (a1 ^ a2);
	 }

      public static long BITXORELONG( long  a1,
				      long  a2 )
	 {
	    return (a1 ^ a2);
	 }

      public static long BITXORLLONG( long  a1,
				      long  a2 )
	 {
	    return (a1 ^ a2);
	 }

      public static int BITNOT( int  a )
	 {
	    return ~a;
	 }

      public static long BITNOTELONG( long  a )
	 {
	    return ~a;
	 }

      public static long BITNOTLLONG( long  a )
	 {
	    return ~a;
	 }

      public static int BITRSH( int  a1, int  a2 )
	 {
	    return (a1 >> a2);
	 }

      public static uint BITURSH( uint a1, int  a2 )
	 {
	    return (a1 >> a2);
	 }

      public static long BITRSHELONG( long  a1, int  a2 )
	 {
	    return (a1 >> a2);
	 }

      public static ulong BITURSHELONG( ulong  a1, int  a2 )
	 {
	    return (a1 >> a2);
	 }

      public static long BITRSHLLONG( long  a1, int  a2 )
	 {
	    return (a1 >> a2);
	 }

      public static ulong BITURSHLLONG( ulong  a1, int  a2 )
	 {
	    return (a1 >> a2);
	 }

      public static int BITLSH( int  a1, int  a2 )
	 {
	    return (a1 << a2);
	 }

      public static long BITLSHELONG( long  a1, int  a2 )
	 {
	    return (a1 << a2);
	 }

      public static long BITLSHLLONG( long  a1, int  a2 )
	 {
	    return (a1 << a2);
	 }

      //////
      // FLOAT
      //////
      public static readonly double BGL_NAN = Double.NaN;
      public static readonly double BGL_INFINITY = Double.PositiveInfinity;

      // Predicates
      public static bool REALP( Object  o )
	 {
	    return (o is real);
	 }

      // Conversions
      public static double REAL_TO_DOUBLE( real  n )
	 {
	    return n.value;
	 }

      public static float REAL_TO_FLOAT( real  n )
	 {
	    return (float)n.value;
	 }

      public static real DOUBLE_TO_REAL( double  n )
	 {
	    return new real( n );
	 }

      public static real FLOAT_TO_REAL( float  n )
	 {
	    return new real( (double)n );
	 }

      public static float DOUBLE_TO_FLOAT( double  n )
	 {
	    return (float)n;
	 }

      public static double FLOAT_TO_DOUBLE( float  n )
	 {
	    return (double)n;
	 }

      public static double FIXNUM_TO_FLONUM( int  n )
	 {
	    return (double)n;
	 }

      public static int FLONUM_TO_FIXNUM( double  n )
	 {
	    return (int)n;
	 }

      public static double ELONG_TO_FLONUM( long  n )
	 {
	    return (double)n;
	 }

      public static long FLONUM_TO_ELONG( double  n )
	 {
	    return (long)n;
	 }

      public static double LLONG_TO_FLONUM( long  n )
	 {
	    return (double)n;
	 }

      public static long FLONUM_TO_LLONG( double  n )
	 {
	    return (long)n;
	 }

      public static long DOUBLE_TO_LLONG_BITS( double n )
	 {
	    return BitConverter.DoubleToInt64Bits(n);
	 }
      public static double LLONG_BITS_TO_DOUBLE ( long n )
	 {
	    return BitConverter.Int64BitsToDouble(n);
	 }

      // CARE [flo]: I'm not sure if GetBytes is endian-agnostic.
      public static int FLOAT_TO_INT_BITS( float n )
	 {
	    return Convert.ToInt32(BitConverter.GetBytes(n));
	 }
      public static float INT_BITS_TO_FLOAT( int n )
	 {
	    return Convert.ToSingle(BitConverter.GetBytes(n));
	 }

      // Open functions
      public static bool EQ_FL( double  n1,
				double  n2 )
	 {
	    return (n1 == n2);
	 }

      public static bool LT_FL( double  n1,
				double  n2 )
	 {
	    return (n1 < n2);
	 }

      public static bool LE_FL( double  n1,
				double  n2 )
	 {
	    return (n1 <= n2);
	 }

      public static bool GT_FL( double  n1,
				double  n2 )
	 {
	    return (n1 > n2);
	 }

      public static bool GE_FL( double  n1,
				double  n2 )
	 {
	    return (n1 >= n2);
	 }

      public static double PLUS_FL( double  n1,
				    double  n2 )
	 {
	    return (n1 + n2);
	 }

      public static double MINUS_FL( double  n1,
				     double  n2 )
	 {
	    return (n1 - n2);
	 }

      public static double MUL_FL( double  n1,
				   double  n2 )
	 {
	    return (n1 * n2);
	 }

      public static double DIV_FL( double  n1,
				   double  n2 )
	 {
	    return (n1 / n2);
	 }

      public static double NEG_FL( double  n )
	 {
	    return -n;
	 }

      public static double fmod( double n1,
				 double n2 )
	 {
	    return (n1 % n2);
	 }
   
      public static double floor( double  n )
	 {
	    return Math.Floor( n );
	 }

      public static double ceil( double  n )
	 {
	    return Math.Ceiling( n );
	 }

      public static double exp( double  n )
	 {
	    return Math.Exp( n );
	 }

      public static double log( double  n )
	 {
	    return Math.Log( n );
	 }

      public static double sin( double  n )
	 {
	    return Math.Sin( n );
	 }

      public static double cos( double  n )
	 {
	    return Math.Cos( n );
	 }

      public static double tan( double  n )
	 {
	    return Math.Tan( n );
	 }

      public static double asin( double  n )
	 {
	    return Math.Asin( n );
	 }

      public static double acos( double  n )
	 {
	    return Math.Acos( n );
	 }

      public static double atan( double  n )
	 {
	    return Math.Atan( n );
	 }

      public static double atan2( double  n1,
				  double  n2 )
	 {
	    return Math.Atan2( n1, n2 );
	 }

      public static double sqrt( double  n )
	 {
	    return Math.Sqrt( n );
	 }

      public static double pow( double  n1,
				double  n2 )
	 {
	    return Math.Pow( n1, n2 );
	 }

      public static double abs( double n )
	 {
	    return Math.Abs( n );
	 }

      public static double max( double  n1,
				double  n2 )
	 {
	    return Math.Max( n1, n2 );
	 }

      public static double min( double  n1,
				double  n2 )
	 {
	    return Math.Min( n1, n2 );
	 }

      public static int BGL_SIGNBIT( double  n )
	 {
	    return (int) (BitConverter.DoubleToInt64Bits( n ) >> 63);
	 }

      public static double round( double n )
	 {
	    return Math.Round( n );
	 }

      public static bool isfinite( double n )
	 {
	    return !( Double.IsInfinity( n ) || Double.IsNaN( n ) );
	 }

      public static bool isinf( double n )
	 {
	    return Double.IsInfinity( n );
	 }

      public static bool isnan( double n )
	 {
	    return Double.IsNaN( n );
	 }

      //////
      // OBJECT CONSTANTS
      //////
      public static readonly Object BUNSPEC= unspecified._unspecified;
      public static readonly Object BNIL= nil._nil;
      public static readonly Object BEOA= BNIL;
      public static readonly Object BEOF= eof._eof;
      public static readonly Object BOPTIONAL= optional._optional;
      public static readonly Object BREST= rest._rest;
      public static readonly Object BKEY= key._key;
      public static readonly int BDB_LIBRARY_MAGIC_NUMBER= 0x1024;     // !!!!! mettre en const => maj bytecode

      public static bool EOF_OBJECTP( Object  o )
	 {
	    return (o == BEOF);
	 }

      public static bool NULLP( Object  o )
	 {
	    return (o == BNIL);
	 }

      public static int CCNST( cnst  o )
	 {
	    return o.value;
	 }

      public static int CCNST( Object  o )
	 {
	    // CARE !! J'arrive pas a changer ce !@?# type dans intext et symbol
	    return ((cnst)o).value;
	 }

      public static Object BCNST( int  v )
	 {
	    return new cnst( v );
	 }

      public static bool CNSTP( Object  o )
	 {
	    return ((o is bchar) || (o is cnst));
	 }

      //////
      // STANGE TYPES (no constructors invoked)

      public static bool POINTERP( Object  o )
	 {
	    return true;
	 }

      public static bool OPAQUEP( Object  o )
	 {
	    return false;
	 }

      public static Object BGL_OPAQUE_NIL() 
	 {
	    return null;
	 }

      public static bool BGL_OBJECTP( Object  o )
	 {
	    return (o is bobject);
	 }

      //////
      // Unicode characters
      //////
      public static bool UCS2P( Object  o )
	 {
	    return (o is bucs2);
	 }

      public static bucs2 BUCS2( char  c )
	 {
	    return new bucs2( c );
	 }

      public static char CUCS2( bucs2  c )
	 {
	    return c.value;
	 }

      public static int CUCS2( char  c )
	 {
	    return (c & 0xFFFF);
	 }

      public static char INT_TO_UCS2( int  n )
	 {
	    return (char)n;
	 }

      public static bool UCS2_EQ( char  c1,
				  char  c2 )
	 {
	    return (c1 == c2);
	 }

      public static bool UCS2_GT( char  c1,
				  char  c2 )
	 {
	    return (c1 > c2);
	 }

      public static bool UCS2_LT( char  c1,
				  char  c2 )
	 {
	    return (c1 < c2);
	 }

      public static bool UCS2_GE( char  c1,
				  char  c2 )
	 {
	    return (c1 >= c2);
	 }

      public static bool UCS2_LE( char  c1,
				  char  c2 )
	 {
	    return (c1 <= c2);
	 }

      public static bool ucs2_letterp( char  c )
	 {
	    return Char.IsLetter( c );
	 }

      public static bool ucs2_digitp( char  c )
	 {
	    return Char.IsDigit( c );
	 }

      public static bool ucs2_whitespacep( char  c )
	 {
	    return Char.IsWhiteSpace( c );
	 }

      public static bool ucs2_lowerp( char  c )
	 {
	    return Char.IsLower( c );
	 }

      public static bool ucs2_upperp( char  c )
	 {
	    return Char.IsUpper( c );
	 }

      public static bool ucs2_definedp( int  c )
	 {
	    // !!!!! i.e. between 0x0000 and 0xFFFF :  have to look for a better implementation
	    return ((Char.MinValue <= c) && (c <= Char.MaxValue));
	 }

      public static char ucs2_toupper( char  c )
	 {
	    return Char.ToUpper( c );
	 }

      public static char ucs2_tolower( char  c )
	 {
	    return Char.ToLower( c );
	 }

      //////
      // Unicode strings
      //////
      public static bool UCS2_STRINGP( Object  o )
	 {
	    return (o is char[]);
	 }

      public static int UCS2_STRING_LENGTH( char[]  o )
	 {
	    return o.Length;
	 }

      public static char[] make_ucs2_string( int len, char c )
	 {
	    char[] result= new char[len];

	    for ( int i= 0 ; i < len ; ++i )
	       result[i]= c;

	    return result;
	 }

      public static char UCS2_STRING_REF( char[] o, int i )
	 {
	    return o[i];
	 }

      public static Object UCS2_STRING_SET( char[] o, int i, char c )
	 {
	    o[i]= c;
	    return unspecified._unspecified;
	 }

      public static bool ucs2_strcmp( char[]  o1,
				      char[]  o2 )
	 {
	    return (new String( o1 )).Equals( new String( o2 ));
	 }

      public static bool ucs2_strcicmp( char[]  o1,
					char[]  o2 ) 
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ), true ) == 0);
	 }

      public static bool ucs2_string_lt( char[]  o1,
					 char[]  o2 )
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ) ) < 0);
	 }

      public static bool ucs2_string_gt( char[]  o1,
					 char[]  o2 )
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ) ) > 0);
	 }

      public static bool ucs2_string_le( char[]  o1,
					 char[]  o2 )
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ) ) <= 0);
	 }

      public static bool ucs2_string_ge( char[]  o1,
					 char[]  o2 )
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ) ) >= 0);
	 }

      public static bool ucs2_string_cilt( char[]  o1,
					   char[]  o2 ) 
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ), true ) < 0);
	 }

      public static bool ucs2_string_cigt( char[]  o1,
					   char[]  o2 )
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ), true ) > 0);
	 }

      public static bool ucs2_string_cile( char[]  o1,
					   char[]  o2 )
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ), true ) <= 0);
	 }

      public static bool ucs2_string_cige( char[]  o1,
					   char[]  o2 )
	 {
	    return (String.Compare( new String( o1 ), new String( o2 ), true ) >= 0);
	 }

      public static char[] c_subucs2_string( char[]  o,
					     int     min,
					     int     max )
	 {
	    return (new String( o )).Substring( min, max-min ).ToCharArray();
	 }

      public static char[] c_ucs2_string_copy( char[]  o ) 
	 {
	    return (new String( o )).ToCharArray();
	 }

      public static char[] c_ucs2_string_append( char[]  o1,
						 char[]  o2 )
	 {
	    char[]       result= new char[o1.Length + o2.Length];

	    o1.CopyTo( result, 0 );
	    o2.CopyTo( result, o1.Length );

	    return result;
	 }

      public static byte[] ucs2_string_to_utf8_string( char[]  o ) 
	 {
	    int          len = Encoding.UTF8.GetByteCount( o, 0, o.Length );
	    byte[]       result= new byte[len];

	    Encoding.UTF8.GetBytes( o, 0, o.Length, result, 0 );
	    return result;
	 }

      public static byte[] ucs2_string_to_utf8_string( String  s )
	 {
	    return ucs2_string_to_utf8_string( s.ToCharArray() );
	 }

      public static int utf8length( byte[]  bytes,
				    int     nb )
	 {
	    int          result= 0;
	    int          i= 0;

	    for( ; i < nb ; ++result )
	    {
	       byte       b= bytes[i];

	       if ((b & 0x80) == 0)
		  ++i;
	       else
		  if ((b & 0x20) == 0)
		     i+= 2;
		  else
		     i+= 3;
	    }

	    if (i != nb)
	       throw new ApplicationException( "Bad utf8 string : " + foreign.newstring( bytes ) );

	    return result;
	 }

      public static char[] utf8_string_to_ucs2_string( byte[]  bytes ) 
	 {
	    int          nb= bytes.Length;
	    int          nc= utf8length( bytes, nb );
	    char[]       chars= new char[nc];
	    int          i= 0;

	    for ( int j= 0 ; j < nc ; ++j ) 
	    {
	       byte       b= bytes[i];

	       if ((b & 0x80) == 0) 
	       {
		  chars[j]= (char)b;
		  ++i;
	       } 
	       else if ((b & 0x20) == 0) 
	       {
		  chars[j]= (char)(((b & 0x1F) << 6) | (bytes[i+1] & 0x3F));
		  i+= 2;
	       }
	       else 
	       {
		  chars[j]= (char)(  ((b & 0x0F) << 12)
				     | ((bytes[i+1] & 0x3F) << 6)
				     | (bytes[i+2] & 0x3F));
		  i+= 3;
	       }
	    }

	    return chars;
	 }

      //////
      // PROCESS
      //////
      public static bool PROCESSP( Object  o ) 
	 {
	    return (o is process);
	 }

      public static process bgl_process_nil()
	 {
	    return process.nil();
	 }

      public static process c_run_process( Object  bhost,
					   Object  bfork,
					   Object  bwaiting,
					   Object  binput,
					   Object  boutput,
					   Object  berror,
					   byte[]  bcommand,
					   Object  bargs,
					   Object  benv )
	 {
	    return new process( ((bhost is byte[]) ? (byte[])bhost : null),     // !!!!! can't directly cast to byte[] ?????
				bfork != bbool.faux,
				bwaiting != bbool.faux,
				binput,
				boutput,
				berror,
				bcommand,
				bargs,
				benv );
	 }

      public static Object c_unregister_process( process  o )
	 {
	    return bigloo.foreign.BUNSPEC;
	 }

      public static int PROCESS_PID( process  o )
	 {
	    return o.pid();
	 }

      public static Object PROCESS_INPUT_PORT( process  o )
	 {
	    return o.input_port;
	 }

      public static Object PROCESS_OUTPUT_PORT( process  o )
	 {
	    return o.output_port;
	 }

      public static Object PROCESS_ERROR_PORT( process  o )
	 {
	    return o.error_port;
	 }

      public static bool c_process_alivep( process  o )
	 {
	    return o.alivep();
	 }

      public static Object c_process_wait( process  o )
	 {
	    o.waitfor();

	    return unspecified._unspecified;
	 }

      public static Object c_process_xstatus( process  o )
	 {
	    return o.xstatus();
	 }

      public static Object c_process_send_signal( process  o,
						  int      s )
	 {
	    return o.send_signal( s );
	 }

      public static Object c_process_kill( process  o )
	 {
	    return o.kill();
	 }

      public static Object c_process_stop( process  o )
	 {
	    return o.stop();
	 }

      public static Object c_process_continue( process  o )
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
      public static bool STRINGP( Object  o )
	 {
	    return (o is byte[]);
	 }

      // Conversions
      public static byte[] string_to_bstring( byte[]  o )
	 {
	    return o;
	 }

      public static byte[] BSTRING_TO_STRING( byte[]  o )
	 {
	    return o;
	 }

      public static int strtol( byte[] s, int i, int radix )
	 {
	    int len = s.Length;

	    return ((len == 0) ? 0 : parseint( s, i, len, radix ));
	 }

      public static int strtoul( byte[] s, int i, int radix )
	 {
	    return strtol(s, i, radix);
	 }

      public static long strtoll( byte[] s, int i, int radix )
	 {
	    int len= s.Length;

	    return ((len == 0) ? 0 : parselong( s, i, len, radix ));
	 }

      public static long strtoull( byte[] s, int i, int radix )
	 {
	    return strtoll(s, i, radix);
	 }

      
      public static byte[] integer_to_string( int n, int radix )
	 {
	    // !!!!! beware: the following code is almost the same than in the next function !!!!!
	    if (radix == 10)
	       return getbytes( n.ToString( "d" ) );
	    else
	    {
	       int bits= (n < 0 ? 1 : 0);
	       int abs_n= Math.Abs( n );
	       int abs_n2= abs_n;

	       do
	       {
		  ++bits;
		  abs_n2/= radix;
	       }
	       while (0 < abs_n2);

	       byte[] result= new byte[bits];

	       do
	       {
		  --bits;
		  result[bits]= hexa[abs_n % radix];
		  abs_n/= radix;
	       }
	       while (0 < bits);

	       if (n < 0)
		  result[0]= (byte)'-';

	       return result;
	    }
	 }
      
      public static byte[] unsigned_to_string( int n, int radix )
	 {
	    int bits= 0;
	    uint abs_n= (uint)n;
	    uint abs_n2= abs_n;
	    uint uradix = (uint)radix;

	    do
	    {
	       ++bits;
	       abs_n2/= uradix;
	    }
	    while (0 < abs_n2);

	    byte[] result= new byte[bits];

	    do
	    {
	       --bits;
	       result[bits]= hexa[abs_n % uradix];
	       abs_n/= uradix;
	    }
	    while (0 < bits);
       
	    return result;
	 }
      
      public static byte[] uelong_to_string( long n, int radix )
	 {
	    int bits= 0;
	    ulong abs_n= (ulong)n;
	    ulong abs_n2= abs_n;
	    uint uradix = (uint)radix;

	    do
	    {
	       ++bits;
	       abs_n2 /= uradix;
	    }
	    while (0 < abs_n2);

	    byte[] result= new byte[bits];

	    do
	    {
	       --bits;
	       result[bits]= hexa[abs_n % uradix];
	       abs_n /= uradix;
	    }
	    while (0 < bits);
       
	    return result;
	 }
      
      public static byte[] ullong_to_string( long n, int radix )
	 {
	    return uelong_to_string( n, radix );
	 }
      
      public static byte[] integer_to_string_padding( int n, int padding, int radix )
	 {
	    byte[] tmp = integer_to_string( n < 0 ? -n : n, radix );
	    if( tmp.Length < padding ) {
	       byte[] result = new byte[ padding ];
	       bcopy( tmp, 0, result, (padding-tmp.Length), tmp.Length);
	 
	       for( int i = (padding - tmp.Length) - 1; i >= 0; i-- ) {
		  result[ i ] = (byte)'0';
	       }
	 
	       if( n < 0 ) result[ 0 ] = (byte)'-';
	 
	       return result;
	    } else {
	       return tmp;
	    }
	 }

      public static byte[] elong_to_string( long n, int radix )
	 {
	    // !!!!! beware: the following code is almost the same than in the previous function !!!!!
	    if (radix == 10)
	       return getbytes( n.ToString( "d" ) );
	    else
	    {
	       int bits= (n < 0 ? 1 : 0);
	       long abs_n= Math.Abs( n );
	       long abs_n2= abs_n;

	       do
	       {
		  ++bits;
		  abs_n2/= radix;
	       }
	       while (0 < abs_n2);

	       byte[] result= new byte[bits];

	       do
	       {
		  --bits;
		  result[bits]= hexa[abs_n % radix];
		  abs_n/= radix;
	       }
	       while (0 < bits);

	       if (n < 0)
		  result[0]= (byte)'-';

	       return result;
	    }
	 }

      public static byte[] llong_to_string( long n, int radix )
	 {
	    return elong_to_string( n, radix );
	 }

      public static byte[] real_to_string( double n )
	 {
	    if ( isnan(n) ) return getbytes( "+nan.0" );
	    if ( isinf(n) )
	        if ( n < 0.0 )
		    return getbytes( "-inf.0" );
		else
		    return getbytes( "+inf.0" );
	    return getbytes( n.ToString( "F" ) );
	 }

      public static double strtod( byte[] s, int i )
	 {
	    int len = s.Length;

	    return ((len == 0) ? 0 : Double.Parse( newstring( s ) ));
	 }

      // Open functions
      public static int STRING_REF( byte[] s, int i )
	 {
	    return (s[i] & 0xFF);
	 }

      public static Object STRING_SET( byte[] s, int i, int cn )
	 {
	    s[i]= (byte)cn;
	    return unspecified._unspecified;
	 }

      // Lib functions
      // Des fois j'me demande un peu!!...faut pas, faut pas...
      public static readonly Hashtable marked_string= new Hashtable();

      public static int STRING_LENGTH( byte[]  s )
	 {
	    return s.Length;
	 }

      // Creations
      public static byte[] make_string( int  n, int  init )
	 {
	    byte[] r= new byte[ n ];
	    byte _in= (byte)init;

	    for ( int i= 0 ; i < n ; ++i )
	       r[i]= _in;

	    return r;
	 }

      public static byte[] make_string_sans_fill( int  n )
	 {
	    return new byte[n];
	 }

      public static byte[] bgl_string_shrink( byte[] src, int len )
	 {
	    return c_substring( src, 0, len );
	 }
   
      public static byte[] c_substring( byte[] src, int min, int max )
	 {
	    int len= max - min;
	    byte[] dst= new byte[len];

	    for ( int i= 0 ; i < len ; ++i )
	       dst[i]= src[min+i];

	    return dst;
	 }

      public static Object ill_char_rep( int  cn )
	 {
	    byte[] r= new byte[5];

	    r[0]= (byte)'#';
	    r[1]= (byte)'a';
	    r[2]= (byte)(((byte)'0') + (cn / 100));
	    r[3]= (byte)(((byte)'0') + ((cn / 10) % 10));
	    r[4]= (byte)(((byte)'0') + (cn % 10));

	    return r;
	 }

      public static void bcopy( byte[] src, int i, byte[] dst, int j, int n )
	 {
	    if (src == dst) 
	    {
	       if (j > i) 
	       {
		  for ( int k= 1 ; k <= n ; ++k )
		     dst[j+n-k]= src[i+n-k];
		  return;
	       }
	    }
	    for ( int k= 0 ; k < n ; ++k )
	       dst[j+k]= src[i+k];
	 }

      public static byte[] string_append( byte[] o1, byte[] o2 )
	 {
	    int n1= o1.Length;
	    int n2= o2.Length;
	    byte[] r= new byte[n1 + n2];

	    bcopy( o1, 0, r, 0, n1 );
	    bcopy( o2, 0, r, n1, n2 );

	    return r;
	 }

      public static byte[] string_append_3( byte[] o1, byte[] o2, byte[] o3 )
	 {
	    int n1= o1.Length;
	    int n2= o2.Length;
	    int n3= o3.Length;
	    byte[] r= new byte[n1 + n2 + n3];

	    bcopy( o1, 0, r, 0, n1 );
	    bcopy( o2, 0, r, n1, n2 );
	    bcopy( o3, 0, r, n1+n2, n3 );

	    return r;
	 }

      public static byte[] bgl_escape_C_string( byte[] src, int start, int end )
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

	 byte[] result = new byte[size];
	 int j = 0;

	 for (int i = start; i < end; ++i, ++j) {
	    if (src[i] != '\\') {
	       result[j] = src[i];
	    } else {
	       byte cn = src[++i];

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
			byte s0 = src[i];
			byte s1 = src[i + 1];
			byte s2 = src[i + 2];

			if (isdigit(s0) && isdigit(s1) && isdigit(s2)) {
			   result[j] = (byte) (64 * ((int) (s0 - '0'))
					       + 8 * ((int) (s1 - '0'))
					       + ((int) (s2 - '0')));
			   i += 2;
			} else {
			   if (((s0 == (byte) 'x') || (s0 == (byte) 'X'))
			       && isxdigit(s1)
			       && isxdigit(s2) ) {
			      byte n1 = xdigit_to_byte( s1 );
			      byte n2 = xdigit_to_byte( s2 );

			      result[j] = (byte) (n1 * 16 + n2);
			      i += 2;
			   } else {
			      if (i + 4 < end) {
				 byte s3 = src[i + 3];
				 byte s4 = src[i + 4];
				 
				 if( ((s0 == (byte) 'u') || (s0 == (byte) 'U'))
				      && isxdigit(s1)
				      && isxdigit(s2) 
				      && isxdigit(s3) 
				      && isxdigit(s4) ) {
				    byte n1 = xdigit_to_byte( s1 );
				    byte n2 = xdigit_to_byte( s2 );
				    byte n3 = xdigit_to_byte( s3 );
				    byte n4 = xdigit_to_byte( s4 );
				    char u =
				       (char)(n1*4096 + n2*512 + n3*16 + n4);
				    char[] ucs2 = make_ucs2_string( 1, u );
				    byte[] utf8 = ucs2_string_to_utf8_string( ucs2 );

				    bcopy(utf8, 0, result, j, STRING_LENGTH( utf8 ));
				    
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
	    byte[] res = new byte[ size - utf8shrink ];
	    bcopy( result, 0, res, 0, size - utf8shrink );
	    return res;
	 } else {
	    return result;
	 }
      }

      static bool isdigit( byte  cn )
	 {
	    return(((byte)'0' <= cn) && (cn <= (byte)'9'));
	 }

      static bool isxdigit( byte b )
	 {
	    if(isdigit( b ) ) {
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

      public static byte[] bgl_escape_scheme_string( byte[] src, int start, int end ) {
	 int w = 0;

	 for( int i = start; i < end; ++i ) {
	    ++w;
	    if (src[i] == (byte)'\\') ++i;
	 }

	 byte[] dst = new byte[w];

	 w = 0;
	 for( int i = start; i < end; ++i ) {
	    byte cn = src[i];

	    if (cn != (byte)'\\') {
	       dst[w++] = (byte)cn;
	    } else {
	       ++i;
	       dst[w++] = ((src[i]=='n') ? (byte)'\n' : src[i]);
	    }
	 }

	 return dst;
      }

      public static byte[] string_for_read( byte[]  src )
	 {
	    return create_string_for_read( src, false );
	 }

      public static byte[] create_string_for_read( byte[]  src,
						   bool    symbolp )
	 {
	    int               len= src.Length;
	    int               w= 0;

	    for ( int i= 0 ; i < len ; ++i )
	    {
	       byte            cn= src[i];

	       switch (cn)
	       {
		  case (byte)'\n': 
		  case (byte)'\t':
		  case (byte)'\b':
		  case (byte)'\r':
		  case (byte)'\f':
		  case (byte)11:
		  case (byte)'"':
		  case (byte)'\\':
		     w+= 2;
		  break;
		  case (byte)'|':
		     w+= (symbolp ? 2 : 1);
		  break;
		  default:
		     w+= (((32 <= cn) && (cn <= 126)) ? 1 : 4);
		     break;
	       }
	    }

	    byte[]            res= new byte[w];

	    w= 0;
	    for ( int i= 0 ; i < len ; ++i )
	    {
	       byte            cn= src[i];

	       switch (cn) 
	       {
		  case (byte)'\n':
		     res[w++]= (byte)'\\';
		  res[w++]= (byte)'n';
		  break;
		  case (byte)'\t':
		     res[w++]= (byte)'\\';
		  res[w++]= (byte)'t';
		  break;
		  case (byte)'\b':
		     res[w++]= (byte)'\\';
		  res[w++]= (byte)'b';
		  break;
		  case (byte)'\r':
		     res[w++]= (byte)'\\';
		  res[w++]= (byte)'r';
		  break;
		  case (byte)'\f':
		     res[w++]= (byte)'\\';
		  res[w++]= (byte)'f';
		  break;
		  case (byte)11:
		     res[w++]= (byte)'\\';
		     res[w++]= (byte)'v';
		     break;
		  case (byte)'"':
		  case (byte)'\\':
		     res[w++]= (byte)'\\';
		  res[w++]= cn;
		  break;
		  case (byte)'|':
		     if (symbolp)
			res[w++]= (byte)'\\';
		  res[w++] = (byte)'|';
		  break;
		  default:
		     if ((32 <= cn) && (cn <= 126))
			res[w++]= cn;
		     else 
		     {
			int       icn= cn & 0xFF;

			res[w++]= (byte)'\\';
			res[w++]= (byte)('0' + ((icn >> 6) & 0x7));
			res[w++]= (byte)('0' + ((icn >> 3) & 0x7));
			res[w++]= (byte)('0' + (icn  & 0x7));
		     }
		     break;
	       }
	    }

	    return res;
	 }

      // Side effects
      public static Object blit_string( byte[] src, int i1,
					byte[] dst, int i2,
					int n )
	 {
	    bcopy( src, i1, dst, i2, n );
	    return unspecified._unspecified;
	 }

      // Comparisons
      public static bool bigloo_strcmp( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;

	    if (n1 != n2)
	       return false;

	    for ( int i= 0 ; i < n1 ; ++i )
	       if (s1[i] != s2[i])
		  return false ;

	    return true;
	 }

      public static bool bigloo_strcmp_at(byte[] s1, byte[] s2, int d)
	 {
	    int n1 = s1.Length;
	    int n2 = s2.Length;

	    if ((d < 0) || (n1 < n2 + d))
	       return false;

	    for (int i = 0; i < n2; ++i)
	       if (s1[i + d] != s2[i])
		  return false;

	    return true;
	 }

      public static bool bigloo_strcmp_ci_at(byte[] s1, byte[] s2, int d)
	 {
	    int n1 = s1.Length;
	    int n2 = s2.Length;

	    if (d < 0 || (n1 < n2 + d))
	       return false;

	    for (int i = 0; i < n2; ++i)
	       if (toupper(s1[i + d]) != toupper(s2[i]))
		  return false;

	    return true;
	 }

      public static bool bigloo_strncmp_at(byte[] s1, byte[] s2, int d, int l)
	 {
	    int n1 = s1.Length;
	    int n2 = s2.Length;
	    int n = n2 < l ? n2 : l;

	    if (d < 0 || (n1 < n + d) || (n < 0))
	       return false;

	    for (int i = 0; i < n; ++i)
	       if (s1[i + d] != s2[i])
		  return false;

	    return true;
	 }

      public static bool bigloo_strncmp_ci_at(byte[] s1, byte[] s2, int d, int l)
	 {
	    int n1 = s1.Length;
	    int n2 = s2.Length;
	    int n = n2 < l ? n2 : l;

	    if (d < 0 || (n1 < n + d) || (n < 0))
	       return false;

	    for (int i = 0; i < n; ++i)
	       if (toupper(s1[i + d]) != toupper(s2[i]))
		  return false;

	    return true;
	 }

      public static bool bigloo_strncmp( byte[] s1, byte[] s2, int l )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;

	    if ((n1 >= l) && (n2 >= l))
	    {
	       for ( int i= 0 ; i < l ; ++i )
		  if (s1[i] != s2[i])
		     return false;
	       return true;
	    } 
	    else
	       return false;
	 }

      public static bool bigloo_strncmp_ci( byte[] s1, byte[] s2, int l )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;

	    if ((n1 >= l) && (n2 >= l)) 
	    {
	       for ( int i= 0 ; i < l ; ++i )
		  if (toupper( s1[i] ) != toupper( s2[i] ))
		     return false;
	       return true;
	    } 
	    else
	       return false;
	 }

      public static bool strcicmp( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;

	    if (n1 != n2)
	       return false;
	    for ( int i= 0 ; i < n1 ; ++i )
	       if (toupper( s1[i] ) != toupper( s2[i] ))
		  return false;
	    return true;
	 }

      public static bool string_le( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;
	    int          min= (n1 < n2) ? n1 : n2;

	    for ( int i= 0 ; i < min ; ++i )
	    {
	       int c1= s1[i] & 0xFF;
	       int c2= s2[i] & 0xFF;

	       if (c1 != c2)
		  return (c1 <= c2);
	    }

	    return (n1 <= n2);
	 }

      public static bool string_lt( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;
	    int min= (n1 < n2) ? n1 : n2;

	    for ( int i= 0 ; i < min ; ++i )
	    {
	       int c1= s1[i] & 0xFF;
	       int c2= s2[i] & 0xFF;

	       if (c1 != c2)
		  return (c1 < c2);
	    }

	    return (n1 < n2);
	 }

      public static bool string_gt( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;
	    int min= (n1 < n2) ? n1 : n2;

	    for ( int i= 0 ; i < min ; ++i )
	    {
	       int c1= s1[i] & 0xFF;
	       int c2= s2[i] & 0xFF;

	       if (c1 != c2)
		  return (c1 > c2);
	    }

	    return (n1 > n2);
	 }
    
      public static bool string_ge( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;
	    int min= (n1 < n2) ? n1 : n2;

	    for ( int i= 0 ; i < min ; ++i )
	    {
	       int c1= s1[i] & 0xFF;
	       int c2= s2[i] & 0xFF;

	       if (c1 != c2)
		  return (c1 >= c2);
	    }

	    return (n1 >= n2);
	 }
    
      public static bool string_cilt( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;
	    int min= (n1 < n2) ? n1 : n2;

	    for ( int i= 0 ; i < min ; ++i )
	    {
	       int c1= toupper( s1[i] & 0xFF );
	       int c2= toupper( s2[i] & 0xFF );

	       if (c1 != c2)
		  return (c1 < c2);
	    }

	    return (n1 < n2);
	 }

      public static bool string_cile( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;
	    int min= (n1 < n2) ? n1 : n2;

	    for ( int i= 0 ; i < min ; ++i )
	    {
	       int c1= toupper( s1[i] & 0xFF );
	       int c2= toupper( s2[i] & 0xFF );

	       if (c1 != c2)
		  return (c1 <= c2);
	    }

	    return (n1 <= n2);
	 }

      public static bool string_cigt( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;
	    int min= (n1 < n2) ? n1 : n2;

	    for (int i= 0 ; i < min ; ++i )
	    {
	       int c1= toupper( s1[i] & 0xFF );
	       int c2= toupper( s2[i] & 0xFF );

	       if (c1 != c2)
		  return (c1 > c2);
	    }

	    return (n1 > n2);
	 }

      public static bool string_cige( byte[] s1, byte[] s2 )
	 {
	    int n1= s1.Length;
	    int n2= s2.Length;
	    int min= (n1 < n2) ? n1 : n2;

	    for ( int i= 0 ; i < min ; ++i )
	    {
	       int c1= toupper( s1[i] & 0xFF );
	       int c2= toupper( s2[i] & 0xFF );

	       if (c1 != c2)
		  return (c1 >= c2);
	    }

	    return (n1 >= n2);
	 }

      //////
      // KEYWORD
      //////
      // Predicates
      public static bool KEYWORDP( Object  o )
	 {
	    return (o is keyword);
	 }

      // Open functions
      public static byte[] KEYWORD_TO_STRING( keyword  key )
	 {
	    return key.pname;
	 }

      // Lib functions
      public static keyword string_to_keyword( byte[]  s )
	 {
	    return keyword.make_keyword( s );
	 }

      //////
      // SYMBOL
      //////
      // Predicates
      public static bool SYMBOLP( Object  o )
	 {
	    return (o is symbol);
	 }

      // Lib functions
      public static bool symbol_exists_p( byte[]  name )
	 {
	    return symbol.exists( name );
	 }

      public static symbol string_to_symbol( byte[]  name )
	 {
	    return symbol.make_symbol( name );
	 }

      public static symbol string_to_symbol( String name )
	 {
	    return symbol.make_symbol( getbytes( name ) );
	 }

      public static byte[] SYMBOL_TO_STRING( symbol  o )
	 {
	    // CARE why not a correct signature in Scheme
	    return o.pname;
	 }

      public static Object GET_SYMBOL_PLIST( symbol  o )
	 {
	    return o.cval;
	 }

      public static Object SET_SYMBOL_PLIST( symbol  o,
					     Object  v )
	 {
	    o.cval= v;
	    return unspecified._unspecified;
	 }

      public static Object GET_KEYWORD_PLIST( keyword  o )
	 {
	    return o.cval;
	 }

      public static Object SET_KEYWORD_PLIST( keyword  o,
					      Object   v )
	 {
	    o.cval= v;
	    return unspecified._unspecified;
	 }

      //////
      // CELL
      //////
      // Predicates
      public static bool CELLP( Object  o )
	 {
	    return (o is cell);
	 }

      // Open functions
      public static cell MAKE_CELL( Object  o )
	 {
	    return new cell( o );
	 }

      public static Object CELL_SET( cell    o,
				     Object  v )
	 {
	    o.car= v;
	    return unspecified._unspecified;
	 }

      public static Object CELL_REF( cell  o )
	 {
	    return o.car;
	 }

      public static Object _EVMEANING_ADDRESS( Object  o )
	 {
	    // CARE Where is declared this function !!
	    return new cell( o );
	 }

      public static Object _EVMEANING_ADDRESS_REF( Object  o )
	 {
	    return ((cell)o).car;
	 }

      public static Object _EVMEANING_ADDRESS_SET( Object  o,
						   Object  v )
	 {
	    ((cell)o).car= v;
	    return unspecified._unspecified;
	 }

      //////
      // FOREIGN
      //////
      public static bool FOREIGNP( Object  o )
	 {
	    return ((o != null) && !(o is obj) && !(o is bobject));
	 }

      public static bool FOREIGN_NULLP( Object  f )
	 {
	    return (f == null);
	 }

      public static bool OBJECT_PTR_NULL( Object  o )
	 {
	    return (o == null);
	 }

      public static Object MAKE_VOID_STAR_NULL() {
	 return null;
      }
	 
      public static foreign void_star_to_obj( Object o ) {
	 return new bigloo.foreign();
      }

      public static bool STRING_PTR_NULL( Byte[]  o )
	 {
	    return (o == null);
	 }

      public static Byte[] MAKE_STRING_PTR_NULL() {
	 return null;
      }
	 
      public static bool FOREIGN_EQP( Object  f1,
				      Object  f2 )
	 {
	    return (f1 == f2);
	 }

      public static symbol FOREIGN_ID( Object  f )
	 {
	    // CARE where defined?
	    return string_to_symbol( f.GetType().ToString() );
	 }

      // !!!!! c'est quoi cette instance de foreign ?????
      public static cobj FOREIGN_TO_COBJ( Object  f )
	 {
	    return null;
	 }

      public static cobj obj_to_cobj( Object  o ) 
	 {
	    return (cobj)o;
	 }

      public static Object COBJ_TO_OBJ( cobj  o ) 
	 {
	    return o;
	 }

      //////
      // CUSTOM
      //////
      public static bool CUSTOMP( Object  o )
	 {
	    return (o is custom);
	 }

      public static custom bgl_custom_nil()
	 {
	    return custom.nil();
	 }
      
      public static bool CUSTOM_CMP( custom  c1,
				     custom  c2 )
	 {
	    return c1.equal( c2 );
	 }

      public static int CUSTOM_HASH_NUMBER( custom  c )
	 {
	    return c.hash();
	 }

      public static byte[] CUSTOM_IDENTIFIER( custom  c )
	 {
	    return c.identifier;
	 }

      public static Object CUSTOM_IDENTIFIER_SET( custom  c,
						  byte[]  s )
	 {
	    c.identifier= s;
	    return unspecified._unspecified;
	 }

      //////
      // DATE
      //////
      public static bool DATEP( Object  o )
	 {
	    return (o is bigloo.date);
	 }

      public static date bgl_make_date( int   s,
					int   min,
					int   h,
					int   d,
					int   mon,
					int   y,
					int   tz, 
					bool  istz,
					int   isdst )
	 {
	    return new bigloo.date( s, min, h, d, mon, y, tz, istz, isdst );
	 }

      public static date bgl_seconds_to_date( long  sec )
	 {
	    return new bigloo.date( sec );
	 }

      public static date bgl_seconds_to_utc_date( long  sec )
	 {
	    date d= new bigloo.date( sec );
	    d.date_time= d.date_time.ToUniversalTime();
	    return d;
	 }

      public static long bgl_current_seconds()
	 {
	    return (long)((DateTime.Now.Ticks / 1e7) - date.EPOCH_in_seconds);
	 }

      public static long bgl_current_microseconds()
	 {
	    return (long)((DateTime.Now.Ticks / 1e3) - date.EPOCH_in_seconds);
	 }

      public static long bgl_date_to_seconds( date d )
	 {
	    return (long)((d.date_time.Ticks / 1e7) - date.EPOCH_in_seconds);
	 }

      public static byte[] bgl_seconds_to_string( long sec )
	 {
	    return getbytes( bgl_seconds_to_date( sec ).date_time.ToString() );
	 }

      public static long bgl_integer_to_seconds(int i)
	 {
	    return i;
	 }

      public static byte[] bgl_seconds_to_utc_string(long sec)
	 {
	    return getbytes( bgl_seconds_to_utc_date(sec).date_time.ToString() );
	 }

      public static int BGL_DATE_SECOND( date d )
	 {
	    return d.date_time.Second;
	 }

      public static int BGL_DATE_MINUTE( date d )
	 {
	    return d.date_time.Minute;
	 }

      public static int BGL_DATE_HOUR( date d )
	 {
	    return d.date_time.Hour;
	 }

      public static int BGL_DATE_DAY( date d )
	 {
	    return d.date_time.Day;
	 }

      public static int BGL_DATE_WDAY( date d )
	 {
	    return ((int)d.date_time.DayOfWeek + 1);
	 }

      public static int BGL_DATE_YDAY( date d )
	 {
	    return d.date_time.DayOfYear;
	 }

      public static int BGL_DATE_MONTH( date d )
	 {
	    return d.date_time.Month;
	 }

      public static int BGL_DATE_YEAR( date d )
	 {
	    return d.date_time.Year;
	 }

      public static int BGL_DATE_TIMEZONE( date d )
	 {
	    return d.timezone;
	 }

      public static int BGL_DATE_ISDST( date d )
	 {
	    return -1;
	 }

      private static readonly byte[][] day_names= { getbytes( "Sunday" ),
						    getbytes( "Monday" ),
						    getbytes( "Tuesday" ),
						    getbytes( "Wednesday" ),
						    getbytes( "Thursday" ),
						    getbytes( "Friday" ),
						    getbytes( "Saturday" ) };

      private static readonly byte[][] day_anames= { getbytes( "Sun" ),
						     getbytes( "Mon" ),
						     getbytes( "Tue" ),
						     getbytes( "Wed" ),
						     getbytes( "Thu" ),
						     getbytes( "Fri" ),
						     getbytes( "Sat" ) };

      private static readonly byte[][] month_names= { getbytes( "January" ),
						      getbytes( "February" ),
						      getbytes( "March" ),
						      getbytes( "April" ),
						      getbytes( "May" ),
						      getbytes( "June" ),
						      getbytes( "July" ),
						      getbytes( "August" ),
						      getbytes( "September" ),
						      getbytes( "October" ),
						      getbytes( "November" ),
						      getbytes( "December" ) };

      private static readonly byte[][] month_anames= { getbytes( "Jan" ),
						       getbytes( "Feb" ),
						       getbytes( "Mar" ),
						       getbytes( "Apr" ),
						       getbytes( "May" ),
						       getbytes( "Jun" ),
						       getbytes( "Jul" ),
						       getbytes( "Aug" ),
						       getbytes( "Sep" ),
						       getbytes( "Oct" ),
						       getbytes( "Nov" ),
						       getbytes( "Dec" ) };

      public static byte[] bgl_day_name( int n )
	 {
	    return day_names[n - 1];
	 }

      public static byte[] bgl_day_aname( int n )
	 {
	    return day_anames[n - 1];
	 }

      public static byte[] bgl_month_name( int n )
	 {
	    return month_names[n - 1];
	 }

      public static byte[] bgl_month_aname( int n )
	 {
	    return month_anames[n - 1];
	 }

      //////
      // PAIR
      //////
      // Predicates
      public static bool PAIRP( Object  o )
	 {
	    return (o is pair);
	 }

      // Open functions
      public static pair MAKE_PAIR( Object car, Object cdr )
	 {
	    // CARE where defined?
	    return new pair( car, cdr );
	 }

      public static Object CAR( pair  c )
	 {
	    return c.car;
	 }

      public static Object CDR( pair  c )
	 {
	    return c.cdr;
	 }

      public static Object SET_CAR( pair    c,
				    Object  o )
	 {
	    c.car= o;
	    return unspecified._unspecified;
	 }

      public static Object SET_CDR( pair    c,
				    Object  o )
	 {
	    c.cdr= o;
	    return unspecified._unspecified;
	 }

      //////
      // EXTENDED_PAIR
      //////
      public static bool EXTENDED_PAIRP( Object  o )
	 {
	    return (o is extended_pair);
	 }

      public static extended_pair MAKE_EXTENDED_PAIR( Object  car,
						      Object  cdr,
						      Object  cer )
	 {
	    return new extended_pair( car, cdr, cer );
	 }

      public static Object CER( extended_pair  c )
	 {
	    return c.cer;
	 }

      public static Object SET_CER( extended_pair  c,
				    Object         o )
	 {
	    c.cer= o;
	    return unspecified._unspecified;
	 }

      //////
      // VECTOR
      //////
      // Predicates
      public static bool VECTORP( Object o )
	 {
	    // CARE bug mono
	    if (o is byte[]) return false;
	    if (o is char[]) return false;
	    return (o is Object[]);
	 }

      public static void FREE_VECTOR_UNCOLLECTABLE(Object[]v)
	 {
	    ;
	 }

      // Open functions
      public static int VECTOR_LENGTH( Object[] v )
	 {
	    return v.Length;
	 }

      public static Object VECTOR_REF( Object[] v, int i )
	 {
	    return v[i];
	 }

      public static Object VECTOR_SET( Object[] v, int i, Object o )
	 {
	    v[i]= o;
	    return unspecified._unspecified;
	 }

      public static bool BOUND_CHECK( int n1, int n2 )
	 {
	    return (n1 < n2);
	 }

      public static bool BOUND_CHECK( long n1, long n2 )
	 {
	    return (n1 < n2);
	 }

      public static Object VECTOR_TAG_SET( Object[] v, int n )
	 {
	    return unspecified._unspecified;
	 }

      public static int VECTOR_TAG( Object[] v )
	 {
	    return 0;
	 }

      // Lib functions
      public static Object[] make_vector( int n, Object init )
	 {
	    Object[] r = new Object[n];

	    for ( int i= 0 ; i < n ; ++i )
	       r[i]= init;

	    return r;
	 }

      public static Object[] create_vector( int n )
	 {
	    return new Object[n];
	 }

      public static Object fill_vector( Object[] v, int len, Object o )
	 {
	    for ( int i= 0 ; i < len ; ++i )
	       v[i]= o;
	    return unspecified._unspecified;
	 }

      public static Object[] sort_vector( Object[] v, procedure p )
	 {
	    int n = v.Length;

	    for ( int incr= n/2 ; incr != 0 ; incr/=2 )
	       for ( int i= incr ; i<n ; ++i )
		  for ( int j= i-incr ; j >= 0 ; j-= incr )
		     if (p.funcall2( v[j], v[j+incr] ) != bbool.faux)
			break;
		     else 
		     {
			Object tmp = v[j + incr];

			v[j + incr]= v[j];
			v[j]= tmp;
		     }

	    return v;
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
	 for(int i=0 ; l != BNIL ; l = ((pair) l).cdr, i++)
	    r[i] = ((pair) l).car;
	 return(r);
      }
      static int list_length(Object l) {
	 int i = 0;
	 for( ; l != BNIL ;  l = ((pair) l).cdr, i++);
	 return(i);
      }
 
      //////
      // HVECTOR
      //////
      // Predicates
      public static bool BGL_HVECTORP(Object o) {
	 return (o is hvector);
      }
      public static bool BGL_S8VECTORP(Object o) {
	 return (o is s8vector);
      }
      public static bool BGL_U8VECTORP(Object o) {
	 return (o is u8vector);
      }
      public static bool BGL_S16VECTORP(Object o) {
	 return (o is s16vector);
      }
      public static bool BGL_U16VECTORP(Object o) {
	 return (o is u16vector);
      }
      public static bool BGL_S32VECTORP(Object o) {
	 return (o is s32vector);
      }
      public static bool BGL_U32VECTORP(Object o) {
	 return (o is u32vector);
      }
      public static bool BGL_S64VECTORP(Object o) {
	 return (o is s64vector);
      }
      public static bool BGL_U64VECTORP(Object o) {
	 return (o is u64vector);
      }
      public static bool BGL_F32VECTORP(Object o) {
	 return (o is f32vector);
      }
      public static bool BGL_F64VECTORP(Object o) {
	 return (o is f64vector);
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

      public static sbyte BGL_S8VREF(s8vector v, int l) {
	 return v.objs[l];
      }
      public static void BGL_S8VSET(s8vector v, int l, sbyte o) {
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
      public static ushort BGL_U16VREF(u16vector v, int l) {
	 return v.objs[l];
      }
      public static void BGL_U16VSET(u16vector v, int l, ushort o) {
	 v.objs[l] = o;
      }
   
      public static int BGL_S32VREF(s32vector v, int l) {
	 return v.objs[l];
      }
      public static void BGL_S32VSET(s32vector v, int l, int o) {
	 v.objs[l] = o;
      }
      public static uint BGL_U32VREF(u32vector v, int l) {
	 return v.objs[l];
      }
      public static void BGL_U32VSET(u32vector v, int l, uint o) {
	 v.objs[l] = o;
      }
   
      public static long BGL_S64VREF(s64vector v, int l) {
	 return v.objs[l];
      }
      public static void BGL_S64VSET(s64vector v, int l, long o) {
	 v.objs[l] = o;
      }
      public static ulong BGL_U64VREF(u64vector v, int l) {
	 return v.objs[l];
      }
      public static void BGL_U64VSET(u64vector v, int l, ulong o) {
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
   
   
      //////
      // TVECTOR
      //////
      public static bool TVECTORP( Object  o )
	 {
	    // !!!!! CARE bug mint/mono
	    if (o is char[])
	       return false;
	    return (   (o is double[])
		       || (o is int[])
		       || (o is long[])
		       || (o is bool[])
		       || (o is Object[]) );
	 }

      private static readonly Object[] desc_table= new Object[4];

      public static Object TVECTOR_DESCR( Object  o )
	 {
	    if (o is double[])
	       return desc_table[0];
	    if (o is int[])
	       return desc_table[1];
	    if (o is long[])
	       return desc_table[2];
	    if (o is bool[])
	       return desc_table[2];
	    if (o is Object[])
	       return desc_table[3];
	    return fail( "tvector_desc", "Unknown tvec object", o );
	 }

      public static Object TVECTOR_DESCR_SET( Object  o,
					      Object  desc )
	 {
	    if (o is double[])
	       desc_table[0]= desc;
	    else if (o is int[])
	       desc_table[1]= desc;
	    else if (o is long[])
	       desc_table[2]= desc;
	    else if (o is bool[])
	       desc_table[2]= desc;
	    else if (o is Object[])
	       desc_table[3]= desc;

	    return unspecified._unspecified;
	 }

      public static int TVECTOR_LENGTH( Object  o )
	 {
	    if (o is double[])
	       return ((double[])o).Length;
	    if (o is int[])
	       return ((int[])o).Length;
	    if (o is long[])
	       return ((long[])o).Length;
	    if (o is bool[])
	       return ((bool[])o).Length;
	    if (o is Object[])
	       return ((Object[])o).Length;

	    fail( "tvector_length", "Unknown tvec object", o );
	    return 0;
	 }

      //////
      // WEAKPTR
      //////
    
      public static bool BGL_WEAKPTRP(Object o){
	 return (o is weakptr);
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
      public static bool STRUCTP( Object  o )
	 {
	    return (o is bstruct);
	 }

      public static Object STRUCT_KEY( bstruct  o )
	 {
	    return o.key;
	 }

      public static Object STRUCT_KEY_SET( bstruct o, Object v )
	 {
	    o.key= v;
	    return unspecified._unspecified;
	 }

      public static int STRUCT_LENGTH( bstruct o )
	 {
	    return o.values.Length;
	 }

      public static Object STRUCT_REF( bstruct o, int i )
	 {
	    return o.values[i];
	 }

      public static Object STRUCT_SET( bstruct o, int i, Object v )
	 {
	    o.values[i]= v;
	    return unspecified._unspecified;
	 }

      public static Object UNSAFE_STRUCT_REF( bstruct o, int i )
	 {
	    return o.values[i];
	 }

      public static Object UNSAFE_STRUCT_SET( bstruct o, int i, Object v )
	 {
	    o.values[i]= v;
	    return unspecified._unspecified;
	 }

      public static bstruct create_struct( symbol key, int size )
	 {
	    return new bstruct( key, size );
	 }

      public static bstruct make_struct( symbol key, int size, Object o )
	 {
	    return new bstruct( key, size, o );
	 }

      /////
      // OBJECT
      /////

      public static int OBJECT_TYPE= 0;

      public static Object BGL_OBJECT_WIDENING_SET( bobject o, Object v )
	 {
	    o.widening= v;
	    return unspecified._unspecified;
	 }

      public static Object BGL_OBJECT_WIDENING( bobject  o )
	 {
	    return o.widening;
	 }

      public static int BGL_OBJECT_CLASS_NUM( bobject  o )
	 {
	    return o.header;
	 }

      public static Object BGL_OBJECT_CLASS_NUM_SET( bobject  o, int n )
	 {
	    o.header= n;
	    return unspecified._unspecified;
	 }

      public static procedure bgl_make_generic( procedure p ) {
	 return new generic( p.index, p.arity, 3, p );
      }
   
      public static Object BGL_HEAP_DEBUG_MARK_OBJ( Object  o )
	 {
	    return o;
	 }

      //////
      // PROCEDURE
      //////
      public static bool PROCEDUREP( Object o )
	 {
	    return (o is procedure);
	 }

      public static procedure buildproc( procedure p, int i, int a, int n )
	 {
	    p.index= i;
	    p.arity= a;
	    p.env= new Object[n];
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
	    return getbytes( "" );
	 }
   
      public static Object bgl_string_to_procedure_entry(byte [] s)
	 {
	    fail( "string->procedure-entry", "not impleemented", s );
	    return BUNSPEC;
	 }
      
      public static int PROCEDURE_ARITY( procedure  p )
	 {
	    return p.arity;
	 }

      public static int PROCEDURE_LENGTH( procedure p )
	 {
	    return p.env.Length;
	 }

      public static bool PROCEDURE_CORRECT_ARITYP( procedure p, int i )
	 {
	    int arity = p.arity;

	    return ((arity == i) || ((arity < 0) && (-i-1 <= arity)));
	 }

      public static Object PROCEDURE_SET( procedure p, int i, Object o )
	 {
	    p.env[i]= o;
	    return unspecified._unspecified;
	 }

      public static Object PROCEDURE_REF( procedure p, int i )
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

      public static procedure MAKE_EL_PROCEDURE( int n )
	 {
	    return new procedure( 0, 0, new Object[n] );
	 }

      public static Object PROCEDURE_EL_SET( procedure p, int i, Object o )
	 {
	    p.env[i]= o;
	    return unspecified._unspecified;
	 }

      public static Object PROCEDURE_EL_REF( procedure p, int i )
	 {
	    return p.env[i];
	 }

      public static procedure MAKE_EL_PROCEDURE_1( int n )
	 {
	    return new procedure( 0, 0, new Object[n] );
	 }

      public static Object PROCEDURE_1_EL_SET( procedure p, int i, Object o )
	 {
	    p.env[i]= o;
	    return unspecified._unspecified;
	 }

      public static Object PROCEDURE_1_EL_REF( procedure p, int i )
	 {
	    return p.env[i];
	 }

      public static Object PROCEDURE_L_REF( procedure p, int i )
	 {
	    return p.env[i];
	 }

      public static Object PROCEDURE_L_SET( procedure p, int i, Object o )
	 {
	    p.env[i]= o;
	    return unspecified._unspecified;
	 }

      // CARE ?!?!
      public static Object PUSH_BEFORE( procedure p )
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
      public static readonly int BGL_ERROR = 1;
      public static readonly int BGL_LOCATION_ERROR = 2;
   
      public static readonly int BGL_TYPE_ERROR = 10;
      public static readonly int BGL_TYPENAME_ERROR = 11;
      public static readonly int BGL_INDEX_OUT_OF_BOUND_ERROR = 12;
   
      public static readonly int BGL_IO_ERROR = 20;
      public static readonly int BGL_IO_PORT_ERROR = 21;
   
      public static readonly int BGL_IO_READ_ERROR = 31;
      public static readonly int BGL_IO_WRITE_ERROR = 32;
      public static readonly int BGL_IO_CLOSED_ERROR = 33;
      public static readonly int BGL_IO_FILE_NOT_FOUND_ERROR = 34;
      public static readonly int BGL_IO_UNKNOWN_HOST_ERROR = 35;
      public static readonly int BGL_IO_PARSE_ERROR = 36;
      public static readonly int BGL_IO_MALFORMED_URL_ERROR = 37;
      public static readonly int BGL_IO_SIGPIPE_ERROR = 38;
      public static readonly int BGL_IO_TIMEOUT_ERROR = 39;
      public static readonly int BGL_PROCESS_EXCEPTION = 50;

      public static bool BGL_DYNAMIC_ENVP( Object o )
	 {
	    return (o is bgldynamic);
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

      public static Object BGL_ENV_BYTECODE(bgldynamic env)
	 {
	    return env.bytecode;
	 }

      public static Object BGL_ENV_BYTECODE_SET(bgldynamic env, Object al)
	 {
	    env.bytecode = al;
	    return BUNSPEC;
	 }

      public static Object BGL_BYTECODE()
	 {
	    return bgldynamic.abgldynamic.get().bytecode;
	 }

      public static Object BGL_BYTECODE_SET(Object al)
	 {
	    bgldynamic.abgldynamic.get().bytecode = al;
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
      private static void bigloo_abort() 
	 {
	 }
   
      public static Exception fail( Object  proc,
				    Object  msg,
				    Object  env )
	 {
	    __cb__.failure( proc, msg, env );

	    ApplicationException e= new ApplicationException( "bigloo error..." );

	    Console.Error.WriteLine( stackwriter.demangle( e.Message ) );
	    Console.Error.WriteLine( stackwriter.demangle( e.StackTrace ) );
	    Console.Error.Flush();

	    bigloo_abort();

	    Object exitv = __cb__.exit_apply( BINT( 1 ) );

	    if (exitv is bint)
	       Environment.Exit( CINT( (bint)exitv ) );
	    else
	       Environment.Exit( 1 );

	    return e;    // dead code
	 }

      public static void internalerror( Exception  e )
	 {
	    Console.Error.WriteLine( "Internal error: {0}", e.Message );
	    Console.Error.WriteLine( stackwriter.demangle( e.StackTrace ) );
	    Console.Error.Flush();

	    bigloo_abort();

	    Environment.Exit( 1 );
	 }

      public static readonly byte[] nomsg= getbytes( "null" );

      public static Exception fail( String proc, String msg, Object env )
	 {
	    return fail( getbytes( proc ), (msg == null) ? nomsg : getbytes( msg ), env );
	 }

      public static void exit(int n)
	 {
	    Environment.Exit( n );
	 }

      public static Object BGL_EXITD_TOP()
	 {
	    return bgldynamic.abgldynamic.get().exitd_top;
	 }

      public static bool BGL_EXITD_BOTTOMP( Object o ) {
	 return o == BFALSE;
      }
      
      public static Object BGL_EXITD_TOP_SET( Object  o )
	 {
	    bgldynamic.abgldynamic.get().exitd_top= o;
	    return unspecified._unspecified;
	 }

      public static Object BGL_EXITD_VAL()
	 {
	    return bgldynamic.abgldynamic.get().exitd_val;
	 }

      public static Object BGL_EXITD_VAL_SET( Object  o )
	 {
	    bgldynamic.abgldynamic.get().exitd_val= o;
	    return unspecified._unspecified;
	 }

      public static Object setexit()
	 {
	    return new exit();
	 }

      public static exit EXITD_TO_EXIT( Object  o )
	 {
	    //print("** self " + o);
	    return (exit)o;
	 }

      public static bool EXITD_USERP( Object  o )
	 {
	    //print("** userp " + o);
	    return ((exit)o).userp > 0;
	 }

      public static bool EXITD_CALLCCP( Object  o )
	 {
	    return false;
	 }

      public static bint EXITD_STAMP( Object  o )
	 {
	    //print("** stamp " + o);
	    return bint.BZERO;
	 }

      public static Object jumpexit( Object  excep,
				     Object  value )
	 {
	    //print( "** jump " + excep + " " + value );
	    throw new bexception( excep, value );
	 }

      public static int BGL_MVALUES_NUMBER()
	 {
	    return bgldynamic.abgldynamic.get().mvalues_number;
	 }

      public static int BGL_MVALUES_NUMBER_SET( int n )
	 {
	    bgldynamic.abgldynamic.get().mvalues_number= n;
	    return n;
	 }

      public static Object BGL_MVALUES_VAL( int n )
	 {
	    return bgldynamic.abgldynamic.get().mvalues_values[n];
	 }

      public static Object BGL_MVALUES_VAL_SET( int n, Object  o )
	 {
	    bgldynamic.abgldynamic.get().mvalues_values[n]= o;
	    return unspecified._unspecified;
	 }

      public static Object debug_handler( bexception  v, exit tag )
	 {
	    if (tag.userp == 0) 
	    {
	       //print( "** PROTECT " + v + " " + tag );
	       return v.value;
	    }
	    if (v.tag == tag)
	    {
	       //   print( "** TAG reached " + v + " " + tag );
	       return v.value;
	    }
	    //	print( "** TAG forward " + v + " " + tag );
	    throw v;
	 }

      public static Object debug_dot_handler( Exception  v, exit tag )
	 {
	    try
	    {
	       Console.Out.Flush();

	       String stack_trace= stackwriter.demangle( v.StackTrace );

	       return fail( "Exception caught in Bigloo .NET runtime", v.Message, ("\n"+stack_trace) );
	    } 
	    catch ( bexception bex )
	    {
	       return debug_handler( bex, tag );
	    }
	 }

      public static Object PUSH_EXIT( exit  v,
				      int  protect )
	 {
	    //print( "** PUSH " + v + " " + protect );
	    v.userp= protect;
	    v.prev= (exit)bgldynamic.abgldynamic.get().exitd_top;
	    bgldynamic.abgldynamic.get().exitd_top= v;
	    return unspecified._unspecified;
	 }

      public static Object POP_EXIT() 
	 {
	    //print("** POP " + EXIT_TOP + " -> " + ((exit)EXIT_TOP).prev);
	    bgldynamic.abgldynamic.get().exitd_top= ((exit)bgldynamic.abgldynamic.get().exitd_top).prev;
	    return unspecified._unspecified;
	 }

      public static Object CALLCC_JUMP_EXIT(exit v, Object o)
	 {
	    return o;
	 }	
   
      public static Object call_cc( procedure  p )
	 {
	    exit         saved= (exit)bgldynamic.abgldynamic.get().exitd_top;
	    exit         me= (exit) setexit();
	    Object       r;

	    PUSH_EXIT( me, 1 );

	    try 
	    {
	       if (PROCEDURE_CORRECT_ARITYP( p, 1 ))
	       {
		  r= p.funcall1( new callcc() );
		  bgldynamic.abgldynamic.get().exitd_top= saved;
	       } 
	       else 
	       {
		  r= null;
		  fail( "call/cc", "Wrong arity", p );
	       }
	    } 
	    catch (bexception x)
	    {
	       r= debug_handler( x, me );
	       bgldynamic.abgldynamic.get().exitd_top= saved;
	    }

	    return r;
	 }

      //////
      // EVAL
      //////
      // CARE
      public static readonly procedure BIGLOO_EXIT_ENV= new procedure();

      public static Object PUSH_TRACE( Object o, Object l )
	 {
	    new stack_trace( o, l );
	    return unspecified._unspecified;
	 }

      public static Object SET_TRACE_NAME( Object o )
	 {
	    stack_trace.set_trace(o);
	    return unspecified._unspecified;
	 }

      public static Object POP_TRACE() 
	 {
	    return stack_trace.pop_trace();
	 }

      public static Object BGL_ENV_PUSH_TRACE( bgldynamic env, Object o, Object l )
	 {
	    new stack_trace( o, l );
	    return unspecified._unspecified;
	 }

      public static Object BGL_ENV_SET_TRACE_NAME( bgldynamic env, Object o )
	 {
	    stack_trace.set_trace(o);
	    return unspecified._unspecified;
	 }

      public static Object BGL_ENV_SET_TRACE_LOCATION( bgldynamic env, Object o )
	 {
	    stack_trace.set_trace_location(o);
	    return unspecified._unspecified;
	 }

      public static Object BGL_ENV_POP_TRACE( bgldynamic env ) 
	 {
	    return stack_trace.pop_trace();
	 }

      public static Object get_trace_stack( int depth )
	 {
	    return stack_trace.get( depth );
	 }

      public static Object __EVMEANING_ADDRESS_REF( procedure  f )
	 {
	    return f.funcall0();
	 }

      public static Object __EVMEANING_ADDRESS_SET( procedure  f,
						    Object     v )
	 {
	    f.funcall1( v );
	    return unspecified._unspecified;
	 }

      public static Object eval_funcall_0( procedure  fun )
	 {
	    return fun.funcall0();
	 }

      public static Object eval_funcall_1( procedure  fun,
					   Object     a0 )
	 {
	    return fun.funcall1( a0 );
	 }

      public static Object eval_funcall_2( procedure  fun,
					   Object     a0,
					   Object a1 )
	 {
	    return fun.funcall2( a0, a1 );
	 }

      public static Object eval_funcall_3( procedure  fun,
					   Object     a0,
					   Object     a1,
					   Object a2 )
	 {
	    return fun.funcall3( a0, a1, a2 );
	 }

      public static Object eval_funcall_4( procedure  fun,
					   Object     a0,
					   Object     a1,
					   Object     a2,
					   Object     a3 )
	 {
	    return fun.funcall4( a0, a1, a2, a3 );
	 }

      public static Object eval_apply( procedure  fun,
				       Object     list )
	 {
	    return fun.apply( list );
	 }

      //////
      // FILES
      ////

      public static bool unlink( byte[]  file )
	 {
	    try
	    {
	       String f= newstring( file );

	       if( File.Exists( f ) ) {
		  File.Delete( f );
		  return File.Exists( f );
	       } else {
		  return true;
	       }
	    }
	    catch (Exception)
	    {
	       return true;
	    }
	 }

      public static bool rmdir( byte[]  file )
	 {
	    try
	    {
	       String f = newstring( file );
	
	       if( Directory.Exists( f ) ) {
		  Directory.Delete( f );
		  return Directory.Exists( f );
	       } else {
		  return true;
	       }
	    }
	    catch (Exception)
	    {
	       return true;
	    }
	 }

      public static bool fexists( byte[]  file )
	 {
	    String s = newstring( file );

	    return ( File.Exists( s ) || Directory.Exists( s ));
	 }

      public static int rename( byte[] old, byte[] to )
	 {
	    try
	    {
	       File.Move( newstring( old ), newstring( to ) );

	       return 0;
	    }
	    catch (Exception)
	    {
	       return 69;
	    }
	 }

      public static bool mkdir( byte[] path, int mode )
	 {
	    try
	    {
	       String s = newstring( path );
	       DirectoryInfo dir = new DirectoryInfo( s );

	       if (dir.Exists)
		  return false;

	       // !!!!! DirectoryInfo.Create is not implemented in pnet 0.5.4
	       // dir.Create();
	       Directory.CreateDirectory( s );

	       dir= new DirectoryInfo( s );             // !!!!! bug in MS VM ?, dir must be reconstructed !!!!!

	       return dir.Exists;
	    }
	    catch (Exception)
	    {
	       return false;
	    }
	 }

      public static bool directoryp( byte[]  file )
	 {
	    return Directory.Exists( newstring( file ) );
	 }

      public static long bgl_file_size( byte[]  file )
	 {
	    return (long)(new FileInfo( newstring( file ) )).Length;
	 }

      public static long bgl_last_modification_time( byte[]  file )
	 {
	    return (long)( (new FileInfo( newstring( file ) )).LastWriteTime.Ticks
			   - date.EPOCH_in_seconds);
	 }

      public static Object directory_to_list( byte[]  name )
	 {
	    FileInfo[] files= new DirectoryInfo( newstring( name ) ).GetFiles();
	    Object result= BNIL;

	    foreach (FileInfo file_info in files)
	       result= new pair( getbytes( file_info.Name ), result );

	    return result;
	 }

      //////
      // SYSTEM and OS
      //////
      public static int PTR_ALIGNMENT= 2;     // !!!!! devrait etre const => maj bytecode
      public static int SIGINT= 3;            // !!!!! devrait etre const => maj bytecode
      public static int SIGILL= 4;            // !!!!! devrait etre const => maj bytecode
      public static int SIGFPE= 8;            // !!!!! devrait etre const => maj bytecode
      public static int SIGBUS= 7;            // !!!!! devrait etre const => maj bytecode
      public static int SIGSEGV= 11;          // !!!!! devrait etre const => maj bytecode  
      public static int SIGPIPE= 13;          // !!!!! devrait etre const => maj bytecode  

      public static int sigsetmask( int n )
	 {
	    return n;
	 }

      public static Object c_signal( int n, Object p )
	 {
	    return unspecified._unspecified;
	 }

      public static Object get_signal_handler( int n )
	 {
	    return BFALSE;
	 }

      public static Object reset_console( Object  o )
	 {
	    return unspecified._unspecified;
	 }

      static byte[] get_property( String  name,
				  String  def )
	 {
	    String            s= null;

	    try {
	       s= Environment.GetEnvironmentVariable( name );
	    }
	    catch (Exception) {
	    }

	    if (s == null)
	       if (def == null)
		  return null;
	       else
		  return getbytes( def );
	    else 
	       return getbytes( s );
	 }

      public static byte[] getenv( byte[] name )
	 {
	    String sname= newstring( name );

	    switch (sname)
	    {
	       case "HOME":
	       case "USERPROFILE":
		  try {
		     return GetPersonalFolder();
		  }
		  catch {
		     return null;
		  }
	       case "USER":
		  try {
		     // Environment.UserName not implemented in Rotor
		     return GetUserName();
		  }
		  catch {
		     return null;
		  }
	       case "TMPDIR":
		  return getbytes( Path.GetTempPath() );
	       case "BIGLOOSTACKDEPTH":
	       case "BIGLOOLIVEPROCESS":
		  return get_property( "bigloo." + sname, "0" );
	       default:
		  return get_property( "bigloo." + sname, null );
	    }
	 }

      private static byte[] GetUserName()
	 {
	    return getbytes( Environment.UserName );
	 }

      private static byte[] GetPersonalFolder()
	 {
	    return getbytes( Environment.GetFolderPath( Environment.SpecialFolder.Personal ) );
	 }

      public static bool getenv_exists( byte[]  name )
	 {
	    String sname= newstring( name );

	    return ( sname.Equals( "HOME" )
		     || sname.Equals( "USERPROFILE" )
		     || sname.Equals( "USER" )
		     || sname.Equals( "TMPDIR" )
		     || sname.Equals( "BIGLOOSTACKDEPTH" )
		     || sname.Equals( "BIGLOOLIVEPROCESS" )
		     || (get_property( "bigloo." + sname, null ) != null));
	 }

      public static int bgl_setenv( byte[]  name,
				    byte[]  val )
	 {
	    // meaningless with .NET
	    return 0;
	 }

      public static Object bgl_time( procedure p ) {
	 bgldynamic env = BGL_CURRENT_DYNAMIC_ENV();
	 DateTime start = DateTime.Now;
	 TimeSpan duration;
	 Object res;
      
	 res = p.funcall0();
	 duration = DateTime.Now - start;
	 
	 env.mvalues_number = 4;
	 env.mvalues_values[ 1 ] = BINT( duration.Milliseconds );
	 env.mvalues_values[ 2 ] = BINT( 0 );
	 env.mvalues_values[ 3 ] = BINT( 0 );
	 
	 return res;
      }

      public static byte[] getcwd( byte[]  path, int i )
	 {
	    return getbytes( Environment.CurrentDirectory );
	 }

      public static int bgl_file_mode(byte[] f)
	 {
	    return 0;
	 }

      public static int bgl_file_uid(byte[] f)
	 {
	    return 0;
	 }

      public static int bgl_file_gid(byte[] f)
	 {
	    return 0;
	 }

      public static bool bgl_chmod( byte[]  f, bool r, bool w, bool x )
	 {
	    if (bigloo.os.OS_CLASS.Equals( getbytes( "unix" ) ))
	    {
	       pair args = new pair( f, nil._nil );
	       StringBuilder mode = new StringBuilder( "a" );

	       mode.Append( r ? "+r " : "-r " );
	       mode.Append( w ? "+w " : "-w " );
	       mode.Append( x ? "+x " : "-x " );

	       args= new pair( getbytes( mode.ToString() ), args );

	       process p = new process( null,
					false,
					true,
					null,
					bigloo.foreign.BUNSPEC,
					bigloo.foreign.BUNSPEC,
					getbytes( "chmod" ),
					args,
					null );
	       obj x_status= p.xstatus();

	       return ((x_status is bint) && (((bint)x_status).value == 0));
	    }

	    return false;
	 }

      public static bool bgl_chmod( byte[]  f, int v )
	 {
	    return false;     // !!!!! faire comme en Java !!!!!
	 }

      public static bool chdir( byte[]  path )
	 {
	    try
	    {
	       Environment.CurrentDirectory= newstring( path );
	    }
	    catch (Exception)
	    {
	       return true;
	    }

	    return false;
	 }

      public static Object command_line= BNIL;

      public static readonly byte[] executable_name= getbytes( Environment.GetCommandLineArgs()[0] );

      public static int system( byte[]  cmd )
	 {
	    process      p= new process( null,
					 false,
					 true,
					 null,
					 bigloo.foreign.BUNSPEC,
					 bigloo.foreign.BUNSPEC,
					 cmd,
					 null,
					 null );

	    return ((p.xstatus() is bint)
		    ? ((bint)p.xstatus()).value
		    : -1);
	 }

      public static long getpid() {
	 return 0;
      }
      
      public static byte[] c_date() 
	 {
	    return getbytes( DateTime.Now.ToString() );
	 }

      public static Object BIGLOO_EXIT( Object n )
	 {
	    Object       exitv= __cb__.exit_apply( n );

	    trace_exit();
	    Environment.Exit( (exitv is bint) ? ((bint)exitv).value : 0 );

	    return null;
	 }

      public static readonly byte[] BGL_DYNAMIC_LOAD_INIT= getbytes( "BGL_DYNAMIC_LOAD_INIT" );

      public static int bgl_dload( byte[] filename, byte[] init_sym, byte[] mod_sym )
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

      //////
      // SOCKET
      //////
      public static byte[] bgl_host( byte[] hostname ) {
	 try {
	    IPHostEntry i = Dns.GetHostByName( newstring( hostname ) );
	    IPAddress addr = i.AddressList[ 0 ];
	    return getbytes( addr.ToString() );
	 } catch( Exception ) {
	    __cb__.failure( symbol.make_symbol( getbytes( "host" ) ),
			    getbytes( "unknown or misspelled host name" ),
			    hostname );
	    return null;
	 }
      }

      public static Object bgl_hostinfo( byte[] hostname ) {
	 pair p = MAKE_PAIR( bgl_host( hostname ), BNIL );
	 return MAKE_PAIR( MAKE_PAIR( "addresses", p ), BNIL );
      }
   
      public static bool SOCKETP( Object  o )
	 {
	    return (o is socket);
	 }

      public static bool BGL_SOCKET_SERVERP( Object  o )
	 {
	    return (o is server_socket);
	 }

      public static bool BGL_SOCKET_CLIENTP( Object  o )
	 {
	    return (o is client_socket);
	 }

      public static socket bgl_make_client_socket( byte[] hostname,
						   int port,
						   int timeo,
						   byte[] inbuf, byte[] outbuf )
	 {
	    return new client_socket( hostname, port, inbuf, outbuf );
	 }

      public static socket bgl_make_server_socket( Object name,
						   int port,
						   int backlog )
	 {
	    return new server_socket( name, port );
	 }

      public static Object SOCKET_HOSTNAME( socket  s )
	 {
	    return s.HOSTNAME();
	 }

      public static Object SOCKET_HOSTIP( socket  s ) 
	 {
	    return s.HOSTIP();
	 }

      public static bool SOCKET_DOWNP( socket  s )
	 {
	    return s.DOWNP();
	 }

      public static int SOCKET_PORT( socket  s )
	 {
	    return s.PORT();
	 }

      public static input_port SOCKET_INPUT( socket  s )
	 {
	    return (input_port)s.input;
	 }

      public static output_port SOCKET_OUTPUT( socket  s )
	 {
	    return (output_port)s.output;
	 }

      public static socket bgl_socket_accept( socket  s,
					      bool errp,
					      byte[] inbuf,
					      byte[] outbuf )
	 {
	    return ((server_socket)s).accept( inbuf, outbuf, errp );
	 }

      public static byte[] socket_local_addr( socket s )
	 {
	    return s.local_addr();
	 }

      public static Object socket_shutdown( socket s, bool b )
	 {
	    return s.shutdown( b );
	 }

      public static Object socket_close( socket s )
	 {
	    return s.close();
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
	    return BUNSPEC;
	 }
	       
      public static Object bgl_setsockopt( socket s, keyword se, Object val )
	 {
	    return BFALSE;
	 }
	       
      
      //////
      // INPUT
      //////

      public static int default_io_bufsiz= 1024;             // !!!!! devrait �tre const ?  Si oui => maj bytecode...

      public static bool INPUT_PORTP( Object o )
	 {
	    return (o is input_port);
	 }

      public static bool INPUT_STRING_PORTP( Object o )
	 {
	    return (o is input_string_port);
	 }

      public static bool INPUT_PROCEDURE_PORTP( Object o )
	 {
	    return (o is input_procedure_port);
	 }

      public static bool INPUT_GZIP_PORTP( Object o )
	 {
	    return (o is input_gzip_port);
	 }

      public static input_port getCurrentInputPort( bgldynamic env )
	 {
	    return env.current_input_port;
	 }

      public static void setCurrentInputPort( bgldynamic env, input_port  o )
	 {
	    env.current_input_port= o;
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
      
      public static byte[] BGL_INPUT_PORT_BUFFER(input_port o)
	 {
	    return o.buffer;
	 }
      
      public static void bgl_input_port_buffer_set(input_port o, byte[] b)
	 {
	    o.bufsiz = b.Length;
	    o.buffer = b;

	    if( !(o is input_string_port) ) {
	       b[ 0 ] = 0;
	    }
	 }
   
      public static byte[] BGL_OUTPUT_PORT_BUFFER(output_port o)
	 {
	    return new byte[1];
	 }
      
      public static void bgl_output_port_buffer_set(output_port o, byte[] b)
	 {
	    ;
	 }
      
      public static Object bgl_open_input_file( byte[] s, byte[] b )
	 {
	    Object result= BFALSE;

	    try 
	    {
	       if (input_pipe_port.pipe_name_p( s )) 
		  result= new input_pipe_port( s, b );
	       else
		  result= new input_file_port( s, b );
	    }
	    catch (Exception)
	    {
	    }

	    return result;
	 }

      public static Object bgl_open_input_pipe( byte[] s, byte[] b )
	 {
	    return new input_pipe_port( newstring( s ), b );
	 }

      public static Object bgl_open_input_resource( byte[] s, byte[] b )
	 {
	    return BFALSE;
	 }

      public static int bgl_input_port_bufsiz( input_port p ) 
	 {
	    return p.bufsiz;
	 }

      public static Object bgl_open_input_string( byte[] s, int start )
	 {
	    return new input_string_port( s, start );
	 }

      public static Object bgl_open_input_string_bang( byte[] s )
	 {
	    return new input_string_port( s, 0 );
	 }

      public static Object bgl_open_input_procedure( procedure p, byte[] b )
	 {
	    return new input_procedure_port( p, b );
	 }

      public static Object bgl_open_input_gzip_port( procedure p, input_port n, byte[] b )
	 {
	    return new input_gzip_port( p, n, b );
	 }

      public static input_port BGL_INPUT_GZIP_PORT_INPUT_PORT( input_port p ) {
	 if( p is input_gzip_port )
	    return ((input_gzip_port)p).input_port;
	 else {
	    fail( "input-gzip-port-input-port", "Illegal input port", p );
	    return p;
	 }
      }

      public static Object bgl_open_input_c_string( byte[] s )
	 {
	    return new input_string_port( s, 0 );
	 }

      public static Object bgl_reopen_input_c_string( input_port p, byte[] s )
	 {
	    ((input_string_port)p).reopen_input_c_string( s );
	    return p;
	 }

      public static bool bgl_input_port_timeout_set( input_port p, int to ) {
	 return false;
      }
   
      public static bool bgl_output_port_timeout_set( output_port p, int to ) {
	 return false;
      }
   
      public static object bgl_input_port_seek( input_port p, int pos )
	 {
	    return p.bgl_input_port_seek( pos );
	 }

      public static Object bgl_input_port_reopen( input_port p )
	 {
	    return p.bgl_input_port_reopen();
	 }

      public static Object bgl_output_port_seek( output_port p, int pos ) 
	 {
	    return p.bgl_output_port_seek( pos );
	 }

      public static bool CLOSED_RGC_BUFFER( input_port o )
	 {
	    return o.other_eof;
	 }

      public static int INPUT_PORT_TOKENPOS(input_port p)
	 {
	    return INPUT_PORT_FILEPOS(p)-RGC_BUFFER_LENGTH(p);
	 }

      public static int INPUT_PORT_FILEPOS( input_port p )
	 {
	    return p.filepos;
	 }

      public static int INPUT_PORT_FILLBARRIER( input_port p )
	 {
	    return p.pseudoeof;
	 }

      public static void INPUT_PORT_FILLBARRIER_SET( input_port p, int e )
	 {
	    p.pseudoeof = e;
	 }

      public static long BGL_INPUT_PORT_LENGTH( input_port p )
	 {
	    return p.length;
	 }

      public static void BGL_INPUT_PORT_LENGTH_SET( input_port p, long e )
	 {
	    p.length = e;
	 }

      public static int OUTPUT_PORT_FILEPOS( output_port p )
	 {
	    return 0;
	 }

      public static byte[] INPUT_PORT_NAME( input_port p )
	 {
	    return foreign.getbytes( p.name );
	 }

      public static byte[] OUTPUT_PORT_NAME( output_port p )
	 {
	    return p.name;
	 }

      public static int RGC_BUFFER_POSITION( input_port p )
	 {
	    return (p.forward - p.matchstart);
	 }

      public static int RGC_BUFFER_GET_CHAR( input_port p )
	 {
	    return (p.buffer[p.forward++] & 0xFF);
	 }

      public static int rgc_buffer_unget_char( input_port p, int c )
	 {
	    p.filepos--;
      
	    if (0 < p.matchstop)
	       --p.matchstop;
	    else 
	    {
	       p.buffer[0]= (byte)c;
	       if (p.bufpos == 0) 
	       {
		  p.bufpos= 1;
		  p.buffer[1]= (byte)'\0';
	       }
	    }
	    return c;
	 }

      private static void rgc_buffer_reserve_space(input_port p, int amount)
	 {
	    int bufsize = p.bufsiz;
	    int bufpos = p.bufpos;
	    int matchstop = p.matchstop;

	    if ( matchstop >= amount ) return;

	    if ( (matchstop + (bufsize - (bufpos-1))) >= amount ) {
	       // shift the buffer to the right
	       int diff = amount - matchstop;

	       bcopy( p.buffer, matchstop, p.buffer, amount, bufpos-1 - matchstop);

	       p.bufpos += diff;
	       p.matchstop += diff;
	    } else {
	       p.rgc_double_buffer();
	       rgc_buffer_reserve_space(p, amount);
	    }
	 }
      public static bool rgc_buffer_insert_substring(input_port p, byte[] s, int from, int to)
	 {
	    if ( from < 0 ) return false;
	    if ( to > s.Length ) return false;
	    if ( p.bufsiz == 2) return false; // unbuffered port
	    if ( CLOSED_RGC_BUFFER( p )) return false;
	    if ( from >= to ) return true;

	    int len = to - from;

	    rgc_buffer_reserve_space(p, len);

	    int matchstop = p.matchstop;

	    bcopy(s, from, p.buffer, (matchstop - len), len);

	    if ( p.filepos >= len )
	       p.filepos -= len;
	    else
	       p.filepos = 0;

	    p.matchstop -= len;
	    p.forward    = p.matchstop;
	    p.matchstart = p.matchstop;
	    return true;
	 }

      public static bool rgc_buffer_insert_char(input_port p, int c)
      {
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

      public static int RGC_START_MATCH( input_port p )
	 {
	    return (p.forward= p.matchstart= p.matchstop);
	 }

      public static int RGC_STOP_MATCH( input_port p )
	 {
	    return (p.matchstop= p.forward);
	 }

      public static int RGC_SET_FILEPOS( input_port p )
	 {
	    return (p.filepos+= (p.matchstop - p.matchstart));
	 }

      public static int RGC_BUFFER_LENGTH( input_port p )
	 {
	    return (p.matchstop - p.matchstart);
	 }
    
      public static bool RGC_BUFFER_EMPTY( input_port p )
	 {
	    return (p.forward == p.bufpos);
	 }

      public static bool bgl_rgc_charready( input_port p )
	 {
	    return p.rgc_charready();
	 }

      public static byte[] bgl_password( byte[] prompt ) 
	 {
	    int len = 80;
	    byte[] chars = new byte[ len ];
	    int i = 0;
	    Console.Error.Write( prompt );
	    ConsoleKeyInfo key = Console.ReadKey( true );
	    while( key.KeyChar != '\r' ) {
	       if( i == len ) {
		  byte[] nchars = new byte[ len * 2 ];

		  for( i = 0; i < len; i++ ) {
		     nchars[ i ] = chars[ i ];
		  }

		  chars = nchars;
		  len *= 2;
	       }
	       chars[ i++ ] = (byte)key.KeyChar;
	       key = Console.ReadKey( true );
	    }
	    return chars;
	 } 
   
      public static bool rgc_buffer_eof_p( input_port p )
	 {
	    return ((p.buffer[p.forward] == 0) && (p.forward+1 == p.bufpos));
	 }

      private static int rgc_do_blit( input_port p, byte[] s, int o, int l ) {
	 RGC_START_MATCH( p );

	 while ( ((p.bufpos - p.matchstart) <= l) && !p.eof) {
	    // MS fix 6 Juillet 2005
	    int fsize = (p.bufsiz - p.bufpos);
	    p.forward= p.bufpos;
	    rgc_fill_buffer( p );
	    // less characters are available
	    if( (p.bufpos - p.forward) <  fsize ) break;
	 }

	 if ((p.bufpos - p.matchstart) <= l)
	    l= (p.bufpos - p.matchstart - 1);

	 p.forward= p.matchstart + l;
	 RGC_STOP_MATCH( p );
	 RGC_SET_FILEPOS( p );
	 bcopy( p.buffer, p.matchstart, s, o, l );

	 return l;
      }

      public static int bgl_rgc_blit_string( input_port p, byte[] s, int o, int l ) {
	 int bs = p.bufsiz;

	 if( CLOSED_RGC_BUFFER( p ) ) {
	    fail( getbytes( "rgc-blit-string" ),
		  getbytes( "input-port closed" ),
		  p );
	 }
      
	 if (l <= bs)
	    return rgc_do_blit( p, s, o, l );
	 else 
	 {
	    int             r= 0;

	    while (bs < l)
	    {
	       r+= rgc_do_blit( p, s, o, bs );
	       o+= bs;
	       l-= bs;
	    }
	    r+= rgc_do_blit( p, s, o, l );
	 
	    return r;
	 }
      }

      public static bool rgc_buffer_bof_p( input_port p )
	 {
	    return (p.filepos == 0);
	 }

      public static bool rgc_buffer_bol_p( input_port p )
	 {
	    if (p.matchstart > 0)
	       return (p.buffer[p.matchstart - 1] == '\n');
	    else
	       return (p.lastchar == (byte)'\n');
	 }

      public static bool rgc_buffer_eol_p( input_port p )
	 {
	    int c = RGC_BUFFER_GET_CHAR( p );

	    if (c == 0) 
	    {
	       if(!RGC_BUFFER_EMPTY( p ))
	       {
		  --p.forward;
		  return false;
	       }
	       if (rgc_fill_buffer( p ))
		  return rgc_buffer_eol_p( p );
	       return false;
	    }
	    --p.forward;
	    return (c == (byte)'\n');
	 }

      public static bool rgc_fill_buffer( input_port p )
	 {
	    --p.forward;
	    if (p.eof)
	       return false;

	    return p.rgc_fill_buffer();
	 }

      public static byte[] rgc_buffer_substring( input_port p, int o, int e )
	 {
	    return c_substring( p.buffer, p.matchstart + o, p.matchstart + e );
	 }

      public static byte[] rgc_buffer_escape_substring( input_port p, int o, int e, bool strict ) {
	 if( strict ) {
	    return bgl_escape_scheme_string( p.buffer, p.matchstart + o, p.matchstart + e );
	 } else {
	    return bgl_escape_C_string( p.buffer, p.matchstart + o, p.matchstart + e );
	 }
      }

      public static byte RGC_BUFFER_CHARACTER( input_port p )
	 {
	    return p.buffer[p.matchstart];
	 }

      public static int RGC_BUFFER_BYTE( input_port p )
	 {
	    return (int)p.buffer[p.matchstart];
	 }

      public static int RGC_BUFFER_BYTE_REF( input_port p, int offset )
	 {
	    return (int)p.buffer[p.matchstart + offset];
	 }

      public static symbol rgc_buffer_symbol( input_port p )
	 {
	    int start= p.matchstart;
	    int stop= p.matchstop;
	    int n= stop - start;
	    byte[] name= new byte[n];

	    for ( int i= 0 ; i < n ; ++i, ++start )
	       name[i]= (byte)(p.buffer[start] & 0xFF);

	    return symbol.make_symbol( name );
	 }

      public static symbol rgc_buffer_subsymbol( input_port p, int o, int e )
	 {
	    int start = p.matchstart + o;
	    int stop = p.matchstop + e;
	    int n = stop - start;
	    byte[] name = new byte[n];

	    for ( int i= 0 ; i < n ; ++i, ++start )
	       name[i]= (byte)(p.buffer[start] & 0xFF);

	    return symbol.make_symbol( name );
	 }

      public static symbol rgc_buffer_upcase_symbol( input_port p )
	 {
	    int          start= p.matchstart;
	    int          stop= p.matchstop;
	    int          n= stop - start;
	    byte[]       name= new byte[n];

	    for ( int i= 0 ; i < n ; ++i, ++start )
	       name[i]= (byte)toupper( p.buffer[start] & 0xFF );

	    return symbol.make_symbol( name );
	 }

      public static symbol rgc_buffer_downcase_symbol( input_port p )
	 {
	    int          start= p.matchstart;
	    int          stop= p.matchstop;
	    int          n= stop - start;
	    byte[]       name= new byte[n];

	    for ( int i= 0 ; i < n ; ++i, ++start )
	       name[i]= (byte)tolower( p.buffer[start] & 0xFF );

	    return symbol.make_symbol( name );
	 }

      public static keyword rgc_buffer_upcase_keyword( input_port p )
	 {
	    int start = p.matchstart;
	    int stop = p.matchstop;
	    int n = stop - start - 1;
	    byte[] name = new byte[n];

	    if( p.buffer[start] == ':' ) start++;
      
	    for ( int i = 0 ; i < n ; ++i, ++start )
	       name[i] = (byte)toupper( p.buffer[start] & 0xFF );

	    return keyword.make_keyword( name );
	 }

      public static keyword rgc_buffer_downcase_keyword( input_port p )
	 {
	    int start = p.matchstart;
	    int stop = p.matchstop;
	    int n = stop - start - 1;
	    byte[] name = new byte[n];

	    if( p.buffer[start] == ':' ) start++;
      
	    for ( int i = 0 ; i < n ; ++i, ++start )
	       name[i] = (byte)tolower( p.buffer[start] & 0xFF );

	    return keyword.make_keyword( name );
	 }

      public static keyword rgc_buffer_keyword( input_port p )
	 {
	    int start = p.matchstart;
	    int stop = p.matchstop;
	    int n = stop - start - 1;
	    byte[] name = new byte[n];

	    if( p.buffer[start] == ':' ) start++;
      
	    for ( int i = 0 ; i < n ; ++i, ++start )
	       name[i] = (byte)(p.buffer[start] & 0xFF);

	    return keyword.make_keyword( name );
	 }

      public static int rgc_buffer_fixnum( input_port p )
	 {
	    return parseint( p.buffer, p.matchstart, p.matchstop, 10 );
	 }

      public static Object rgc_buffer_integer(input_port p)
	 {
	    return parseinteger(p.buffer, p.matchstart, p.matchstop, 10);
	 }

      public static double rgc_buffer_flonum( input_port p )
	 {
	    return Double.Parse( newstring( c_substring( p.buffer, p.matchstart, p.matchstop ) ) );
	 }

      public static Object bgl_close_input_port( input_port p )
	 {
	    p.close();
	    return p;
	 }

      public static bool reset_eof( Object  p )
	 {
	    //print("RES_EOF " + p);
	    return ((input_port)p).reset_eof();
	 }

      //////
      // BINARY
      //////

      public static bool BINARY_PORTP( Object  o )
	 {
	    return (o is binary_port);
	 }

      public static bool BINARY_PORT_INP( binary_port p ) 
	 {
	    return p.stream.CanRead;
	 }

      public static bool BINARY_PORT_OUTP( binary_port p ) 
	 {
	    return p.stream.CanWrite;
	 }

      public static Object BINARY_PORT_TO_FILE( binary_port p ) 
	 {
	    return p.stream;
	 }

      public static Object open_output_binary_file( byte[]  file ) 
	 {
	    return new binary_port( new FileStream( newstring( file ), FileMode.OpenOrCreate, FileAccess.Write, FileShare.Read ) );
	 }

      public static Object append_output_binary_file( byte[]  file ) 
	 {
	    return new binary_port( File.Open( newstring( file ),
					       FileMode.Append,
					       FileAccess.Write ) );
	 }

      public static Object open_input_binary_file( byte[]  file )
	 {
	    return new binary_port( new FileStream( newstring( file ), FileMode.Open, FileAccess.Read, FileShare.ReadWrite ) );
	 }

      public static Object close_binary_port( binary_port p )
	 {
	    return p.close();
	 }

      public static Object bgl_flush_binary_port( binary_port p )
	 {
	    return p.flush();
	 }

      public static Object output_obj( binary_port p, Object obj )
	 {
	    return p.output_obj( obj );
	 }
	
      public static Object input_obj( binary_port p )
	 {
	    return p.input_obj();
	 }

      public static int BGL_INPUT_CHAR( binary_port p )
	 {
	    return p.stream.ReadByte();
	 }

      public static bool BGL_INT_EOFP( int  i ) 
	 {
	    return (i == -1);
	 }

      public static Object BGL_OUTPUT_CHAR( binary_port p, byte c) 
	 {
	    p.stream.WriteByte( c );
	    return p;
	 }

      public static byte[] bgl_input_string( binary_port p, int len )
	 {
	    byte[] buf = new byte[ len ];
	    int l = p.stream.Read( buf, 0, buf.Length );

	    if( l < len )
	       return bgl_string_shrink( buf, l );
	    else	
	       return buf;
	 }

      public static int bgl_input_fill_string( binary_port p, byte[] buf )
	 {
	    return p.stream.Read( buf, 0, buf.Length );
	 }

      public static int bgl_output_string( binary_port p, byte[] buf )
	 {
	    p.stream.Write( buf, 0, buf.Length );
	    return buf.Length;
	 }

      //////
      // OUTPUT
      //////
      public static output_port getCurrentOutputPort( bgldynamic env )
	 {
	    return env.current_output_port;
	 }

      public static void setCurrentOutputPort( bgldynamic env, output_port  o )
	 {
	    env.current_output_port = o;
	 }
   
      public static output_port getCurrentErrorPort( bgldynamic env )
	 {
	    return env.current_error_port;
	 }

      public static void setCurrentErrorPort( bgldynamic env, output_port  o )
	 {
	    env.current_error_port = o;
	 }

      public static bool OUTPUT_PORTP( Object  o )
	 {
	    return (o is output_port);
	 }

      public static bool OUTPUT_STRING_PORTP( Object  o )
	 {
	    return (o is output_string_port);
	 }

      public static bool OUTPUT_PROCEDURE_PORTP( Object  o )
	 {
	    return (o is output_procedure_port);
	 }

      public static Object FLUSH_OUTPUT_PORT( output_port p )
	 {
	    return p.flush();
	 }
    
      public static Object bgl_reset_output_string_port( output_port p )
	 {
	    return ((output_string_port)p).reset();
	 }
    
      public static Object bgl_open_output_file( byte[] file, byte[] buf )
	 {
	    if (bigloo_strcmp( file, getbytes( "null:" ) ))
	       return new output_stream_port( Stream.Null, file );
	    else
	       return new output_stream_port( file );
	 }

      public static Object bgl_append_output_file( byte[] file, byte[] buf )
	 {
	    return new output_stream_port( file, true );
	 }

      public static Object bgl_open_output_string(byte[] buf)
	 {
	    return new output_string_port();
	 }

      public static Object bgl_open_output_procedure(procedure p,
						     procedure f,
						     procedure c,
						     byte[] buf)
	 {
	    return new output_procedure_port(p, f, c);
	 }

      public static byte[] get_output_string( output_port p )
	 {
	    // CARE why not a correct signature in Scheme
	    return ((output_string_port)p).get_string();
	 }

      public static Object bgl_close_output_port( output_port p )
	 {
	    return p.close();
	 }

      public static Object display_char( int cn, output_port p )
	 {
	    p.write( cn );
	    return p;
	 }

      public static Object display_char( byte cn, output_port p )
	 {
	    p.write( cn );
	    return p;
	 }

      private static readonly String[] char_name= {
	 "", "", "", "", "", "", "", "",
	 "",  "tab", "newline", "", "", "return", "", "",
	 "", "", "", "", "", "", "", "",
	 "", "", "", "", "", "", "", "",
	 "space", "!", "\"","#","$","%","&","'",
	 "(", ")", "*", "+", ",", "-", ".", "/",
	 "0", "1", "2", "3", "4", "5", "6", "7",
	 "8", "9", ":", ";", "<", "=", ">", "?",
	 "@", "A", "B", "C", "D", "E", "F", "G",
	 "H", "I", "J", "K", "L", "M", "N", "O",
	 "P", "Q", "R", "S", "T", "U", "V", "W",
	 "X", "Y", "Z", "[", "\\", "]", "^", "_",
	 "`", "a", "b", "c", "d", "e", "f", "g",
	 "h", "i", "j", "k", "l", "m", "n", "o",
	 "p", "q", "r", "s", "t", "u", "v", "w",
	 "x", "y", "z", "{", "|", "}", "~", "" };
      
      public static Object write_char( bchar c, output_port p ) 
	 {
	    int cn= c.value & 0xFF;

	    if ((0 < cn) && (cn < 128))
	    {
	       String rep= char_name[cn];

	       if (rep.Length != 0) 
	       {
		  p.write( "#\\" );
		  p.write( rep );
		  return p;
	       }
	    }

	    p.write( "#a" );
	    p.write( ((byte)'0') + (cn / 100) );
	    p.write( ((byte)'0') + ((cn / 10) % 10) );
	    p.write( ((byte)'0') + (cn % 10) );

	    return p;
	 }

      private static readonly byte[] hexa= { (byte)'0', (byte)'1', (byte)'2', (byte)'3',
					     (byte)'4', (byte)'5', (byte)'6', (byte)'7',
					     (byte)'8', (byte)'9', (byte)'a', (byte)'b',
					     (byte)'c', (byte)'d', (byte)'e', (byte)'f' };

      public static Object write_ucs2( bucs2        s,
				       output_port p )
	 {
	    int          value= s.value;
	
	    p.write( "#u" );
	    p.write( hexa[ (value & 0xf000) >> 12 ] );
	    p.write( hexa[ (value & 0x0f00) >> 8 ] );
	    p.write( hexa[ (value & 0x00f0) >> 4 ] );
	    p.write( hexa[ (value & 0x000f) ] );
	
	    return p;
	 }

      public static Object display_ucs2( bucs2        s,
					 output_port p )
	 {
	    p.write( s.value );
	    return p;
	 }

      public static Object write_object( Object o, output_port p )
	 {
	    if (o == null)
	       p.write( "#<.NET:null>" );
	    else
	       if (o is obj)
		  ((obj)o).write( p );
	       else
		  p.write( o.ToString() );
	    return p;
	 }

      public static Object write_string( byte[] s, bool b, output_port p )
	 {
	    if (b)
	       p.write( (byte)'#' );
	    p.write( (byte)'\"' );
	    p.write( s );
	    p.write( (byte)'\"' );
	    return p;
	 }

      public static Object display_string( byte[] s, output_port p )
	 {
	    p.write( s );
	    return p;
	 }

      public static Object display_substring( byte[] s, int start, int end, output_port p )
	 {
	    p.write( s, start, end );
	    return p;
	 }

      public static Object display_fixnum( bint n, output_port p )
	 {
	    p.write( n.value.ToString() );
	    return p;
	 }

      public static Object display_flonum( real n, output_port p )
	 {
	    p.write( n.value.ToString() );
	    return p;
	 }

      public static Object display_elong( long n, output_port p )
	 {
	    p.write( n.ToString() );
	    return p;
	 }

      public static Object write_elong( long n, output_port p )
	 {
	    p.write( (byte)'#' );
	    p.write( (byte)'e' );
	    p.write( n.ToString() );
	    return p;
	 }

      public static Object write_llong( long n, output_port p )
	 {
	    p.write( (byte)'#' );
	    p.write( (byte)'l' );
	    p.write( n.ToString() );
	    return p;
	 }

      public static Object display_llong( long n, output_port p )
	 {
	    p.write( n.ToString() );
	    return p;
	 }

      public static Object bgl_write_bignum( bignum n, output_port p )
         {
	    p.write( (byte)'#' );
	    p.write( (byte)'z' );
	    p.write( n.value.ToString() );
	    return p;
	 }
      
      public static Object bgl_display_bignum( bignum n, output_port p )
         {
	    p.write( n.value.ToString() );
	    return p;
	 }

      public static Object write_utf8string( byte[] s, output_port p )
	 {
	    p.write( ucs2_string_to_utf8_string( "#u\"" + foreign.newstring( s ) + "\"" ) );
	    return p;
	 }

      public static Object display_ucs2string( char[] s, output_port p )
	 {
	    p.write( new String( s ) );
	    return p;
	 }

      //////
      // HASH
      //////
      private static readonly byte[] hash_random_table=
      { (byte)1, (byte)14, (byte)110, (byte)25, (byte)97, (byte)174, (byte)132,
        (byte)119, (byte)138, (byte)170, (byte)125, (byte)118, (byte)27,
        (byte)233, (byte)140, (byte)51,
        (byte)87, (byte)197, (byte)177, (byte)107, (byte)234, (byte)169,
        (byte)56, (byte)68, (byte)30, (byte)7, (byte)173, (byte)73, (byte)188,
        (byte)40, (byte)36, (byte)65,
        (byte)49, (byte)213, (byte)104, (byte)190, (byte)57, (byte)211,
        (byte)148, (byte)223, (byte)48, (byte)115, (byte)15, (byte)2, (byte)67,
        (byte)186, (byte)210, (byte)28,
        (byte)12, (byte)181, (byte)103, (byte)70, (byte)22, (byte)58, (byte)75,
        (byte)78, (byte)183, (byte)167, (byte)238, (byte)157, (byte)124,
        (byte)147, (byte)172, (byte)144,
        (byte)176, (byte)161, (byte)141, (byte)86, (byte)60, (byte)66, (byte)128,
        (byte)83, (byte)156, (byte)241, (byte)79, (byte)46, (byte)168, (byte)198,
        (byte)41, (byte)254,
        (byte)178, (byte)85, (byte)253, (byte)237, (byte)250, (byte)154,
        (byte)133, (byte)88, (byte)35, (byte)206, (byte)95, (byte)116,
        (byte)252, (byte)192, (byte)54, (byte)221,
        (byte)102, (byte)218, (byte)255, (byte)240, (byte)82, (byte)106,
        (byte)158, (byte)201, (byte)61, (byte)3, (byte)89, (byte)9, (byte)42,
        (byte)155, (byte)159, (byte)93,
        (byte)166, (byte)80, (byte)50, (byte)34, (byte)175, (byte)195, (byte)100,
        (byte)99, (byte)26, (byte)150, (byte)16, (byte)145, (byte)4, (byte)33,
        (byte)8, (byte)189,
        (byte)121, (byte)64, (byte)77, (byte)72, (byte)208, (byte)245, (byte)130,
        (byte)122, (byte)143, (byte)55, (byte)105, (byte)134, (byte)29,
        (byte)164, (byte)185, (byte)194,
        (byte)193, (byte)239, (byte)101, (byte)242, (byte)5, (byte)171,
        (byte)126, (byte)11, (byte)74, (byte)59, (byte)137, (byte)228,
        (byte)108, (byte)191, (byte)232, (byte)139,
        (byte)6, (byte)24, (byte)81, (byte)20, (byte)127, (byte)17, (byte)91,
        (byte)92, (byte)251, (byte)151, (byte)225, (byte)207, (byte)21,
        (byte)98, (byte)113, (byte)112,
        (byte)84, (byte)226, (byte)18, (byte)214, (byte)199, (byte)187,
        (byte)13, (byte)32, (byte)94, (byte)220, (byte)224, (byte)212,
        (byte)247, (byte)204, (byte)196, (byte)43,
        (byte)249, (byte)236, (byte)45, (byte)244, (byte)111, (byte)182,
        (byte)153, (byte)136, (byte)129, (byte)90, (byte)217, (byte)202,
        (byte)19, (byte)165, (byte)231, (byte)71,
        (byte)230, (byte)142, (byte)96, (byte)227, (byte)62, (byte)179,
        (byte)246, (byte)114, (byte)162, (byte)53, (byte)160, (byte)215,
        (byte)205, (byte)180, (byte)47, (byte)109,
        (byte)44, (byte)38, (byte)31, (byte)149, (byte)135, (byte)0,
        (byte)216, (byte)52, (byte)63, (byte)23, (byte)37, (byte)69, (byte)39,
        (byte)117, (byte)146, (byte)184,
        (byte)163, (byte)200, (byte)222, (byte)235, (byte)248, (byte)243,
        (byte)219, (byte)10, (byte)152, (byte)131, (byte)123, (byte)229,
        (byte)203, (byte)76, (byte)120, (byte)209 };

      public static int get_hash_number( byte[]  s )
	 {
	    byte hash= 0;

	    for ( int i= 0 ; i < s.Length ; ++i )
	       hash=  hash_random_table[(hash ^ s[i]) & 0xFF];
	    return (hash & 0xFF);
	 }

      public static int get_hash_power_number( byte[]  str, int power ) 
	 {
	    int result= 0;

	    for ( int i= 0 ; i < str.Length ; ++i )
	       result+= (result << 3) + str[i];
	    return (result & ((1 << power) - 1));
	 }

      public static int get_hash_number_from_int( int  i )
	 {
	    byte hash= 0;

	    while (i != 0 ) 
	    {
	       hash= hash_random_table[(int)((hash ^ i) & 0xFF)];
	       i >>= 8;
	    }
	    return(hash & 0xFF);
	 }

      public static int get_hash_number_from_int( Object  o )
	 {
	    return get_hash_number_from_int( o.GetHashCode() );
	 }

      public static int get_hash_power_number_from_int( int  i, int  power )
	 {
	    int result = 0;

	    while (i != 0) 
	    {
	       result += (result << 3) + (i & 0xFF);
	       i >>= 8;
	    }
	    return(result & ((1 << power) - 1));
	 }

      public static int get_hash_power_number_from_int( Object i, int power )
	 {
	    int hash_code= i.GetHashCode();

	    return (hash_code & ((1 << power) - 1));
	 }

      public static int bgl_string_hash_number( byte[]  s )
	 {
	    int result= 0;

	    for ( int i= 0 ; i < s.Length ; ++i )
	       result+= (result << 3) + s[i];
	    return result & ((1 << 29) - 1);
	 }
    
      public static int bgl_string_hash( byte[]  s, int start, int len )
	 {
	    int result= 0;

	    for ( int i= start ; i < len ; ++i )
	       result+= (result << 3) + s[i];
	    return result & ((1 << 29) - 1);
	 }
    
      public static int bgl_symbol_hash_number( symbol  obj )
	 {
	    return (1 + bgl_string_hash_number( SYMBOL_TO_STRING( obj ) ));
	 }
    
      public static int bgl_keyword_hash_number( keyword  obj )
	 {
	    return (2 + bgl_string_hash_number( KEYWORD_TO_STRING( obj ) ));
	 }
    
      public static int bgl_obj_hash_number( Object  obj )
	 {
	    return obj.GetHashCode();
	 }

      public static int bgl_foreign_hash_number( Object  obj )
	 {
	    return obj.GetHashCode();
	 }

      public static int bgl_pointer_hash_number( Object  obj, int power )
	 {
	    return obj.GetHashCode() % power;
	 }

      public static int bgl_elong_hash_number(long n)
	 {
	    return (int)n;
	 }

      public static int bgl_llong_hash_number(long n)
	 {
	    return ((int)n);
	 }

      public static byte[] bgl_double_to_ieee_string( double  v )
	 {
	    return foreign.getbytes( v.ToString() );
	 }
      public static byte[] bgl_float_to_ieee_string( float  v )
	 {
	    return foreign.getbytes( v.ToString() );
	 }

      public static double bgl_ieee_string_to_double( byte[]  s )
	 {
	    return Double.Parse( newstring( s ) );
	 }
       public static float bgl_ieee_string_to_float( byte[]  s )
	 {
	    return (float)(Double.Parse( newstring( s ) ));
	 }

      public static int rand() 
	 {
	    return randg.Next();
	 }

      public static bignum bgl_rand_bignum( bignum bx )
         {
	    return bx.rand(randg);
	 }

      public static void srand( int seed )
	 {
	    randg = new Random( seed );
	 }

      public static void bgl_sleep( int  microsecs )
	 {
	    Thread.Sleep( (int)(microsecs / 1000) );
	 }

      // CARE not implemented yet
      public static byte[] bgl_gethostname() {
	 Console.Out.WriteLine( "bgl_gethostname(runtime/CSlib/foreign.cs) not implemented" );
	 return getbytes( "localhost" );
      }

      //////
      // MUTEX and CONDITION VARIABLE
      //////
      public static bool BGL_MUTEXP(Object o)
	 {
	    return (o is mutex);
	 }

      public static mutex bgl_make_mutex(Object o) {
	 return mutex.make(o);
      }
   
      public static mutex bgl_make_nil_mutex() {
	 return bigloo.mutex.nil_mutex;
      }

      public static mutex bigloo_generic_mutex = bgl_make_mutex( BFALSE);
      
      public static Object BGL_MUTEX_NAME(mutex o)
	 {
	    return o.name;
	 }

      public static bool bgl_mutex_lock(mutex o)
	 {
	    return o.acquire_lock();
	 }
   
      public static bool bgl_mutex_timed_lock(mutex o, int tmt)
	 {
	    return o.acquire_timed_lock(tmt);
	 }
   
      public static bool bgl_mutex_unlock(mutex o)
	 {
	    return o.release_lock();
	 }
   
      public static Object bgl_mutex_state(mutex o)
	 {
	    return o.state();
	 }
   
      public static bool BGL_CONDVARP(Object o)
	 {
	    return (o is condvar);
	 }
   
      public static condvar bgl_make_condvar(Object o) {
	 return condvar.make(o);
      }
   
      public static condvar bgl_make_nil_condvar() {
	 return bigloo.condvar.nil_condvar;
      }
   
      public static Object BGL_CONDVAR_NAME(condvar o)
	 {
	    return o.name;
	 }

      public static bool bgl_condvar_wait(condvar c, mutex o) {
	 lock( c ) {
	    try
	    {
	       bgl_mutex_unlock( o );
	       Monitor.Wait(c);
	       bgl_mutex_lock( o );
	       return true;
	    }
	    catch(Exception)
	    {
	       return false;
	    }
	 }
      }

      public static bool bgl_condvar_timed_wait(condvar c, mutex o, int ms) {
	 lock( c ) {
	    try
	    {
	       bool res;
	       bgl_mutex_unlock( o );
	       res = Monitor.Wait(c, ms);
	       bgl_mutex_lock( o );
	       return res;
	    }
	    catch(Exception)
	    {
	       return false;
	    }
	 }
      }

      public static bool bgl_condvar_broadcast(condvar c) {
	 lock( c ) {
	    Monitor.PulseAll(c);
	 }

	 return true;
      }
   
      public static bool bgl_condvar_signal(condvar c) {
	 lock( c ) {
	    Monitor.Pulse(c);
	 }
      
	 return true;
      }
      public static bool BGL_MMAPP( Object o ) {
	 return (o is mmap);
      }

      public static mmap bgl_open_mmap( byte[] fname, bool r, bool w ) {
	 return new mmap( fname, r, w );
      }

      public static mmap bgl_string_to_mmap( byte[] s, bool r, bool w ) {
	 return new mmaps( s, r, w );
      }
      
      public static Object bgl_close_mmap( mmap o ) {
	 o.close();
	 return BTRUE;
      }

      public static Object bgl_sync_mmap( mmap o ) {
	 return o;
      }

      public static int BGL_MMAP_REF( mmap o, long i ) {
	 return o.get( i );
      }
   
      public static Object BGL_MMAP_SET( mmap o, long i, int c ) {
	 o.put( i, c );
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
	 o.br.Seek( i, 0 );
      }
   
      public static long BGL_MMAP_WP_GET( mmap o ) {
	 return o.wp;
      }
   
      public static void BGL_MMAP_WP_SET( mmap o, long i ) {
	 o.wp = i;
	 o.bw.Seek( (long)i, 0 );
      }
   }
}
