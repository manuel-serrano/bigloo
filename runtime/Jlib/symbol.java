package bigloo;

import java.util.*;

public class symbol extends obj
{
   static final class bucket
   {
      public final symbol symb;
      public boolean marked = false;
      public bucket next;

      public bucket( final symbol symb, final bucket next ) {
	 this.symb = symb;
	 this.next = next;
      }
   }

   public final byte[] string;
   public Object cval;

   public static final bucket[] table = new bucket[4096];

   public symbol( final Object cval, final byte[] string ) {
      this.cval = cval;
      this.string = string;
   }

   public static boolean exists( final byte[] name ) {
      synchronized( table ) {
	 final int hash_number = foreign.get_hash_power_number( name, 12 );
	 final bucket bucket = table[hash_number];

	 if (bucket == null)
	    return false;

	 for (bucket run = bucket; (run != null); run = run.next)
	    if (foreign.bigloo_strcmp( run.symb.string, name ))
	       return true;

	 return false;
      }
   }

   public static symbol make_symbol( final byte[] name ) {
      synchronized( table ) {
	 final int hash_number = foreign.get_hash_power_number( name, 12 );
	 final bucket bucket = table[hash_number];

	 if (bucket == null) {
	    final symbol symbol = new symbol( nil.nil, name );

	    table[hash_number] = new bucket( symbol, null );
	    return symbol;
	 }

	 for (bucket run = bucket; (run != null) ; run = run.next)
	    if (foreign.bigloo_strcmp( run.symb.string, name ))
	       return run.symb;

	 final symbol result = new symbol( nil.nil, name );

	 table[hash_number] = new bucket( result, bucket );

	 return result;
      }
   }

   public void write( final output_port p ) {
      p.write( string );
   }
}
