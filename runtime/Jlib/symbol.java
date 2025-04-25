package bigloo;

import java.util.*;

public class symbol extends obj
{
   static final class bucket
   {
      public final symbol symb;
      public boolean marked = false;
      public bucket next;

      public bucket(final symbol symb, final bucket next) {
	 this.symb = symb;
	 this.next = next;
      }
   }

   public final byte[] string;
   public Object cval;

   public static int shift = 12;
   public static bucket[] table = new bucket[1 << shift];
   public static int cnt = 0;

   public symbol(final Object cval, final byte[] string) {
      this.cval = cval;
      this.string = string;
   }

   public static boolean exists(final byte[] name) {
      synchronized(table) {
	 final int hash_number = foreign.get_hash_power_number(name, shift);
	 final bucket bucket = table[hash_number];

	 if (bucket == null)
	    return false;

	 for (bucket run = bucket; (run != null); run = run.next)
	    if (foreign.bigloo_strcmp(run.symb.string, name))
	       return true;

	 return false;
      }
   }

   public static bucket[] resize_table(bucket[] old_table, int new_shift) {
      bucket[] new_table = new bucket[1 << new_shift];
      int old_size = old_table.length;
      int i;

      for (i = 0; i < old_size; i++) {
	 for (bucket run = old_table[i]; (run != null) ; run = run.next) {
	    symbol symb = run.symb;
	    int hash = foreign.get_hash_power_number(symb.string, new_shift);
	    new_table[hash] = new bucket(symb, new_table[hash]);
	 }
      }
      
      return new_table;
   }
   
   public static symbol make_symbol(final byte[] name) {
      synchronized(table) {
	 final int hash_number = foreign.get_hash_power_number(name, shift);
	 final bucket bucket = table[hash_number];

	 if (bucket == null) {
	    final symbol symbol = new symbol(nil.nil, name);

	    table[hash_number] = new bucket(symbol, null);
	    return symbol;
	 }

	 for (bucket run = bucket; (run != null) ; run = run.next)
	    if (foreign.bigloo_strcmp(run.symb.string, name))
	       return run.symb;

	 if (cnt++ > (1 << shift) * 4) {
	    shift++;
	    table = resize_table(table, shift);
	 } else {
	    final symbol result = new symbol(nil.nil, name);
	    table[hash_number] = new bucket(result, bucket);
	    return result;
	 }
      }
      // this point is reached when the table has been resized
      return make_symbol(name);
   }

   public void write(final output_port p) {
      p.write(string);
   }
}
