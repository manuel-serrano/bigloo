package bigloo;

import java.util.*;

public class keyword extends obj
{
   static final class keybucket {
      public final keyword symb;
      public boolean marked = false;
      public final keybucket next;

      public keybucket( final keyword symb, final keybucket next ) {
	 this.symb = symb;
	 this.next = next;
      }
   }

   public byte[] string;
   public Object cval;

   public static final keybucket[] table = new keybucket[4096];

   public keyword( final Object cval, final byte[] string ) {
      this.cval = cval;
      this.string = string;
   }

   public static boolean exists( final byte[] name ) {
      final int hash_number = foreign.get_hash_power_number( name, 12 );
      final keybucket keybucket = table[hash_number];

      if (keybucket == null)
	 return false;

      for (keybucket run = keybucket ; (run != null) ; run = run.next)
	 if (foreign.bigloo_strcmp( run.symb.string, name ))
	    return true;

      return false;
   }

   public static keyword make_keyword( final byte[] name ) {
      synchronized( table ) {
	 final int hash_number = foreign.get_hash_power_number( name, 12 );
	 final keybucket keybucket = table[hash_number];

	 if (keybucket == null) {
	    final keyword symbol = new keyword( nil.nil, name );

	    table[hash_number] = new keybucket( symbol, null );
	    return symbol;
	 }

	 for (keybucket run = keybucket; (run != null); run = run.next)
	    if (foreign.bigloo_strcmp( run.symb.string, name ))
	       return run.symb;

	 final keyword symbol = new keyword( nil.nil, name );

	 table[hash_number] = new keybucket( symbol, keybucket );

	 return symbol;
      }
   }

   public static keyword make_keyword( String name ) {
      return make_keyword( name.getBytes() );
   }
   
   public void write( final output_port  p ) {
      p.write( string );
   }
}
