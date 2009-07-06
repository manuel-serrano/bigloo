using System;

namespace bigloo {
   public sealed class keyword: obj {
      private sealed class keybucket {
	 public readonly keyword symb;
	 public readonly keybucket next;
	 public bool marked = false;

	 public keybucket( keyword symb, keybucket next ) {
	    this.symb = symb;
	    this.next = next;
	 }
      }

      public readonly byte[] pname;
      public Object cval;
      private static readonly keybucket[] table = new keybucket[4096];

      public keyword( Object cval, byte[] pname ) {
	 this.cval = cval;
	 this.pname = pname;
      }

      public static bool exists( byte[] name ) {
	 int hash_number = foreign.get_hash_power_number( name, 12 );
	 keybucket _keybucket = table[hash_number];

	 if (_keybucket == null)
	    return false;

	 for (keybucket run = _keybucket ; (run != null) ; run = run.next)
	    if (foreign.bigloo_strcmp( run.symb.pname, name ))
	       return true;

	 return false;
      }

      public static keyword make_keyword( byte[] name ) {
	 lock( table ) {
	    int hash_number = foreign.get_hash_power_number( name, 12 );
	    keybucket _keybucket = table[hash_number];

	    if (_keybucket == null) {
	       keyword symbol = new keyword( nil._nil, name );

	       table[hash_number] = new keybucket( symbol, null );
	       return symbol;
	    }

	    for (keybucket run = _keybucket ; (run != null) ; run = run.next)
	       if (foreign.bigloo_strcmp( run.symb.pname, name ))
		  return run.symb;

	    keyword result = new keyword( nil._nil, name );

	    table[hash_number] = new keybucket( result, _keybucket );

	    return result;
	 }
      }

      public override void write( output_port p ) {
	 p.write( pname );
      }
   }
}
