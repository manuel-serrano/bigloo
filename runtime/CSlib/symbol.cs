using System;

namespace bigloo {
   public sealed class symbol: obj {
      sealed class bucket {
	 public readonly symbol symb;
	 public bool marked = false;
	 public bucket next;
   
	 public bucket( symbol symb, bucket next ) {
	    this.symb = symb;
	    this.next = next;
	 }
      }

      public readonly byte[] pname;
      public Object cval;

      private static readonly bucket[] table = new bucket[4096];

      public symbol( Object cval, byte[] pname ) {
	 this.cval = cval;
	 this.pname = pname;
      }

      public static bool exists( byte[] name ) {
	 int hash_number = foreign.get_hash_power_number( name, 12 );
	 bucket _bucket = table[hash_number];

	 if (_bucket == null)
	    return false;

	 for (bucket run = _bucket ; (run != null) ; run = run.next)
	    if (foreign.bigloo_strcmp( run.symb.pname, name ))
	       return true;

	 return false;
      }

      public static symbol make_symbol( byte[] name ) {
	 lock( table ) {
	    int hash_number = foreign.get_hash_power_number( name, 12 );
	    bucket _bucket = table[hash_number];

	    if (_bucket == null) {
	       symbol _symbol = new symbol( nil._nil, name );

	       table[hash_number] = new bucket( _symbol, null );
	       return _symbol;
	    }

	    for (bucket run = _bucket ; (run != null) ; run = run.next)
	       if (foreign.bigloo_strcmp( run.symb.pname, name ))
		  return run.symb;

	    symbol result = new symbol( nil._nil, name );

	    table[hash_number] = new bucket( result, _bucket );

	    return result;
	 }
      }

      public override void write( output_port p ) {
	 p.write( pname );
      }
   }
}

