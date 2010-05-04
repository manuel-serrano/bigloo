using System;
using System.IO;

namespace bigloo {
   public class mmaps : mmap {
      private bool reada, writea;

      public mmaps( byte[] s, bool r, bool w ) : base( s ) {
	 reada = r;
	 writea = w;
      }

      public override int get( long i ) {
	 if( reada ) {
	    return name[ i ];
	 } else {
	    foreign.fail( "mmap", "write only mmap", this );
	    return -1;
	 }
      }

      public override void put( long i, int c ) {
	 if( writea )
	    name[ i ] = (byte)c;
	 else
	    foreign.fail( "mmap", "read only mmap", this );
      }

      public override void close() {
	 ;
      }
   }
}
      
      
