using System;

namespace bigloo
{
   public class s64vector: hvector {
      public long[] objs;
   
      public s64vector( int l ) {
	 len = l;
	 objs = new long[ l ];
      }
   
      public override int ident() {
	 return 6;
      }
   }
}
