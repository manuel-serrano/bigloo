using System;

namespace bigloo
{
   public class u64vector: hvector {
      public ulong[] objs;
   
      public u64vector( int l ) {
	 len = l;
	 objs = new ulong[ l ];
      }
   
      public override int ident() {
	 return 7;
      }
   }
}
