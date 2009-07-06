using System;

namespace bigloo
{
   public class u32vector: hvector {
      public uint[] objs;
   
      public u32vector( int l ) {
	 len = l;
	 objs = new uint[ l ];
      }
   
      public override int ident() {
	 return 5;
      }
   }
}
