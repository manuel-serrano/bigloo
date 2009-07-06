using System;

namespace bigloo
{
   public class u16vector: hvector {
      public ushort[] objs;
   
      public u16vector( int l ) {
	 len = l;
	 objs = new ushort[ l ];
      }
   
      public override int ident() {
	 return 3;
      }
   }
}
