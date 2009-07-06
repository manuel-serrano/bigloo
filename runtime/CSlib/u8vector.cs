using System;

namespace bigloo
{
   public class u8vector: hvector {
      public byte[] objs;
   
      public u8vector( int l ) {
	 len = l;
	 objs = new byte[ l ];
      }
   
      public override int ident() {
	 return 1;
      }
   }
}
