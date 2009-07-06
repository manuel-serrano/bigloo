using System;

namespace bigloo
{
   public class s32vector: hvector {
      public int[] objs;
   
      public s32vector( int l ) {
	 len = l;
	 objs = new int[ l ];
      }
   
      public override int ident() {
	 return 4;
      }
   }
}
