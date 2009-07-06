using System;

namespace bigloo
{
   public class s16vector: hvector {
      public short[] objs;
   
      public s16vector( int l ) {
	 len = l;
	 objs = new short[ l ];
      }
   
      public override int ident() {
	 return 2;
      }
   }
}
