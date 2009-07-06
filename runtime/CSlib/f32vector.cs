using System;

namespace bigloo
{
   public class f32vector: hvector {
      public float[] objs;
   
      public f32vector( int l ) {
	 len = l;
	 objs = new float[ l ];
      }
   
      public override int ident() {
	 return 8;
      }
   }
}
