using System;

namespace bigloo
{
   public class f64vector: hvector {
      public double[] objs;
   
      public f64vector( int l ) {
	 len = l;
	 objs = new double[ l ];
      }
   
      public override int ident() {
	 return 9;
      }
   }
}
