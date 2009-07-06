using System;

namespace bigloo
{
   public class s8vector: hvector {
      public sbyte[] objs;
   
      public s8vector( int l ) {
	 len = l;
	 objs = new sbyte[ l ];
      }
   
      public override int ident() {
	 return 0;
      }
   }
}
