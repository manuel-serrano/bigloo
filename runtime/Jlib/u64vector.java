package bigloo;

public class u64vector extends hvector {
   long[] objs;
   
   public u64vector( int l ) {
      len = l;
      objs = new long[ l ];
   }
   
   public int ident() {
      return 7;
   }
}
