package bigloo;

public class s64vector extends hvector {
   long[] objs;
   
   public s64vector( int l ) {
      len = l;
      objs = new long[ l ];
   }
   
   public int ident() {
      return 6;
   }
}
