package bigloo;

public class u32vector extends hvector {
   int[] objs;
   
   public u32vector( int l ) {
      len = l;
      objs = new int[ l ];
   }
   
   public int ident() {
      return 5;
   }
}
