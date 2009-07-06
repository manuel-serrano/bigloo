package bigloo;

public class s32vector extends hvector {
   int[] objs;
   
   public s32vector( int l ) {
      len = l;
      objs = new int[ l ];
   }
   
   public int ident() {
      return 4;
   }
}
