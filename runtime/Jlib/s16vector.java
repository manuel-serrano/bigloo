package bigloo;

public class s16vector extends hvector {
   short[] objs;
   
   public s16vector( int l ) {
      len = l;
      objs = new short[ l ];
   }
   
   public int ident() {
      return 2;
   }
}
