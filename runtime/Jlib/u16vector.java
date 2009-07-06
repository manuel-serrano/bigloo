package bigloo;

public class u16vector extends hvector {
   short[] objs;
   
   public u16vector( int l ) {
      len = l;
      objs = new short[ l ];
   }
   
   public int ident() {
      return 3;
   }
}
