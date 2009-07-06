package bigloo;

public class f32vector extends hvector {
   float[] objs;
   
   public f32vector( int l ) {
      len = l;
      objs = new float[ l ];
   }
   
   public int ident() {
      return 8;
   }
}
