package bigloo;

public class f64vector extends hvector {
   double[] objs;
   
   public f64vector( int l ) {
      len = l;
      objs = new double[ l ];
   }
   
   public int ident() {
      return 9;
   }
}
