package bigloo;

public class s8vector extends hvector {
   byte[] objs;
   
   public s8vector( int l ) {
      len = l;
      objs = new byte[ l ];
   }
   
   public int ident() {
      return 0;
   }
}
