package bigloo;

public class u8vector extends hvector {
   byte[] objs;
   
   public u8vector( int l ) {
      len = l;
      objs = new byte[ l ];
   }
   
   public int ident() {
      return 1;
   }
}
