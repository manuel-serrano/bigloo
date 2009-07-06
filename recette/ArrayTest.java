public class ArrayTest {
   int[] t;
   
   public ArrayTest( int[] tab ) {
      t = tab;
   }
   
   public static int hello( int[] tab ) {
      return tab.length + tab[ 0 ] + tab[ 1 ];
   }
}
