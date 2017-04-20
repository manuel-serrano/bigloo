package bigloo;

import java.util.*;

public class semaphore extends obj {
   public final Object name;
   
   public semaphore() {
      this.name = bigloo.foreign.BUNSPEC;
   }

   public semaphore( final Object n ) {
      this.name = n;
   }

   public void write( final output_port p ) {
     p.write( "#<semaphore>" );
   }
}
