package bigloo;

import java.util.*;

public class mutex extends obj
{
   public final Object name;
   public final Object backend;
   public static final mutex nil_mutex = new mutex();
   public static mutex amutex = new mutex( foreign.BUNSPEC );
   
   public mutex() {
      this.name = bigloo.foreign.BUNSPEC;
      this.backend = "pthread".getBytes();
   }

   public mutex( final Object n ) {
      this.name = n;
      this.backend = "pthread".getBytes();
   }

   protected mutex create( Object name ) {
      return new mutex( name );
   }

   public static mutex make( Object name ) {
      return amutex.create( name );
   }
   
   public void write( final output_port p ) {
     p.write( "#<mutex>" );
   }

   public int acquire_lock() {
      return 0;
   }
   
   public int acquire_timed_lock( int ms ) {
      return 0;
   }
   
   public int release_lock() {
      return 0;
   }

   public Object state() {
      return bigloo.foreign.BUNSPEC;
   }
}
