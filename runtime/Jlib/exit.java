package bigloo;

public class exit {
   public static int count= 0;
   public int userp;
   public int stamp= count++;
   public exit prev;
   public Object protect;

   public exit() {
      protect = foreign.BNIL;
   }

   public void write( final output_port p ) {
      p.write( "<exit" );
      for ( exit i= this ; i != null ; i= i.prev ) {
	 p.write( " " );
	 p.write( i.stamp );
      }
      p.write( ">" );
   }
}
