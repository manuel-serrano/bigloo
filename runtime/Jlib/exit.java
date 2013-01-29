package bigloo;

public class exit {
   public static int count= 0;
   public int userp;
   public int stamp= count++;
   public exit prev;
   public Object mutex0;
   public Object mutex1;
   public Object mutexn;
   public Object protect0;
   public Object protect1;
   public Object protectn;

   public exit() {
      mutex0 = foreign.BFALSE;
      mutex1 = foreign.BFALSE;
      mutexn = foreign.BNIL;
      protect0 = foreign.BFALSE;
      protect1 = foreign.BFALSE;
      protectn = foreign.BNIL;
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
