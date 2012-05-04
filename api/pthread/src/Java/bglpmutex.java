/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Java/bglpmutex.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Fri May  4 21:14:00 2012 (serrano)                */
/*    Copyright   :  2005-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Mutex implementation                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.pthread;
import java.lang.*;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    bglpmutex                                                        */
/*---------------------------------------------------------------------*/
public class bglpmutex extends bigloo.mutex {
   private static Object mutexes = bigloo.foreign.BNIL;
   
   protected static void setup() {
      bigloo.mutex.amutex = new bglpmutex( bigloo.foreign.BUNSPEC );
   }

   protected Object thread = null;
   protected String state = "not-abandoned";
   
   private Object specific;

   public bglpmutex( Object n ) {
      super( n );
   }

   protected mutex create( Object name ) {
      mutex m = new bglpmutex( name );

      mutexes = foreign.MAKE_PAIR( m, mutexes );
      
      return m;
   }

   protected static void mutexes_unlock( Object thread ) {
      Object w = mutexes;

      while( w instanceof pair ) {
	 bglpmutex m = (bglpmutex)(foreign.CAR( (pair)w ));
	 
	 if( m.thread == thread ) {
	    m.release_lock();
	    m.state = "abandoned";
	 }
	 w = foreign.CDR( (pair)w );
      }
   }

   public boolean acquire_lock( int ms ) {
      synchronized( this ) {
	 Object th = bglpthread.current_thread();
      
	 while( thread != null && thread != th ) {
	    try {
	       if( ms > 0 ) {
		  wait( ms );
		  if( thread != null && thread != th ) return false;
	       } else {
		  wait();
	       }
	    } catch( Exception e ) {
	       foreign.fail( "mutex-lock!", 
			     e.getClass() + ": " + e.getMessage(),
			     this );
	    }
	 }

	 /* mark mutex owned */
	 thread = th;
	 state = null;
	 
	 return true;
      }
   }

   public boolean acquire_lock() {
      return acquire_lock( 0 );
   }
   
   public boolean acquire_timed_lock( int ms ) {
      return acquire_lock( ms );
   }

   public boolean release_lock() {
      synchronized( this ) {
	 Object th = bglpthread.current_thread();
	 
	 if( thread == th ) {
	    /* mark mutex no longer owned */
	    thread = null;
	    state = "not-abandoned";
	 
	    notifyAll();
	 } else {
	   foreign.fail( "mutex-unlock!",
			 "mutex not owned by thread",
			 this );
	 }
      }
      return true;
   }

   public static Object SPECIFIC( Object m ) {
      if( m instanceof bglpmutex )
	 return ((bglpmutex) m).specific;
      else
	 return bigloo.foreign.BUNSPEC;
   }

   public static void SPECIFIC_SET( Object m, Object s ) {
      ((bglpmutex) m).specific = s;
   }

   public Object state() {
      return state == null ? thread : bigloo.foreign.string_to_symbol( state );
   }
}
