/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/Java/bglpmutex.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Fri Jun 18 11:34:45 2010 (serrano)                */
/*    Copyright   :  2005-10 Manuel Serrano                            */
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
   protected bglpcondvar condv = null;
   private String state = "not-abandoned";
   
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
   
   public synchronized boolean acquire_lock( int ms ) {
      Object th = bglpthread.current_thread();
      
      synchronized( this ) {
	 bglpthread.debug( ">>> acquire_lock m=" + this + " thread=" + thread + " th=" + th );

	 while( thread != null ) {
	    try {
	       if( ms > 0 ) {
		  wait( ms );
		  if( thread != null ) return false;
	       } else {
		  wait();
	       }
	    } catch( Exception e ) {
	       foreign.fail( "mutex-lock!", 
			     e.getClass() + ": " + e.getMessage(),
			     this );
	    }
	 }
	 
	 thread = th;
	 state = null;
	 bglpthread.debug( "<<< acquire_lock m=" + this + " thread=" + thread + " th=" + th );
	 return true;
      }
   }

   public synchronized boolean acquire_lock() {
      return acquire_lock( 0 );
   }
   
   public synchronized boolean acquire_timed_lock( int ms ) {
      return acquire_lock( ms );
   }

   public synchronized boolean release_lock() {
      Object th = bglpthread.current_thread();
      
      synchronized( this ) {
	 bglpthread.debug( "relase_lock m=" + this + " thread=" + thread + " th=" + bglpthread.current_thread() );

	 /* assertion */
	 if( thread != th ) {
	    foreign.fail( "mutex-unlock!",
			  "mutex not owned by current thread",
			  this );
	 }
	 thread = null;
	 state = "not-abandoned";
	 notify();
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
