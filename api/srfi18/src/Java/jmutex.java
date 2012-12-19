/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/srfi18/src/Java/jmutex.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Wed Dec 19 10:52:05 2012 (serrano)                */
/*    Copyright   :  2005-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Mutex implementation                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.srfi18;
import java.lang.*;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    jmutex                                                           */
/*---------------------------------------------------------------------*/
public class jmutex extends bigloo.mutex {
   private static Object mutexes = bigloo.foreign.BNIL;
   
   protected static void setup() {
      bigloo.mutex.amutex = new jmutex( bigloo.foreign.BUNSPEC );
   }

   protected Object thread = null;
   protected String state = "not-abandoned";
   
   private Object specific;

   public jmutex( Object n ) {
      super( n );
   }

   protected static void mutexes_unlock( Object thread ) {
      Object w = mutexes;

      while( w instanceof pair ) {
	 jmutex m = (jmutex)(foreign.CAR( (pair)w ));
	 
	 if( m.thread == thread ) {
	    m.release_lock();
	    m.state = "abandoned";
	 }
	 w = foreign.CDR( (pair)w );
      }
   }

   public int acquire_lock( int ms ) {
      synchronized( this ) {
	 Object th = jthread.current_thread();
      
	 while( thread != null && thread != th ) {
	    try {
	       if( ms > 0 ) {
		  wait( ms );
		  if( thread != null && thread != th ) return 1;
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
	 
	 return 0;
      }
   }

   public int acquire_lock() {
      return acquire_lock( 0 );
   }
   
   public int acquire_timed_lock( int ms ) {
      return acquire_lock( ms );
   }

   public int release_lock() {
      synchronized( this ) {
	 Object th = jthread.current_thread();
	 
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
      return 0;
   }

   public static Object SPECIFIC( Object m ) {
      if( m instanceof jmutex )
	 return ((jmutex) m).specific;
      else
	 return bigloo.foreign.BUNSPEC;
   }

   public static void SPECIFIC_SET( Object m, Object s ) {
      ((jmutex) m).specific = s;
   }

   public Object state() {
      return state == null ? thread : bigloo.foreign.string_to_symbol( state );
   }

   public static bigloo.mutex srfi18_make_jmutex( Object o ) {
      return bigloo.foreign.bgl_make_mutex( o );
   }
}
