/*=====================================================================*/
/*    /tmp/BGL2/bigloo-unstable/api/srfi18/src/Java/jmutex.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Mon Dec 18 21:58:05 2023 (serrano)                */
/*    Copyright   :  2005-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Mutex implementation                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.srfi18;
import java.lang.*;
import bigloo.*;
import bigloo.pthread.*;

/*---------------------------------------------------------------------*/
/*    jmutex                                                           */
/*---------------------------------------------------------------------*/
public class jmutex extends bigloo.pthread.bglpmutex {
   
   protected static void setup() {
      bigloo.mutex.amutex = new jmutex( bigloo.foreign.BUNSPEC );
   }
   
   private Object specific;

   public jmutex( Object n ) {
      super( n );
      state = "not-abandoned";
   }

   protected bigloo.mutex create( Object name ) {
     bigloo.mutex m = new jmutex( name );
      
     mutexes = foreign.MAKE_PAIR( m, mutexes );
      
     return m;
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

      thread = null;
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

   public int release_lock() {
      Object th = jthread.current_thread();
      if( thread == th ) {
	 /* mark mutex no longer owned */
	 try {
	    super.release_lock();
	    // notifyAll();
	 } catch (java.lang.IllegalMonitorStateException e) {
	    ;
	 }
      } else {
	 foreign.fail( "mutex-unlock!",
		       "mutex not owned by thread",
		       this );
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
