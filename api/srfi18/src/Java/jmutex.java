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
import java.util.concurrent.TimeUnit;

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
       
       int res = 1;
       try {

           if (ms > 0) {
               if( mutex.tryLock(ms, TimeUnit.MILLISECONDS) ) {
                   res = 0;
               }
           } else {
               mutex.lock();
               res = 0;       
           }

           /* if we have acquired the lock, mark it owned */
           if (res == 0) {
                /* We synchronize on this to prevent a race condition between 1
                   thread unlocking the mutex and another locking it and their
                   subsequent updating of thread and state */
               synchronized (this) {
                   /* mark mutex owned */
                   thread = jthread.current_thread();
                   state = null;
               }
           }      
       } catch( Exception e ) {
           foreign.fail( "mutex-lock!", 
                         e.getClass() + ": " + e.getMessage(),
                         this );
       }
       return res;
   }

   public int release_lock() {
     /* We synchronize on this to prevent a race condition between 1
        thread unlocking the mutex and another locking it and their
        subsequent updating of thread and state */
       synchronized (this) {
           mutex.unlock();
           /* mark mutex no longer owned */
           thread = null;
           state = "not-abandoned";
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
