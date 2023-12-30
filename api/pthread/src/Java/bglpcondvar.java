/*=====================================================================*/
/*    .../bigloo/bigloo/api/pthread/src/Java/bglpcondvar.java          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Wed Dec 20 12:51:39 2023 (serrano)                */
/*    Copyright   :  2005-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Condvar implementation                                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.pthread;
import java.lang.*;
import java.util.*;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    bglpcondvar                                                      */
/*---------------------------------------------------------------------*/
public class bglpcondvar extends bigloo.condvar {
   
   protected static void setup() {
      bigloo.condvar.acondvar = new bglpcondvar( bigloo.foreign.BUNSPEC );
   }
   private Object specific;
   
   public bglpcondvar( Object n ) {
      super( n );
   }

   protected condvar create( Object name ) {
      return new bglpcondvar( name );
   }
   
   public static Object SPECIFIC( Object o ) {
      return ((bglpcondvar) o).specific;
   }
   
   public static void SPECIFIC_SET( Object o, Object s ) {
      ((bglpcondvar) o).specific = s;
   }

    /* This implementation of wait with timeout is inspired by the Timed Waits section of
       Doug Lea's Concurrent Programming in Java 2nd Edition pages 194-195 */
   public boolean wait( final bglpmutex m, final int ms ) {
       boolean res = true;
       boolean interrupted = false;
       
          /* make sure we have successfully released the mutex m and are
             waiting on the condition before a signal or broadcast can be
             made. Also, Note the monitor associated with the this
             variable must be locked for the call to this.wait to work
             correctly.  See
             https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#wait--
          */
          synchronized( this ) {
              Object th = bglpthread.current_thread();
       
              /* assertion */
              if( m.thread != th) {
                  foreign.fail( "condition-variable-wait!",
                                "mutex not owned by current thread",
                                m );
              }

              /* The semantics of condition-variable-wait! require m be unlocked
                 while waiting, */
              m.release_lock();
          
              /* track wait time when needed */
              long wait = ms;
              long start = System.currentTimeMillis();
          
              while (true) {
                  
                  // Is it a timed wait and have we timed out?
                  //If so, indicate it and exit. 
                  if (ms > 0 && wait <= 0) {
                      res = false;
                      break; 
                  }
              
                  /* wait to be signaled */
                  try {          
                      if( ms > 0 ) {
                          wait( ms );
                      } else {
                          wait();
                      }
                      
                  } catch( Exception e ) {
                      if (e instanceof InterruptedException) {
                          interrupted = true;
                      }
                      res = false;
                      notify(); 
                      break;
                  }

                  /* If we are waiting with timeout, update remaining time.*/
                  if (ms > 0) {
                      long now = System.currentTimeMillis();
                      wait = ms - (now - start);
                  } else {
                      // we have waited successfully and can return
                      res = true;
                      break;
                  }
              }
          }

          /* The semantics of condition-variable-wait! require m to be
          locked on return. */
          m.acquire_lock();

          // if we were interrupted make sure we pass on the fact.
          if (interrupted) {
              Thread.currentThread().interrupt();
          }
       
       return res;
   }

      
   public boolean wait( final mutex o, final int ms ) {
      if( !(o instanceof bglpmutex) ) {
	 bigloo.foreign.fail( "condition-variable-wait!",
			      "Not a pmutex (library badly initialized)",
			      o );
	 return false;
      } else {
	 return wait( (bglpmutex)o, ms );
      }
   }


   public boolean wait( mutex o ) {
      return wait( o, 0 );
   }
   
   public boolean timed_wait( mutex o, int ms ) {
      return wait( o, ms );
   }

   public synchronized boolean cond_signal(boolean b) {     
	 if( b ) {
            notifyAll();
	 } else {
	    notify();
	 }
	 return true;
   }
      
   public boolean broadcast() {
      return cond_signal( true );
   }
   
   public boolean signal() {
      return cond_signal( false );
   }

   public static condvar bglpth_make_condvar( Object o ) {
      return bigloo.foreign.bgl_make_condvar( o );
   }
}
