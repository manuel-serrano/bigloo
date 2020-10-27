/*=====================================================================*/
/*    .../project/bigloo/api/pthread/src/Java/bglpcondvar.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Fri Nov 23 17:38:07 2012 (serrano)                */
/*    Copyright   :  2005-12 Manuel Serrano                            */
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
   ArrayList alist = new ArrayList();
   
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

   public boolean wait( final bglpmutex m, final int ms ) {
      boolean res = true;
      
      synchronized( this ) {
	 Object th = bglpthread.current_thread();

	 /* assertion */
	 if( m.thread != th ) {
	    foreign.fail( "condition-variable-wait!",
			  "mutex not owned by current thread",
			  m );
	 }

	 /* release the lock */
	 m.release_lock();
	 
	 try {
	    if( ms > 0 ) {
	       alist.add( th );
	       wait( ms );
	       if( alist.contains( th ) ) {
		  res = false;
		  alist.remove( th );
	       }
	    } else {
	       wait();
	    }
	 } catch( Exception e ) {
            // the semantics of condition-variable-wait! requires the mutex
            // be locked regardless of whether or not the wait was successful
             m.acquire_lock(); 
	    return false;
	 }
      }
      
      /* release the condvar and re-acquire the lock */
      m.acquire_lock();
      
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

   public boolean cond_signal(boolean b) {
      synchronized( this ) {

	 if( b ) {
	    alist.clear();
	    notifyAll();
	 } else {
	    if( alist.size() > 0 ) alist.remove( 0 );
	    
	    notify();
	 }
	 
	 return true;
      }
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
