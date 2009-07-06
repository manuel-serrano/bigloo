/*=====================================================================*/
/*    .../project/bigloo/api/pthread/src/Java/bglpcondvar.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Thu Jul  2 10:40:48 2009 (serrano)                */
/*    Copyright   :  2005-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Condvar implementation                                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.pthread;
import java.lang.*;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    bglpcondvar                                                      */
/*---------------------------------------------------------------------*/
public class bglpcondvar extends bigloo.condvar {
   public bglpmutex mutex = null;
   
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
      Object th = bglpthread.current_thread();
      
      synchronized( m ) {
	 bglpthread.debug( ">>> pcondvar-wait cv=" + this + " m=" + m + " thread=" + m.thread + " th=" + th );
	 
	 /* assertion */
	 if( m.thread != th ) {
	    foreign.fail( "condition-variable-wait!",
			  "mutex not owned by current thread",
			  m );
	 }
	 
	 synchronized( this ) {
	    /* assertion */
	    if( mutex != null ) {
	       foreign.fail( "condition-variable-wait!",
			     "condition variable already bound to mutex",
			     this );
	    }

	    mutex = m;
	 }

	 /* release the lock */
	 m.thread = null;
	 m.notify();
	 
	 while( m.condv != this || m.thread != null ) {
	    try {
	       bglpthread.debug( "pcondvar-wait, loop cv=" + this + " m=" + m + " thread=" + m.thread + " th=" + th );

	       if( ms > 0 ) {
		  m.wait( ms );
		  if( m.condv != this && m.thread == null ) {
		     m.thread = th;
		     m.condv = null;
		     mutex = null;
		     return false;
		  }
	       } else {
		  m.wait();
	       }
	    } catch( Exception e ) {
	       m.thread = th;
	       m.condv = null;
	       mutex = null;
	       return false;
	    }
	 }

	 /* re-acquire the lock */
	 m.thread = th;
	 m.condv = null;
	 mutex = null;
	 
	 bglpthread.debug( "<<< pcondvar-wait cv=" + this + " m=" + m + " thread=" + m.thread + " th=" + th );
	 return true;
      }
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
	 bglpthread.debug( "pcondvar-signal cv=" + this + " m=" + mutex+ " thread=" + (mutex == null ? "_" : mutex.thread) + " th=" + bglpthread.current_thread() );

	 if( mutex != null ) {
	    synchronized( mutex ) {
	       mutex.condv = this;

	       if( b )
		  mutex.notifyAll();
	       else
		  mutex.notify();
	    }
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
}
