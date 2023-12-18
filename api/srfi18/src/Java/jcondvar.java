/*=====================================================================*/
/*    /tmp/BGL2/bigloo-unstable/api/srfi18/src/Java/jcondvar.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Mon Dec 18 15:24:34 2023 (serrano)                */
/*    Copyright   :  2005-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Condvar implementation                                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.srfi18;
import java.lang.*;
import java.util.*;
import bigloo.*;
import bigloo.pthread.*;

/*---------------------------------------------------------------------*/
/*    jcondvar                                                         */
/*---------------------------------------------------------------------*/
public class jcondvar extends bigloo.pthread.bglpcondvar {
   ArrayList alist = new ArrayList();
   
   protected static void setup() {
      bigloo.condvar.acondvar = new jcondvar( bigloo.foreign.BUNSPEC );
   }
   
   private Object specific;
   
   public jcondvar( Object n ) {
      super( n );
   }

   public static Object SPECIFIC( Object o ) {
      return ((jcondvar) o).specific;
   }
   
   public static void SPECIFIC_SET( Object o, Object s ) {
      ((jcondvar) o).specific = s;
   }

   public boolean wait( final jmutex m, final int ms ) {
      boolean res = true;
      Object th = jthread.current_thread();
      
      synchronized( m ) {
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
	    return false;
	 }
      }
      
      /* release the condvar and re-acquire the lock */
      m.acquire_lock();
      m.thread = th;
      System.out.println("jscondvar after wait..." + th.toString());
      
      return res;
   }

      
   public boolean wait( final bigloo.mutex o, final int ms ) {
      if( !(o instanceof jmutex) ) {
	 bigloo.foreign.fail( "condition-variable-wait!",
			      "Not a pmutex (library badly initialized)",
			      o );
	 return false;
      } else {
	 return wait( (jmutex)o, ms );
      }
   }


   public boolean wait( bigloo.mutex o ) {
      return wait( o, 0 );
   }
   
   public boolean timed_wait( bigloo.mutex o, int ms ) {
      return wait( o, ms );
   }

   public boolean cond_signal(boolean b) {
      if( b ) {
	 alist.clear();
	 notifyAll();
      } else {
	 if( alist.size() > 0 ) alist.remove( 0 );
	    
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

   public static bigloo.condvar srfi18_make_condvar( Object o ) {
      return bigloo.foreign.bgl_make_condvar( o );
   }
}
