/*=====================================================================*/
/*    .../bigloo-unstable/api/pthread/src/Java/bglpthread.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 22 12:12:04 2002                          */
/*    Last change :  Mon Dec 18 21:57:50 2023 (serrano)                */
/*    Copyright   :  2002-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Java utilities for native Bigloo fair threads implementation.    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.pthread;
import java.lang.*;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    bglpthread                                                       */
/*---------------------------------------------------------------------*/
public class bglpthread extends Thread {
   public Object specific = bigloo.foreign.BUNSPEC;
   public Object cleanup = bigloo.foreign.BUNSPEC;
   public Object thread = bigloo.foreign.BUNSPEC;
   public procedure thunk;
   public bgldynamic env;

   static bglpthread nilthread = new bglpthread();
   
   // debug
   static private boolean debug = false;

   static void debug( String msg ) {
      if( debug ) System.out.println( msg );
   }
   
   // setup
   public static void setup() {
      bglpmutex.setup();
      bglpcondvar.setup();
      bglpdynamic.setup();
   }
      
   // public constructor
   public bglpthread() {
      ;
   }
   
   public bglpthread( procedure t ) {
      super();
      thunk = t;
   }

   // nil instance
   public static bglpthread nil() {
      return nilthread;
   }

   // public SPECIFIC get
   public Object SPECIFIC() {
      return specific;
   }

   // public SPECIFIC set
   public void SPECIFIC_SET( Object o ) {
      specific = o;
   }
   
   // public CLEANUP get
   public Object CLEANUP() {
      return cleanup;
   }

   // public CLEANUP set
   public void CLEANUP_SET( Object p ) {
      cleanup = p;
   }
   
   // The thread entry-point
   public void start( Object t, boolean _b ) {
      thread = t;
      env = new bglpdynamic( bgldynamic.abgldynamic.get() );
      start();
   }

   // Run the thread
   public void run() {
      try {
	 thunk.funcall0();
      } catch( Throwable e ) {
	 try {
	    foreign.internalerror( e );
	 } catch( Throwable _t ) {
	    System.exit( 1 );
	 }
      } finally {
          mutexes_unlock();

	 if( cleanup instanceof procedure ) {
	    ((procedure)cleanup).funcall1( thread );
	 }
      }
   }

   // mutex cleanup
   public void mutexes_unlock() {
      bglpmutex.mutexes_unlock(thread);
   }
   
   // Terminate a thread
   public static boolean terminate( bglpthread thread ) {
      thread.interrupt();
      return true;
   }

   // send a signal
   public static int kill( bglpthread thread, int n ) {
      // not implemented
      return -1;
   }

   // Returns the current thread
   public static Object current_thread() {
      Thread t = currentThread();
      if( t instanceof bglpthread ) {
	 return ((bglpthread)t).thread;
      } else {
	 return bigloo.foreign.BFALSE;
      }
   }

   // Yield the processor
   public static int sched_yield() {
      Thread.yield();
      return 0;
   }
   
   // Join
   public static void dojoin( bglpthread t, Object tmt ) {
      try {
	 if( bigloo.foreign.INTEGERP( tmt ) ) {
	    t.join( bigloo.foreign.CINT( (bint)tmt ) );
	 } else {
	    t.join();
	 }
      } catch( Throwable e ) {
	 try {
	    foreign.internalerror( e );
	 } catch( Throwable _t ) {
	    System.exit( 1 );
	 }
      }
   }
}
