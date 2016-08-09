/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/srfi18/src/Java/jthread.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 22 12:12:04 2002                          */
/*    Last change :  Tue Aug  9 10:57:47 2016 (serrano)                */
/*    Copyright   :  2002-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Java utilities for native Bigloo fair threads implementation.    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.srfi18;
import java.lang.*;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    jthread                                                          */
/*---------------------------------------------------------------------*/
public class jthread extends Thread {
   private Object specific = bigloo.foreign.BUNSPEC;
   private Object cleanup = bigloo.foreign.BUNSPEC;
   private Object thread = bigloo.foreign.BUNSPEC;
   private procedure thunk;
   protected bgldynamic env;

   static jthread nilthread = new jthread();
   
   // debug
   static private boolean debug = false;

   static void debug( String msg ) {
      if( debug ) System.out.println( msg );
   }
   
   // setup
   public static void setup() {
      jmutex.setup();
      jcondvar.setup();
      jdynamic.setup();
   }
      
   // public constructor
   public jthread() {
      ;
   }
   
   public jthread( procedure t ) {
      super();
      thunk = t;
   }

   // nil instance
   public static jthread nil() {
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
   public void start( Object t, boolean b ) {
      thread = t;
      env = new bgldynamic( bgldynamic.abgldynamic.get() );
      
      start();
   }

   // Run the thread
   public void run() {
      try {
	 thunk.funcall0();
      } catch( Throwable e ) {
	 try {
	    foreign.internalerror( e );
	 } catch( Throwable t ) {
	    System.exit( 1 );
	 }
      } finally {
	 jmutex.mutexes_unlock( thread );

	 if( cleanup instanceof procedure ) {
	    ((procedure)cleanup).funcall1( thread );
	 }
      }
   }

   // Terminate a thread
   public static boolean terminate( jthread thread ) {
      thread.interrupt();
      return true;
   }

   // Returns the current thread
   public static Object current_thread() {
      Thread t = currentThread();

      if( t instanceof jthread ) {
	 return ((jthread)t).thread;
      } else {
	 return bigloo.foreign.BFALSE;
      }
   }

   // Yield the processor
   public static int sched_yield() {
      yield();
      return 0;
   }
   
   // Join
   public static void dojoin( jthread t, Object tmt ) {
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
