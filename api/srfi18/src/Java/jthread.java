/*=====================================================================*/
/*    /tmp/BGL2/bigloo-unstable/api/srfi18/src/Java/jthread.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 22 12:12:04 2002                          */
/*    Last change :  Mon Dec 18 16:10:10 2023 (serrano)                */
/*    Copyright   :  2002-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Java utilities for native Bigloo fair threads implementation.    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.srfi18;
import java.lang.*;
import bigloo.*;
import bigloo.pthread.*;

/*---------------------------------------------------------------------*/
/*    jthread                                                          */
/*---------------------------------------------------------------------*/
public class jthread extends bigloo.pthread.bglpthread {
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
      super();
   }
   
   public jthread( procedure t ) {
      super(t);
      thunk = t;
   }

   // nil instance
   public static jthread nil() {
      return nilthread;
   }

   public void mutexes_unlock() {
      jmutex.mutexes_unlock(thread);
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
   
   // Terminate a thread
   public static boolean terminate( jthread thread ) {
      thread.interrupt();
      return true;
   }

   // Yield the processor
   public static int sched_yield() {
      Thread.yield();
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
