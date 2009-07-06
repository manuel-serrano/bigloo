/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/DotNet/bglpthread.cs     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 22 12:12:04 2002                          */
/*    Last change :  Fri Apr 10 06:55:17 2009 (serrano)                */
/*    Copyright   :  2002-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C# utilities for native Bigloo fair threads implementation.      */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
using System;
using System.Threading;
using bigloo;

/*---------------------------------------------------------------------*/
/*    bglpthread                                                       */
/*---------------------------------------------------------------------*/
namespace bigloo.pthread {
   public class bglpthread {
      private Object specific = bigloo.foreign.BUNSPEC;
      private Object cleanup = bigloo.foreign.BUNSPEC;
      private Object thread = bigloo.foreign.BUNSPEC;
      private procedure thunk;
      protected bgldynamic env;
      private Thread cthread;

      // setup
      public static void setup() {
	 bglpmutex.setup();
	 bglpcondvar.setup();
	 bglpdynamic.setup();
      }

      // dummy construcor (why is this needed for the _e library?)
      public bglpthread( ) {
      }

      // public constructor
      public bglpthread( procedure t ) {
	 thunk = t;
	 cthread = new Thread( new ThreadStart( run ) );
      }

      // nil instance
      public static bglpthread nil() {
	 return (bglpthread)null;
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
	 cleanup = (Object)p;
      }
   
      // The thread entry-point
      public void start( Object t, bool _ ) {
	 thread = t;
	 cthread.Start();
      }

      // Run the thread
      public void run() {
	 Thread.SetData( Thread.GetNamedDataSlot( "bgldynamic" ), 
			 new bgldynamic( bgldynamic.abgldynamic.get() ) );
	 Thread.SetData( Thread.GetNamedDataSlot( "bglthread" ), thread );
	 Thread.SetData( Thread.GetNamedDataSlot( "bglcthread" ), this );

	 try {
	    thunk.funcall0();
	 } catch( Exception e ) {
	    foreign.internalerror( e );
	 } finally {
	    bglpmutex.mutexes_unlock( thread );
	    
	    if( cleanup is procedure ) {
	       ((procedure)cleanup).funcall1( thread );
	    }
	 }
      }

      // Terminate a thread
      public static bool terminate( bglpthread thread ) {
	 thread.cthread.Abort();
	 return true;
      }

      // Returns the current thread
      public static Object current_thread() {
	 Object o = Thread.GetData( Thread.GetNamedDataSlot( "bglcthread" ) );

	 if( o is bglpthread ) {
	    return ((bglpthread)o).thread;
	 } else {
	    return bigloo.foreign.BFALSE;
	 }
      }

      // Yield the processor
      public static int sched_yield() {
	 return 0;
      }

      // Join
      public static void dojoin( bglpthread t ) {
	 t.cthread.Join();
      }
   }
}
