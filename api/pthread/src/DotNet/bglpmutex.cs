/*=====================================================================*/
/*    .../prgm/project/bigloo/api/pthread/src/DotNet/bglpmutex.cs      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Sat May  3 06:24:24 2008 (serrano)                */
/*    Copyright   :  2005-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Mutex implementation                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
using System;
using System.Threading;
using bigloo;

/*---------------------------------------------------------------------*/
/*    bglpmutex                                                        */
/*---------------------------------------------------------------------*/
namespace bigloo.pthread {
   public class bglpmutex : bigloo.mutex {
      private static Object mutexes = bigloo.foreign.BNIL;
      private static Object sym_not_owned = bigloo.foreign.BUNSPEC;
      private static Object sym_abandoned = bigloo.foreign.BUNSPEC;
      private static Object sym_not_abandoned = bigloo.foreign.BUNSPEC;
      private static bool init = false;
      
      public static void setup() {
	 if( !init ) {
	    init = true;
	    bigloo.mutex.amutex = new bglpmutex( bigloo.foreign.BUNSPEC );
	    sym_not_owned = bigloo.foreign.string_to_symbol( "not-owned" );
	    sym_abandoned = bigloo.foreign.string_to_symbol( "abandoned" );
	    sym_not_abandoned = bigloo.foreign.string_to_symbol( "not-abandoned" );
	 }
      }

      private bool locked = false;
      protected Object thread = null;
      
      private Object specific = bigloo.foreign.BUNSPEC;

      public bglpmutex( Object n ) : base( n ) {
	 ; 
      }

      protected override mutex create( Object name ) {
	 mutex m = new bglpmutex( name );
	 mutexes = foreign.MAKE_PAIR( m, mutexes );
	 return m;
      }

      public static void mutexes_unlock( Object thread ) {
	 Object w = mutexes;

	 while( w is pair ) {
	    bglpmutex m = (bglpmutex)(foreign.CAR( (pair)w ));
	    
	    if( m.thread == thread ) {
	       m.release_lock();
	       m.thread = thread;
	    }
	    w = foreign.CDR( (pair)w );
	 }
      }
   
      public override bool acquire_lock() {
	 base.acquire_lock();
	 locked = true;
	 thread = bglpthread.current_thread();

	 return true;
      }

      public override bool release_lock() {
	 locked = false;
	 thread = null;
	 base.release_lock();

	 return true;
      }

      public static Object SPECIFIC( Object m ) {
	 if( m is bglpmutex )
	    return ((bglpmutex) m).specific;
	 else
	    return bigloo.foreign.BUNSPEC;
      }

      public static void SPECIFIC_SET( Object m, Object s ) {
	 if( m is bglpmutex )
	    ((bglpmutex) m).specific = s;
      }

      public override Object state() {
	 if( locked == true ) {
	    return thread;
	 }
	 if( thread == null )
	    return sym_not_abandoned;
	 else
	    return sym_abandoned;
      }
   }
}
