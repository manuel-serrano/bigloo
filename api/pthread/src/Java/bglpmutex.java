/*=====================================================================*/
/*    .../bigloo/bigloo/api/pthread/src/Java/bglpmutex.java            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Wed Dec 20 07:45:38 2023 (serrano)                */
/*    Copyright   :  2005-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Mutex implementation                                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.pthread;
import java.lang.*;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.TimeUnit; 

import bigloo.*;

/*---------------------------------------------------------------------*/
/*    bglpmutex                                                        */
/*---------------------------------------------------------------------*/
public class bglpmutex extends bigloo.mutex {
   public static Object mutexes = bigloo.foreign.BNIL;
   public final ReentrantLock mutex;
    
   protected static void setup() {
      bigloo.mutex.amutex = new bglpmutex( bigloo.foreign.BUNSPEC );
   }

   public Object thread = null;
   public String state = "unlocked";
   
   private Object specific;

   public bglpmutex( Object n ) {
      super( n );
      mutex = new ReentrantLock();
   }

   protected mutex create( Object name ) {
      mutex m = new bglpmutex( name );

      mutexes = foreign.MAKE_PAIR( m, mutexes );
      
      return m;
   }

   protected static void mutexes_unlock( Object thread ) {
      Object w = mutexes;

      while( w instanceof pair ) {
         bglpmutex m = (bglpmutex)(foreign.CAR( (pair)w ));
	 
         if( m.thread == thread ) {
            m.release_lock();
            m.state = "unlocked";
         }
         w = foreign.CDR( (pair)w );
      }
   }

   public int acquire_lock( int ms ) {
       int res = 1;
       try {
          
           if( mutex.tryLock(ms, TimeUnit.MILLISECONDS) ) {

               /* We synchronize on this to prevent a race condition between 1
                  thread unlocking the mutex and another locking it and their
                  subsequent updating of thread and state */
               synchronized (this) {
                   /* mark mutex owned */
                   thread = bglpthread.current_thread();
                   //System.out.printf("thread %s is locking%n", thread); 
                   state = "locked";
               }
               
               res = 0;                    
           }      
       } catch( Exception e ) {
           foreign.fail( "mutex-lock!", 
                         e.getClass() + ": " + e.getMessage(),
                         this );
       }
       return res;
   }

   public int acquire_lock() {
       int res = 1;
       try {
               
               mutex.lock();
               
               /* We synchronize on this to prevent a race condition between 1
                  thread unlocking the mutex and another locking it and their
                  subsequent updating of thread and state */
               synchronized (this) {
                   /* mark mutex owned */
                   thread = bglpthread.current_thread();
                   state = "locked";
               }
           
           res = 0;
       } catch( Exception e ) {
           foreign.fail( "mutex-lock!", 
                         e.getClass() + ": " + e.getMessage(),
                         this );
       }
       
       return res;
   }
   
    public int acquire_timed_lock( int ms ) {
      return acquire_lock( ms );
   }

   public int release_lock() {

       /* We synchronize on this to prevent a race condition between 1
          thread unlocking the mutex and another locking it and their
          subsequent updating of thread and state */
       synchronized (this) {
           mutex.unlock();
           /* mark mutex no longer owned */
           thread = null;
           state = "unlocked";
       }
       
       return 0;
   }

   public static Object SPECIFIC( Object m ) {
      if( m instanceof bglpmutex )
	 return ((bglpmutex) m).specific;
      else
	 return bigloo.foreign.BUNSPEC;
   }

   public static void SPECIFIC_SET( Object m, Object s ) {
      ((bglpmutex) m).specific = s;
   }

   public Object state() {
      return state == null ? thread : bigloo.foreign.string_to_symbol( state );
   }

   public static mutex bglpth_make_mutex( Object o ) {
      return bigloo.foreign.bgl_make_mutex( o );
   }
}
