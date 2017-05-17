/*=====================================================================*/
/*    .../project/bigloo/api/pthread/src/Java/bglpsemaphore.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 20 11:29:56 2017                          */
/*    Last change :  Wed May 17 09:27:01 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Semaphore implementation                                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo.pthread;
import java.lang.*;
import bigloo.*;

/*---------------------------------------------------------------------*/
/*    bglpsemaphore                                                    */
/*---------------------------------------------------------------------*/
public class bglpsemaphore extends bigloo.semaphore {
   static semaphore bgl_open_semaphore( Object name, boolean create, boolean excl,
					int mode, int value ) {
      return new semaphore( name );
   }

   static int bgl_close_semaphore( Object name ) {
      return -1;
   }
   
   static int bgl_delete_semaphore( semaphore sem ) {
      return -1;
   }
   
   static int bgl_wait_semaphore( semaphore sem ) {
      return -1;
   }
   
   static int bgl_timed_wait_semaphore( semaphore sem, int ms ) {
      return -1;
   }
   
   static int bgl_trywait_semaphore( semaphore sem ) {
      return -1;
   }
   
   static int bgl_post_semaphore( semaphore sem ) {
      return -1;
   }
   
   static int bgl_getvalue_semaphore( semaphore sem ) {
      return -1;
   }
}
