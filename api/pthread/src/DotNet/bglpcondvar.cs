/*=====================================================================*/
/*    .../prgm/project/bigloo/pthread/src/DotNet/bglpcondvar.cs        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar  5 13:37:30 2005                          */
/*    Last change :  Tue Mar  8 15:31:25 2005 (serrano)                */
/*    Copyright   :  2005 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Condvar implementation                                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
using System;
using System.Threading;
using bigloo;

/*---------------------------------------------------------------------*/
/*    bglpcondvar                                                      */
/*---------------------------------------------------------------------*/
namespace bigloo.pthread {
   public class bglpcondvar : bigloo.condvar {
      public static void setup() {
	 bigloo.condvar.acondvar = new bglpcondvar( bigloo.foreign.BUNSPEC );
      }
   
      private Object specific = bigloo.foreign.BUNSPEC;
   
      public bglpcondvar( Object n ) : base( n ) {
	 ;
      }

      protected override condvar create( Object name ) {
	 return new bglpcondvar( name );
      }
   
      public static Object SPECIFIC( Object cv ) {
	 if( cv is bglpcondvar )
	    return ((bglpcondvar)cv).specific;
	 else
	    return bigloo.foreign.BUNSPEC;
      }
   
      public static void SPECIFIC_SET( Object cv, Object s ) {
	 if( cv is bglpcondvar )
	    ((bglpcondvar)cv).specific = s;
      }
   }
}
