/*=====================================================================*/
/*    .../project/bigloo/api/pthread/src/DotNet/bglpdynamic.cs         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Oct 19 10:42:04 2002                          */
/*    Last change :  Tue Oct 25 07:43:00 2005 (serrano)                */
/*    Copyright   :  2002-05 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Pthread global dynamic environments.                             */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
using System;
using System.Threading;
using bigloo;

/*---------------------------------------------------------------------*/
/*    bglpdynamic                                                      */
/*---------------------------------------------------------------------*/
namespace bigloo.pthread {
   public class bglpdynamic : bigloo.bgldynamic {
      public static void setup() {
	 bigloo.bgldynamic.abgldynamic = new bglpdynamic();
      }

      public override bgldynamic get() {
	 Object d = Thread.GetData( Thread.GetNamedDataSlot( "bgldynamic" ) );

	 if( d is bgldynamic ) {
	    return (bgldynamic)d;
	 } else {
	    return bigloo.bgldynamic.current_dynamic_env;
	 }
      }
   }
}
