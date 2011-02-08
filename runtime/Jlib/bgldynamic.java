/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/bgldynamic.java         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Oct 19 10:42:04 2002                          */
/*    Last change :  Tue Feb  8 10:39:49 2011 (serrano)                */
/*    Copyright   :  2002-11 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Handling global dynamic environments (current_output_port et al) */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo;

import java.lang.*;
import bigloo.foreign;

/*---------------------------------------------------------------------*/
/*    bgldynamic                                                       */
/*---------------------------------------------------------------------*/
public class bgldynamic
{
   protected static bgldynamic current_dynamic_env = new bgldynamic();
   public static bgldynamic abgldynamic = new bgldynamic();

   public bgldynamic get() {
      return current_dynamic_env;
   }

   // global environment
   public input_port current_input_port;
   public output_port current_output_port;
   public output_port current_error_port;
   
   public Object current_display;
   
   public int mvalues_number;
   public final Object[] mvalues_values= { unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified,
					   unspecified.unspecified };
   
   public Object exitd_top;
   public Object exitd_val;
   public Object error_handler;
   public Object uncaught_exception_handler;
   public Object error_notifiers;
   public Object interrupt_notifier;
   public Object current_thread;
   public Object debug_alist = bigloo.foreign.BNIL;
   public Object lexical_stack = bigloo.foreign.BNIL;
   public Object bytecode = bigloo.foreign.BUNSPEC;
   public Object evstate = bigloo.foreign.BUNSPEC;
   public Object module = bigloo.foreign.BUNSPEC;
   public Object abase = bigloo.foreign.BUNSPEC;
   public Object parameters = bigloo.foreign.BNIL;
   public Object thread_backend = bigloo.foreign.BUNSPEC;
   public Object user_data = bigloo.foreign.BNIL;

   // constructor
   public bgldynamic() {
      exitd_top = new exit();
      exitd_val = new pair( new pair(unspecified.unspecified,
				     unspecified.unspecified),
			    unspecified.unspecified );

      error_handler = bigloo.nil.nil;
      uncaught_exception_handler = bigloo.nil.nil;

      mvalues_number = 1;

      current_input_port = new input_console_port( new byte[ foreign.default_io_bufsiz ] );
      current_output_port = new output_port( System.out );
      current_error_port = new output_port( System.err );
   }

   // constructor
   public bgldynamic( final bgldynamic o ) {
      exitd_top = new exit();
      exitd_val = new pair( new pair(unspecified.unspecified,
				     unspecified.unspecified),
			    unspecified.unspecified );
      
      error_handler = nil.nil;

      mvalues_number = 1;

      current_input_port = o.current_input_port;
      current_output_port = o.current_output_port;
      current_error_port = o.current_error_port;
      
      thread_backend = o.thread_backend;
      current_thread = o.current_thread;
      
      module = o.module;
      abase = o.abase;
   }
}
