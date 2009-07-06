/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/bgldynamic.cs          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Oct 19 10:42:04 2002                          */
/*    Last change :  Fri Jun 19 15:19:04 2009 (serrano)                */
/*    Copyright   :  2002-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Handling global dynamic environments (current_output_port et al) */
/*=====================================================================*/
using System;

namespace bigloo
{
  public class bgldynamic 
  {
    public static bgldynamic current_dynamic_env= new bgldynamic();
    public static bgldynamic abgldynamic = new bgldynamic();

    public virtual bgldynamic get() {
      return current_dynamic_env;
    }

    // global environment
    public input_port current_input_port;
    public output_port current_output_port;
    public output_port current_error_port;

    public Object current_display;

    public Object exitd_top;
    public Object exitd_val;
    public Object error_handler;
    public Object uncaught_exception_handler;
    public Object interrupt_notifier;
    public Object current_thread;
    public Object debug_alist = bigloo.foreign.BNIL;
    public Object lexical_stack = bigloo.foreign.BNIL;
    public Object bytecode = bigloo.foreign.BUNSPEC;
    public Object module = bigloo.foreign.BUNSPEC;
    public Object abase = bigloo.foreign.BUNSPEC;
    public Object parameters = bigloo.foreign.BNIL;
    public Object thread_backend = bigloo.foreign.BFALSE;
    public Object user_data = bigloo.foreign.BNIL;

    public int mvalues_number;
    public readonly Object[] mvalues_values= { unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
					       unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified,
                                               unspecified._unspecified };

    // constructor
    public bgldynamic()
    {
      exitd_top= new exit();
      exitd_val = new pair( new pair(unspecified._unspecified,
				     unspecified._unspecified),
			    unspecified._unspecified );
      error_handler= nil._nil;
      uncaught_exception_handler= nil._nil;

      mvalues_number= 1;

      current_input_port= new input_console_port( foreign.default_io_bufsiz );
      current_output_port= new output_stream_port( Console.OpenStandardOutput() );    // !!!!! to check
      current_error_port= new output_stream_port( Console.OpenStandardError() );      // !!!!! to check

      error_handler= bigloo.nil._nil;
    }

    // constructor
    public bgldynamic( bgldynamic o ) 
    {
      exitd_top= new exit();
      exitd_val = new pair( new pair(unspecified._unspecified,
				     unspecified._unspecified),
			    unspecified._unspecified );
      
      error_handler= nil._nil;

      mvalues_number= 1;

      current_input_port= o.current_input_port;
      current_output_port= o.current_output_port;
      current_error_port= o.current_error_port;

      thread_backend = o.thread_backend;
      current_thread = o.current_thread;
      
      module = o.module;
      abase = o.abase;
    }
  }
}
