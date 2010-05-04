/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/stack_trace.cs         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 11 12:49:28 2000                          */
/*    Last change :  Tue Apr 20 12:35:53 2010 (serrano)                */
/*    Copyright   :  2000-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Stack trace JVM implementation                                   */
/*=====================================================================*/
using System;

namespace bigloo
{
  /*---------------------------------------------------------------------*/
  /*    STACK_TRACE                                                      */
  /*    -------------------------------------------------------------    */
  /*    This class is only used for debugging purpose. It enables        */
  /*    error handlers to display the current Scheme stack when an       */
  /*    error is raised.                                                 */
  /*---------------------------------------------------------------------*/
  public sealed class stack_trace
  {
    public static stack_trace top_of_stack =
       new stack_trace( symbol.make_symbol( foreign.getbytes( "" ) ) );
    private Object _symbol;
    private readonly stack_trace link;

    public stack_trace( Object  _symbol )
    {
      this._symbol= _symbol;
      link= top_of_stack;
      top_of_stack= this;
    }

    public static void set_trace( Object symbol )
    {
       top_of_stack._symbol = symbol;
    }
	   
    public static Object pop_trace()
    {
      top_of_stack = top_of_stack.link;
      return unspecified._unspecified;
    }

    public static Object get( int depth )
    {
      stack_trace runner = top_of_stack;
      obj l = constant.nil;
      int level= 0;

      while (((depth < 0) || (level < depth)) && (runner != null))
      {
	 if (bigloo.foreign.SYMBOLP( runner._symbol ))
	 {
	    l = new pair(runner._symbol, l);
	    
	    level++;
	 }
	 
	 runner= runner.link;
      }

      return l;
    }
  }
}
