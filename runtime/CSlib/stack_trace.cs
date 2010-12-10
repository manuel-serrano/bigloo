/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/stack_trace.cs         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 11 12:49:28 2000                          */
/*    Last change :  Fri Dec 10 16:49:27 2010 (serrano)                */
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
    public static stack_trace top_of_stack = new stack_trace( foreign.BUNSPEC );
    private Object _name;
    private Object _location;
    private readonly stack_trace link;

    public stack_trace( Object _name )
    {
      this._name = _name;
      this._location = foreign.BUNSPEC;
      link = top_of_stack;
      top_of_stack = this;
    }

    public stack_trace( Object _name, Object loc  )
    {
      this._name = _name;
      this._location = foreign.BUNSPEC;
      this._location = loc;
      link = top_of_stack;
      top_of_stack = this;
    }

    public static void set_trace( Object name )
    {
       top_of_stack._name = name;
    }
	   
    public static void set_trace_location( Object loc )
    {
       top_of_stack._location = loc;
    }
	   
    public static Object pop_trace()
    {
      top_of_stack = top_of_stack.link;
      return unspecified._unspecified;
    }

    public static Object get( int depth )
    {
      stack_trace runner = top_of_stack;
      obj l = nil._nil;
      int level= 0;

      while (((depth < 0) || (level < depth)) && (runner != null))
      {
	 if (bigloo.foreign.SYMBOLP( runner._name ))
	 {
	    l = new pair(new pair(runner._name, runner._location), l);
	    
	    level++;
	 }
	 
	 runner= runner.link;
      }

      return l;
    }
  }
}
