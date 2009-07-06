/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/stack_trace.cs         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 11 12:49:28 2000                          */
/*    Last change :  Fri Jun 16 08:00:14 2006 (serrano)                */
/*    Copyright   :  2000-06 Manuel Serrano                            */
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

    public static Object dump( output_port port, int depth ) 
    {
      stack_trace runner= top_of_stack;
      int recursion= 0;
      int level= 0;
      Object old= null;

      while ((level < depth) && (runner != null))
      {
        if (bigloo.foreign.SYMBOLP( runner._symbol ))
        {
          if (runner._symbol == old)
          {
            ++recursion;
            ++depth;
          }
          else
          {
            if (0 < recursion)
            {
              port.write( " (%" );
              port.write( (1 + recursion).ToString() );
              port.write( " times)\n" );
            }
            else
              if (0 < level)
                port.write( "\n" );
	    
            port.write( "  " );
            port.write( level.ToString() );
            port.write( ". " );
            port.write( ((symbol)runner._symbol).pname );
            recursion= 0;
          }

          old= runner._symbol;
          ++level;
        }

        runner= runner.link;
      }

      port.write( "\n" );
       
      return unspecified._unspecified;
    }

    public static Object get( int depth )
    {
      stack_trace runner = top_of_stack;
      obj l = constant.nil;
      int level= 0;

      while ((level < depth) && (runner != null))
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
