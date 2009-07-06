/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/stack_trace.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 11 12:49:28 2000                          */
/*    Last change :  Fri Jun 23 11:20:03 2006 (serrano)                */
/*    Copyright   :  2000-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Stack trace JVM implementation                                   */
/*=====================================================================*/
package bigloo;

import java.io.*;

/*---------------------------------------------------------------------*/
/*    STACK_TRACE                                                      */
/*    -------------------------------------------------------------    */
/*    This class is only used for debugging purpose. It enables        */
/*    error handlers to display the current Scheme stack when an       */
/*    error is raised.                                                 */
/*---------------------------------------------------------------------*/
public class stack_trace {
   public static stack_trace top_of_stack =
   new stack_trace( bigloo.symbol.make_symbol( "".getBytes() ) );
   private Object symbol;
   private final stack_trace link;

   public stack_trace( final Object symbol ) {
      this.symbol = symbol;
      link = top_of_stack;
      top_of_stack = this;
   }

   public static void set_trace( Object symbol ) {
      top_of_stack.symbol = symbol;
   }
   
   public static Object pop_trace() {
      top_of_stack = top_of_stack.link;
      return unspecified.unspecified;
   }

   public static Object dump( final output_port port, int depth )
      throws IOException {
      stack_trace runner = top_of_stack;

      int recursion = 0;
      int level = 0;
      Object old = null;

      while ((level < depth) && (runner != null)) {
	 if (bigloo.foreign.SYMBOLP( runner.symbol )) {
	    if (runner.symbol == old) {
	       ++recursion;
	       ++depth;
	    } else {
	       if (0 < recursion) {
		  port.write( " (%" );
		  port.write( Integer.toString( 1 + recursion ) );
		  port.write( " times)\n" );
	       } else {
		  if (0 < level)
		     port.write( "\n" );
	       }

	       port.write( "  " );
	       port.write( Integer.toString( level ) );
	       port.write( ". " );
	       port.write( ((symbol)(runner.symbol)).string );
	       recursion= 0;
	    }

	    old = runner.symbol;
	    ++level;
	 }

	 runner= runner.link;
      }
      port.write( "\n" );

      return unspecified.unspecified;
   }

   public static Object get( int depth ) throws IOException {
      stack_trace runner = top_of_stack;
      obj l = constant.nil;
      int level= 0;

      while ((level < depth) && (runner != null)) {
	 if (bigloo.foreign.SYMBOLP( runner.symbol )) {
	    l = new pair(runner.symbol, l);
	  
	    level++;
	 }

	 runner= runner.link;
      }

      return l;
   }
}
