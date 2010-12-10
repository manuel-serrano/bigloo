/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/stack_trace.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 11 12:49:28 2000                          */
/*    Last change :  Fri Dec 10 16:49:57 2010 (serrano)                */
/*    Copyright   :  2000-10 Manuel Serrano                            */
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
   public static stack_trace top_of_stack = new stack_trace( bigloo.foreign.BUNSPEC );
   private Object name;
   private Object location;
   private final stack_trace link;

   public stack_trace( final Object nm ) {
      this.name = nm;
      this.location = bigloo.foreign.BUNSPEC;
      link = top_of_stack;
      top_of_stack = this;
   }

   public stack_trace( final Object nm, final Object l ) {
      this.name = nm;
      this.location = l;
      link = top_of_stack;
      top_of_stack = this;
   }

   public static Object get_top() {
      return top_of_stack.name;
   }
   
   public static void set_trace( Object nm ) {
      top_of_stack.name = nm;
   }
   
   public static void set_trace_location( Object loc ) {
      top_of_stack.location = loc;
   }
   
   public static Object pop_trace() {
      top_of_stack = top_of_stack.link;
      return unspecified.unspecified;
   }

   public static Object get( int depth ) throws IOException {
      stack_trace runner = top_of_stack;
      obj l = nil.nil;
      int level= 0;

      while (((depth < 0) || (level < depth)) && (runner != null)) {
	 if (bigloo.foreign.SYMBOLP( runner.name )
	     && (((symbol)runner.name).string.length > 0)) {
	    l = new pair(new pair(runner.name, runner.location), l);
	  
	    level++;
	 }

	 runner = runner.link;
      }

      return l;
   }
}
