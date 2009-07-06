package bigloo;

import java.util.*;

public class condvar extends obj
{
   public final Object name;
   public static final condvar nil_condvar = new condvar();
   public static condvar acondvar = new condvar(foreign.BUNSPEC);
   public Object condition = null;
   
   public condvar() {
      this.name = bigloo.foreign.BUNSPEC;
   }

   public condvar( final Object n )
   {
      this.name = n;
   }

   protected condvar create( Object name ) {
      return new condvar( name );
   }
   
   public static condvar make( Object name ) {
      return acondvar.create( name );
   }
   
   public void write( final output_port p ) {
     p.write( "#<condvar>" );
   }

   public boolean wait(mutex o) {
      synchronized( this ) {
	 try {
	    bigloo.foreign.bgl_mutex_unlock( o );
	    this.wait();
	    bigloo.foreign.bgl_mutex_lock( o );
	    return true;
	 } catch( Exception e ) {
	    bigloo.foreign.fail( "condition-variable-wait!",
				 e.getMessage(),
				 this );
	    return false;
	 }
      }
   }

   public boolean timed_wait(mutex o, int ms) {
      synchronized( this ) {
	 try {
	    boolean res;
	    this.condition = null;
	    bigloo.foreign.bgl_mutex_unlock( o );
	    this.wait( ms == 0 ? 1 : (long)ms );
	    
	    res = ((this.condition instanceof Boolean) &&
		   ((Boolean)this.condition == Boolean.TRUE))
	       || ((this.condition instanceof condvar) &&
		   ((condvar)this.condition == this));

	    this.condition = null;
	    bigloo.foreign.bgl_mutex_lock( o );
	    
	    return res;
	 } catch( Exception e ) {
	    bigloo.foreign.fail( "condition-variable-wait!",
				 e.getMessage(),
				 this );
	    return false;
	 }
      }
   }

   public boolean broadcast() {
      synchronized( this ) {
	 this.condition = Boolean.TRUE;
	 this.notifyAll();
      }

      return true;
   }
   
   public boolean signal() {
      synchronized( this ) {
	 this.condition = (Object)this;
	 this.notify();
      }
      
      return true;
   }
}
