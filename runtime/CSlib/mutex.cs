using System;
using System.Threading;

namespace bigloo {
  public class mutex : obj {
     public readonly Object name;
     public readonly static mutex nil_mutex = new mutex( bigloo.foreign.BUNSPEC );
     public static mutex amutex = new mutex( foreign.BUNSPEC );

     public mutex() {
       this.name = foreign.BUNSPEC;
     }

     public mutex( Object n ) {
       this.name = n;
     }

     protected virtual mutex create( Object name ) {
	return new mutex( name );
     }

     public static mutex make( Object name ) {
	return amutex.create( name );
     }
   
     public override void write( output_port p ) {
       p.write( "#<mutex>" );
     }
     
     public virtual bool acquire_lock() {
	Monitor.Enter( this );
	return true;
     }
   
     public virtual bool acquire_timed_lock( int tmt ) {
	return Monitor.TryEnter( this, tmt );
     }
   
     public virtual bool release_lock() {
	Monitor.Exit( this );
	return true;
     }
     
     public virtual Object state() {
	return foreign.BUNSPEC;
     }
  }
}
