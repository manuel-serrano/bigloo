using System;

namespace bigloo {
  public class condvar : obj {
     public readonly Object name;
     public readonly static condvar nil_condvar = new condvar( bigloo.foreign.BUNSPEC );
     public static condvar acondvar = new condvar( foreign.BUNSPEC );

     public condvar( Object n ) {
       this.name = n;
     }

     protected virtual condvar create( Object name ) {
	return new condvar( name );
     }

     public static condvar make( Object name ) {
	return acondvar.create( name );
     }
   
     public override void write( output_port p ) {
       p.write( "#<condvar>" );
     }
  }
}
