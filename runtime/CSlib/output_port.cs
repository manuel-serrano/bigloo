using System;
using System.IO;

namespace bigloo
{
   public abstract class output_port: obj
   {
      public readonly byte[] name;
      public Object chook = bigloo.foreign.BUNSPEC;
      public Object fhook = bigloo.foreign.BUNSPEC;
      public Object flushbuf = bigloo.foreign.BUNSPEC;

      protected output_port()
      {
	 name= foreign.getbytes( "???" );
      }

      protected output_port( byte[] name )
      {
	 this.name= name;
      }

      public virtual Object close() {
	 if( chook is procedure )
	 {
	    ((procedure)chook).funcall1(this);
	 }
	 return this;
      }

      public abstract Object flush();

      public abstract void invoke_flush_hook( bigloo.bint size );
	 
      public Object bgl_output_port_seek( int pos )
      {
	 return bigloo.foreign.BFALSE;
      }

      public abstract void write( int cn );

      public abstract void write( byte[] s );

      public abstract void write( byte[] s, int start, int len );

      public abstract void write( String s );

      public override void write( output_port p ) 
      {
	 p.write( "#<output_port: " + foreign.newstring( name ) + ">" );
      }
   }
}
