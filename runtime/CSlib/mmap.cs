using System;
using System.IO;

namespace bigloo {
   public class mmap : obj {
      public readonly byte[] name;
      public readonly Stream br, bw;
      public readonly long len;
      public long rp, wp;

      protected mmap( byte[] s ) {
	 name = s;
      }
      
      public mmap( byte[] fname, bool r, bool w ) {
	 String s = foreign.newstring( fname );
	 br = r ? new FileStream( s, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ) : null;
	 bw = w ? new FileStream( s, FileMode.OpenOrCreate, FileAccess.Write, FileShare.Read ) : null;
	 name = fname;
	 len = br.Length;
	 rp = 0;
	 wp = 0;
      }

      public virtual int get( long i ) {
	 if( br != null ) {
	    if( i != rp ) {
	       br.Seek( (long)i, 0 );
	    }

	    rp = i + 1;
	    return br.ReadByte();
	 } else {
	    foreign.fail( "mmap-ref", "write only mmap", this );
	    return 0;
	 }
      }

      public virtual void put( long i, int c ) {
	 if( bw != null ) {
	    if( i != wp ) {
	       bw.Seek( (long)i, 0 );
	    }

	    wp = i + 1;
	    bw.WriteByte( (byte)c );
	    bw.Flush();
	    if( br != null ) {
	       br.Flush();
	       br.Seek( (long)rp, 0 );
	    }
	    return;
	 } else {
	    foreign.fail( "mmap-set!", "read only mmap", this );
	    return;
	 }
      }

      public virtual void close() {
	 if( br !=null ) br.Close();
	 if( bw !=null ) bw.Close();
      }
      
      public override void write( output_port p ) {
	 p.write( "#<mmap:" + name + ":" + len + ">" );
      }
   }
}
