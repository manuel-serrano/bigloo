package bigloo;

import java.util.*;
import java.io.*;
import java.nio.*;
import java.nio.channels.*;

public class mmap extends obj {
   public byte[] name;
   public InputStream in;
   public MappedByteBuffer map;
   public long len;
   public long rp, wp;
   FileChannel fc;
   RandomAccessFile raf;

   protected mmap() {
      rp = 0; wp = 0;
   }
   
   public mmap( String resource ) {
      try {
	 map = null;
         in = foreign.class.getClassLoader().getResourceAsStream( resource.replace( '\\', '/' ) );
	 name = resource.getBytes();
	 rp = 0; wp = 0;
	 len = in.available();
      } catch( FileNotFoundException e ) {
	 foreign.fail( "mmap", "Resource not found", resource );
      } catch( IOException e ) {
	 foreign.fail( "mmap", "Cannot mmap resource", resource );
      } 
   }
   
   public mmap( final byte[] file, boolean r, boolean w ) {
      try {
	 raf = new RandomAccessFile( new String( file ),
				     (r && w) ? "rw"
				     : (r?"r":"w") );
	 fc = raf.getChannel();
      
	 len = raf.length();

	 in = null;
	 
	 if( r && w ) {
	    map = fc.map( FileChannel.MapMode.READ_WRITE, 0, len );
	 } else {
	    if( r ) {
	       map = fc.map( FileChannel.MapMode.READ_ONLY, 0, len );
	    } else {
	       map = fc.map( FileChannel.MapMode.READ_WRITE, 0, len );
	    }
	 }
	 name = file;
	 
	 map.load();
	 
	 rp = 0; wp = 0;
      } catch( FileNotFoundException e ) {
	 foreign.fail( "mmap", "File not found", file );
      } catch( IOException e ) {
	 foreign.fail( "mmap", "Cannot mmap file", file );
      }
   }

   public Object close() {
      if( map !=null ) {
	 map = null;
	 try {
	    fc.close();
	    raf.close();
	 } catch( IOException _i ) {
	    return bbool.faux;
	 }

	 return bbool.vrai;
      }

      if( in != null ) {
	 try {
	    in.close();
	 } catch( IOException _i ) {
	    return bbool.faux;
	 }

	 in = null;
	 
	 return bbool.vrai;
      }

      return bbool.faux;
   }
   
   public int get( long i ) {
      try {
	 if( in != null ) {
	    if( i != rp ) {
	       if( i > rp ) {
		  in.skip( rp - i );
	       } else {
		  in.close();
                  in = foreign.class.getClassLoader().getResourceAsStream( new String( name ) );
		  in.skip( i );
	       }
	    }

	    rp = i + 1;
	    return in.read();
	 } else {
	    foreign.fail( "mmap-ref", "write only mmap", this );
	    return 0;
	 }
      } catch( IOException _i ) {
	 return 0;
      }
   }

   public void put( long i, byte c ) {
      ;
   }
   
   public void write( final output_port  p ) {
      p.write( "#<mmap:" + new String( name ) + ":" + len + ">" );
   }
}
