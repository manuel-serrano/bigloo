package bigloo;

import java.util.*;
import java.io.*;
import java.nio.*;
import java.nio.channels.*;

public class mmaps extends mmap {
   private boolean read, write;
   
   public mmaps( byte[] s, boolean r, boolean w ) {
      super();
      map = null;
      name = s;
      read = r;
      write = w;
      len = s.length;
   }

   public Object close() {
      return this;
   }
   
   public int get( long i ) {
      if( read ) 
	 return name[ (int)i ];
      else {
	 foreign.fail( "mmap", "write only mmap", this );
	 return -1;
      }
   }

   public void put( long i, byte c ) {
      if( write ) 
	 name[ (int)i ] = c;
      else
	 foreign.fail( "mmap", "read only mmap", this );
   }
}   
