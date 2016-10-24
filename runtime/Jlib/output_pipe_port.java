/*=====================================================================*/
/*    .../prgm/project/bigloo/runtime/Jlib/output_pipe_port.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 14:14:16 2009                          */
/*    Last change :  Mon Oct 24 13:43:55 2016 (serrano)                */
/*    Copyright   :  2009-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Java output pipe ports                                           */
/*=====================================================================*/
package bigloo;

import java.lang.*;
import java.io.*;
import java.util.*;

/*---------------------------------------------------------------------*/
/*    output_pipe_port ...                                             */
/*---------------------------------------------------------------------*/
public class output_pipe_port extends output_buffered_port {
   Process process;
   Thread thread;
   
   static boolean pipe_name_p( final byte[] name ) {
      
      return ( name.length > 2 
               && (name[ 0 ] == (byte)'|') 
               && (name[ 1 ] == (byte)' ') )
	  || (name.length > 5 
	      && name[ 0 ] == (byte)'p'
	      && name[ 1 ] == (byte)'i'
	      && name[ 2 ] == (byte)'p'
	      && name[ 3 ] == (byte)'e'
	      && name[ 4 ] == (byte)':');
   }

   static int skip_whitespace( byte[] s, int i ) {
      while( i < s.length && s[ i ] == ' ' ) i++;

      return i;
   }

   static String[] tokenizer( byte[] s ) {
      int len = s.length;
      Vector v = new Vector();
      // if | then offset = 2, otherwise, if pipe: offset = 5
      int offset =  (s[0] == (byte)'|') ? 2 : 5;
      int i = skip_whitespace( s, offset );

      while( i < len ) {
	 int j = i;
	 
	 while( j < len && s[ j ] != ' ' && s[ j ] != '"' ) j++;

	 if( j == len ) {
	    v.add( new String( s, i, j - i ) );
	    break;
	 }

	 if( s[ j ] == ' ' ) {
	    v.add( new String( s, i, j - i ) );
	    i = skip_whitespace( s, j + 1 );
	    continue;
	 }

	 j++; i = j;
	 while( j < s.length && s[ j ] != '"' ) j++;
	 v.add( new String( s, i, j - i ) );
	 i = skip_whitespace( s, j + 1 );
      }

      return (String[])(v.toArray( new String[ v.size() ] ));
   }

   static String[] make_cmd( byte[] s ) {
      if( foreign.bigloo_strcmp( os.OS_CLASS, "unix".getBytes() ) ) {
	 String[] scmd = new String[ 3 ];
	 scmd[ 0 ] = new String( configure.SHELL );
	 scmd[ 1 ] = "-c";
         
         // if | then offset = 2, otherwise, if pipe: offset = 5
         int offset =  (s[0] == (byte)'|') ? 2 : 5;
	 scmd[ 2 ] = new String( s, offset, s.length - offset );

	 return scmd;
      } else {
	 return tokenizer( s );
      }
   }
   
   public output_pipe_port( final byte[] cmd, final byte[] buf )
      throws IOException {
      buffer = buf;
      count = 0;
      name = cmd;

      try {
	 String[] scmd = make_cmd( cmd );

	 process = Runtime.getRuntime().exec( scmd );

	 out = process.getOutputStream();
	 final InputStream in = process.getInputStream();

	 thread = new Thread( new Runnable () {
	       public void run() {
		  byte[] buf = new byte[ 80 ];
		  int n;
		  
		  try {
		     while( true ) {
			n = in.read( buf );
			if( n <= 0 ) break;
			System.out.write( buf, 0, n );
		     }
		  } catch( Exception e ) {
		     ;
		  }
	       }
	    } );
	 thread.start();
      } catch (final IOException _i) {
	 final String scmd = new String( cmd, 2, cmd.length-2 );

	 foreign.fail( "open-output-file",
		       "Can't execute command",
		       scmd.getBytes());
      }
   }

   public Object close() {
      try {
	 flush();
	 out.close();
	 process.waitFor();
	 return bbool.vrai;
      } catch( Exception _e ) {
	 return bbool.faux;
     }
   }
}
