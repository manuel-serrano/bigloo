/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Jlib/dlopen.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 11 15:42:09 2000                          */
/*    Last change :  Tue Mar 12 10:40:36 2019 (serrano)                */
/*    Copyright   :  2000-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Dynamic class loading for the Jvm back-end.                      */
/*=====================================================================*/
package bigloo;

import java.lang.*;
import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.lang.reflect.*;

/*---------------------------------------------------------------------*/
/*    dlopen ...                                                       */
/*---------------------------------------------------------------------*/
public abstract class dlopen {
   private static final String NO_ERROR_YET = "No error (yet)";
   private static String bgl_dload_error = NO_ERROR_YET;
   private static final Hashtable dlopen_table = new Hashtable();

   static String prefix( final byte[] filename ) {
      final int len = filename.length;
      int i;

      for( i = len-1 ; (0 <= i) && (filename[i] != '.') ; --i ) ;

      if (0 <= i)
	 return new String( filename, 0, i );
      else
	 return new String( filename );
   }

   static String prefix( final String filename ) {
      final int len = filename.length();
      int i;

      for( i = len-1 ; (0 <= i) && (filename.charAt(i) != '.') ; --i ) ;

      if (0 <= i)
	 return filename.substring( 0, i );
      else
	 return filename;
   }

   static String suffix( final byte[] filename ) {
      final int len = filename.length;
      int i;

      for( i = len-1 ; (0 <= i) && (filename[i] != '.') ; --i ) ;

      if (0 <= i) {
	 i++;
	 return new String( filename, i , len - i );
      }
      else
	 return new String( "" );
   }

   private static Object init_module( Class cla ) {
      Method[] ms = cla.getDeclaredMethods();

      for (int i = ms.length - 1; i >=0 ; i--) {
	 Method m = ms[ i ];

	 if (m.getName().equals( "BgL_modulezd2initializa7ationz75")) {
	    try {
	       return JDK.invoke3( m, 0, "dynamic-load".getBytes() );
	    } catch (Exception e) {
	       System.out.println( e.toString() );
	       return symbol.make_symbol( "__dload_error".getBytes() );
	    }
	 }
      }
      
      return symbol.make_symbol( "__dload_noinit".getBytes() );

   }
	       
   private static Object dloadzip( final byte[] filename,
				   final byte[] init_sym,
				   final byte[] mod_sym )
      throws Exception {
      InputStream in = new FileInputStream( new String( filename ) );
      ZipInputStream zin = new ZipInputStream( in );
      boolean init = (init_sym.length > 0);
      String mod = new String( mod_sym );
      Object res = symbol.make_symbol( "__dload_noinit".getBytes() );

      while( true ) {
	 ZipEntry zentry = zin.getNextEntry();

	 if( zentry == null ) break;

	 String name = prefix( zentry.getName().getBytes() ).replace( '/', '.' );

         Class new_class = foreign.class.getClassLoader().loadClass( name );

	 if( init ) {
	    try {
	       final Method initm = JDK.getDeclaredMethod( new_class, init_sym );
	       init = false;
	       res = JDK.invoke( initm );
	    } catch (Exception e) {
	       ;
	    }
	 }

	 if (new_class.getName().equals(mod)) {
	    init_module(new_class);
	 }
      }
	    
      zin.close();
      in.close();
	    
      return res;
   }
   
   public static Object dloadresource( final byte[] filename,
				       final byte[] init_sym,
				       final byte[] mod_sym )
      throws Exception {
      String name = prefix( bigloo.foreign.resource_name( filename ) ).replace( '/', '.' );

      Class new_class = foreign.class.getClassLoader().loadClass( name.replace( '\\', '/' ) );
      
      if( init_sym.length > 0 ) {
	 try {
	    final Method init = JDK.getDeclaredMethod( new_class, init_sym );
	 
	    JDK.invoke( init );
	 } catch (final Exception e) {
	    bgl_dload_error = e.toString();
	    return symbol.make_symbol( "__dload_error".getBytes() );
	 }
      }
      
      if( mod_sym.length > 0 ) {
	 return init_module(new_class);
      }
      return foreign.BUNSPEC;
   }

   static Object dload_inner( final byte[] filename,
			      final byte[] init_sym,
			      final byte[] mod_sym ) {
      bgl_dload_error = NO_ERROR_YET;

      System.out.println( "MS 12mar19: DLOAD_INNER incompatible with the C version" );
      System.out.println( "should return a multiple value" );
      
      try {
	 if( suffix( filename ).equals( "zip" ) ) {
	    return dloadzip( filename, init_sym, mod_sym );
	 } else {
	    if( bigloo.foreign.is_resourcep( filename ) ) {
	       return dloadresource( filename, init_sym, mod_sym );
	    } else {
	       String name = prefix( filename );

	       final Class new_class = Class.forName( name );
	    
	       if( init_sym.length > 0 ) {
		  final Method init = JDK.getDeclaredMethod( new_class, init_sym );
	 
		  JDK.invoke( init );
	       }

	       if( mod_sym.length > 0 ) {
		  return init_module(new_class);
	       }
	       return foreign.BUNSPEC;
	    }
	 }
      } catch (final Exception e) {
	 bgl_dload_error = e.toString();
	 return symbol.make_symbol( "__dload_error".getBytes() );
      }
   }

   public static Object dload( final byte[] filename,
			       final byte[] init_sym,
			       final byte[] mod_sym ) {
      synchronized( dlopen_table ) {
	 if( !(dlopen_table.contains( filename )) ) {
	    Object res = dload_inner( filename, init_sym, mod_sym );
	    dlopen_table.put( filename, new Boolean( true ) );
	    return res;
	 } else {
	    return foreign.BUNSPEC;
	 }
      }
   }

   public static byte[] dload_error() {
      return bgl_dload_error.getBytes();
   }
}
