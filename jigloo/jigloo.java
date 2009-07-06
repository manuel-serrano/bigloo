/*=====================================================================*/
/*    serrano/prgm/project/bigloo/jigloo/jigloo.java                   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jan  1 17:24:51 2001                          */
/*    Last change :  Tue Oct 31 15:58:08 2006 (serrano)                */
/*    Copyright   :  2001-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Automatic Bigloo Java module clause generation (by               */
/*    instrospection of JVM class files).                              */
/*=====================================================================*/
import java.lang.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.*;

/*---------------------------------------------------------------------*/
/*    jigloo ...                                                       */
/*---------------------------------------------------------------------*/
public abstract class jigloo {
   static PrintStream out = System.out;
   static int verbose = 0;
   static boolean directives = true;
   static Hashtable arrays = new Hashtable();
   
   static boolean isInterface = false;

   static void verbose( int level, String s ) {
      if( verbose >= level ) {
	 System.err.println( s );
      }
   }

   static void emit( String s ) {
      out.print( s );
   }

   static String type_name( Class type ) {
      if( type.isArray() ) {
	 if( type.getComponentType() == byte.class )
	    return "bstring";
	 else
	    return type_name( type.getComponentType() ) + "*";
      }
	 
//      if ( type == Object.class )
//	 return "obj";
      if ( type == boolean.class )
	 return "bool";
      if ( type == byte.class )
	 return "char";
      if ( type == char.class )
	 return "ucs2";
      if ( type == long.class )
	 return "elong";
      if ( type.getName().equals("bigloo.pair") )
	 return "pair";
      if ( type.getName().equals("bigloo.procedure") )
	 return "procedure";
     return type.getName();
   }

   static void jigloo_type( Class type ) {
      if( type.isArray() ) jigloo_array( type );
      emit( "::" + type_name( type ) );
   }

   static void jigloo_args( Class[] args ) {
      emit( "(" );
      for( int i = 0; i < args.length; i++ ) {
	 jigloo_type( args[ i ] );
	 emit( " " );
      }
      emit( ") " );
   }

   static void jigloo_args( Class first, Class[] args ) {
      emit( "(" );
      jigloo_type( first );
      emit( " " );
      for( int i = 0; i < args.length; i++ ) {
	 jigloo_type( args[ i ] );
	 emit( " " );
      }
      emit( ") " );
   }

   static void jigloo_modifiers( int mod ) {
      // only emit abstract, if the current class is really an interface.
      if( isInterface && Modifier.isAbstract( mod ) ) emit( "abstract " );
      if( Modifier.isFinal( mod ) ) emit( "final " );
      if( Modifier.isInterface( mod ) ) emit( "interface " );
      if( Modifier.isNative( mod ) ) emit( "native " );
      if( Modifier.isPrivate( mod ) ) emit( "private " );
      if( Modifier.isProtected( mod ) ) emit( "protected " );
      if( Modifier.isPublic( mod ) ) emit( "public " );
      if( Modifier.isStatic( mod ) ) emit( "static " );
      if( Modifier.isStrict( mod ) ) emit( "strict " );
      if( Modifier.isSynchronized( mod ) ) emit( "synchronized " );
      if( Modifier.isTransient( mod ) ) emit( "transient " );
      if( Modifier.isVolatile( mod ) ) emit( "volatile " );
      emit( " " );
   }

   static void jigloo_constructor( Constructor constr, int override_index ) {
      emit( "     (constructor " );
      jigloo_modifiers( constr.getModifiers() );
      emit( constr.getName() );
      if( override_index != 0 )
	 emit( Integer.toString( override_index ) );
      emit( " " );
      jigloo_args( constr.getParameterTypes() );
      emit( ")\n" );
   }
    
   static void jigloo_field( Field field ) {
      emit( "     (field " );
      jigloo_modifiers( field.getModifiers() );
      emit( field.getName() );
      jigloo_type( field.getType() );
      emit( " \"" + field.getName() + "\")\n" );
   }

   static void jigloo_method( Class owner, Method method, Hashtable overrides ) {
      int mod = method.getModifiers();
      if( ! Modifier.isNative( mod ) ) {
	 Integer override_index= (Integer)overrides.get( method.getName() );
	 emit( "     (method " );
	 jigloo_modifiers( mod );
	 emit( method.getName() );
	 if( override_index.intValue() != 0 ) {
	    emit( override_index.toString() );
	    overrides.put( method.getName(),
			   new Integer( override_index.intValue() + 1 ) );
	 }
	 jigloo_type( method.getReturnType() );
	 emit( " " );
	 if( Modifier.isStatic( mod ) )
	    jigloo_args( method.getParameterTypes() );
	 else
	    jigloo_args( owner, method.getParameterTypes() );
	 emit( " \"" + method.getName() + "\")\n" );
      }
   }

   static void jigloo_array( Class an_array ) {
      Class component = an_array.getComponentType();
      arrays.put( type_name( an_array ), an_array );
      if( component.isArray() ) {
	 jigloo_array( component );
      }
   }

   static int jigloo_array_depth( Class an_array ) {
      int res = 0;
      Class item = an_array.getComponentType();

      while( item.isArray() ) {
	 res++;
	 item = item.getComponentType();
      }

      return res;
   }

   static void jigloo_emit_array( Class an_array ) {
      String tname = type_name( an_array );

      if( !tname.equals( "bstring" ) ) {
	 emit( "   (array " +  tname + " " );
	 jigloo_type( an_array.getComponentType() );
	 emit( ")\n" );
      }
   }

   static void jigloo_emit_arrays() {
      Object[] tab = arrays.values().toArray();
      int i = 0;
      int depth = 0;
      boolean empty = tab.length == 0;
	
      while( !empty ) {
	 empty = true;

	 for( i = 0; i < tab.length; i++ ) {
	    Object an_obj = tab[ i ];
	    if( an_obj != null ) {
	       Class a_class = (Class)an_obj;
	       if( jigloo_array_depth( a_class ) == depth ) {
		  jigloo_emit_array( a_class );
		  tab[ i ] = null;
	       } else {
		  empty = false;
	       }
	    }
	 }
	 depth++;
      }
   }

   static void jigloo_class( Class a_class ) {
      if( a_class.isArray() ) {
	 jigloo_array( a_class );
      } else {
	 Class the_super = a_class.getSuperclass(); 
	 String name = a_class.getName();
	 Class[] all_classes = a_class.getClasses();
	 Constructor[] all_constructors = a_class.getConstructors();
	 Field[] all_fields = a_class.getFields();
	 Method[] all_methods = a_class.getMethods();
	 Hashtable overrides = new Hashtable();

	 // handling overrides
	 for( int i = 0; i < all_methods.length; i++ ) {
	    Integer override_count = (Integer)overrides.get( all_methods[i].getName() );

	    overrides.put( all_methods[i].getName(),
			   new Integer( (override_count == null) ? 0 : 1 ) );
	 }


	 for( int i = 0; i < all_classes.length; i++ ) {
	    jigloo_class( all_classes[ i ] );
	 }
	    

	 int mod = a_class.getModifiers();
	 isInterface = (Modifier.isInterface(mod));

	 // start the class emission
	 if( isInterface ) {
            // must not do this for abstract classes!
	    // (otherwise java.lang.IncompatibleClassChangeError)
	    emit( "   (abstract-class " + name + "\n" );
	 } else {
	    emit( "   (class " + name + "\n" );
	 }

	 for( int i = 0; i < all_constructors.length; i++ ) {
	    jigloo_constructor( all_constructors[ i ],
				((all_constructors.length == 0) ? 0 : (i+1)) );
	 }

	 for( int i = 0; i < all_fields.length; i++ ) {
	    jigloo_field( all_fields[ i ] );
	 }

	 for( int i = 0; i < all_methods.length; i++ ) {
	    jigloo_method( a_class, all_methods[ i ], overrides );
	 }

	 // stop with this class
	 emit( "    \"" + name + "\")\n" );
      }
   }

   static void jigloo_file( String filename ) {
      verbose( 1, "  [" + filename + "]" );
      try {
	 jigloo_class( Class.forName( new String( filename ) ) );
      } catch( Throwable e ) {
	 System.err.println( "*** ERROR:jigloo:Can't open class -- " + filename );
      }
   }

   static String[] parse_args( String argv[] ) {
      String[] file = new String[ argv.length ];
      String[] res;
      int w = 0;
	
      for( int i = 0; i < argv.length; i++ ) {
	 if( argv[ i ].equals( "-help" ) ) {
	    System.err.println( "usage: jigloo [options] [src_name]*" );
	    System.err.println();
	    System.err.println( "  -o <name>            --  Name the output file <name>." );
	    System.err.println( "  -no-directives       --  Do not emit directives header." );
	    System.err.println( "  -v[23]               --  Be verbose." );
	    System.exit( 0 );
	 } else { 
	    if( argv[ i ].equals( "-no-directives" ) )
	       directives = false; 
	    else {
	       if( argv[ i ].equals( "-o" ) ) { 
		  try {
		     out = new PrintStream( new FileOutputStream( argv[ i + 1 ] ) );
		     i++;
		  } catch( java.io.FileNotFoundException e ) {
		     System.err.println( "*** ERROR:jigloo:Can't open file for output -- " + argv[ i + 1 ] );
		  } 
	       } else {
		  if( argv[ i ].equals( "-v" ) )
		     verbose = 1;
		  else {
		     if( argv[ i ].equals( "-v2" ) )
			verbose = 2; 
		     else {
			if( argv[ i ].equals( "-v3" ) )
			   verbose = 3;
			else {
			   file[ w ] = argv[ i ];
			   w++;
			}
		     }
		  }
	       }
	    }
	 }
      }
	
      res = new String[ w ];
      System.arraycopy( file, 0, res, 0, w );
      return res;
   }
		    

   public static void main( String argv[] ) {
      String file[] = parse_args( argv );

      if( directives ) emit( "(directives\n  (java\n" );

      for( int i = 0; i < file.length; i++ ) 
	 jigloo_file( file[ i ] );

      jigloo_emit_arrays();

      if( directives ) emit( "))\n" );
   }
}
