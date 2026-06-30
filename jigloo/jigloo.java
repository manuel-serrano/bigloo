/*=====================================================================*/
/*    serrano/prgm/project/bigloo/5.0.x/jigloo/jigloo.java             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jan  1 17:24:51 2001                          */
/*    Last change :  Tue Jun 30 08:31:32 2026 (serrano)                */
/*    Copyright   :  2001-26 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Automatic Bigloo Java module clause generation (by               */
/*    instrospection of JVM class files).                              */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The module                                                       */
/*---------------------------------------------------------------------*/
import java.lang.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.*;

/*---------------------------------------------------------------------*/
/*    jigloo ...                                                       */
/*---------------------------------------------------------------------*/
public abstract class jigloo {
   static String target = null;
   static PrintStream out = System.out;
   static int verbose = 0;
   static boolean directives = true;
   static Hashtable arrays = new Hashtable();
   static int moduleVersion = 5;
   static boolean stripPackage = false;
   static boolean pkgEmitted = false;
   static boolean superEmit = false;
   static boolean inheritance = true;
   
   static boolean isInterface = false;

   static void verbose(int level, String s) {
      if (verbose >= level) {
	 System.err.println(s);
      }
   }

   static void emit(String s) {
      out.print(s);
   }

   static String type_name(Class type, String pkg) {
      if (type.isArray()) {
	 if (type.getComponentType() == byte.class) {
	    return "bstring";
	 } else {
	    return type_name(type.getComponentType(), pkg) + "*";
	 }
      }
	 
//      if ( type == Object.class)
//	 return "obj";
      if (type == boolean.class)
	 return "bool";
      if (type == byte.class)
	 return "char";
      if (type == char.class)
	 return "ucs2";
      if (type == long.class)
	 return "elong";
      if (type.getName().equals("bigloo.pair"))
	 return "pair";
      if (type.getName().equals("bigloo.procedure"))
	 return "procedure";
      if (type.getName().equals("java.lang.String"))
	 return "String";
      if (type.getName().equals("java.lang.Object"))
	 return "obj";
      if (type.getName().equals("java.lang.Runnable"))
	 return "java.lang.Runnable";
      if (type.getName().equals("java.lang.CharSequence"))
	 return "CharSequence";
      if (type.getName().startsWith("java.lang"))
	 return "obj";
      return unpackage(type.getName(), pkg);
   }

   static String type_descr(Class type) {
      if (type.isArray()) {
	 if (type.getComponentType() == byte.class) {
	    return "bstring";
	 } else {
	    return "A" + type_descr(type.getComponentType());
	 }
      }
	 
      if (type == boolean.class)
	 return "Z";
      if (type == byte.class)
	 return "B";
      if (type == char.class)
	 return "C";
      if (type == long.class)
	 return "J";
      if (type == int.class)
	 return "I";
      if (type == short.class)
	 return "S";
      if (type == void.class)
	 return "V";
      if (type == float.class)
	 return "F";
      if (type == double.class)
	 return "D";
      if (type.getName().equals("bigloo.pair"))
	 return "P";
      if (type.getName().equals("bigloo.procedure"))
	 return "F";
      if (type.getName().equals("java.lang.String"))
	 return "S";
      if (type.getName().equals("java.lang.Object"))
	 return "O";
      if (type.getName().equals("java.lang.Runnable"))
	 return "R";
      if (type.getName().equals("java.lang.CharSequence"))
	 return "CS";
      if (type.getName().startsWith("java.lang"))
	 return "L" + type.getName().replace('.', '/');
      return "O";
   }

   static String types_descr(Class[] types) {
      String res = "";
      int i;
      
      for (i = 0; i < types.length; i++) {
	 res += type_descr(types[i]);
      }

      return res;
   }

   static void jigloo_type(Class type, String pkg) {
      if (type.isArray()) {
	 jigloo_array(type);
      }
      emit("::" + type_name(type, pkg));
   }

   static void jigloo_args(Class[] args, String pkg) {
      for (int i = 0; i < args.length - 1; i++) {
	 jigloo_type(args[i], pkg);
	 emit(" ");
      }
	 jigloo_type(args[args.length - 1], pkg);
   }

   static void jigloo_args(Class first, Class[] args, String pkg) {
      jigloo_type(first, pkg);
      emit(" ");
      for (int i = 0; i < args.length - 1; i++) {
	 jigloo_type(args[i], pkg);
	 emit(" ");
      }
      jigloo_type(args[args.length - 1], pkg);
   }

   static void jigloo_modifiers(int mod) {
      // only emit abstract, if the current class is really an interface.
      String buf = "";
      if (isInterface && Modifier.isAbstract(mod)) buf += "abstract ";
      if (Modifier.isFinal(mod)) buf += "final ";
      if (Modifier.isInterface(mod)) buf += "interface ";
      if (Modifier.isNative(mod)) buf += "native ";
      if (Modifier.isPrivate(mod)) buf += "private ";
      if (Modifier.isProtected(mod)) buf += "protected ";
      if (Modifier.isPublic(mod)) buf += "public ";
      if (Modifier.isStatic(mod)) buf += "static ";
      if (Modifier.isStrict(mod)) buf += "strict ";
      if (Modifier.isSynchronized(mod)) buf += "synchronized ";
      if (Modifier.isTransient(mod)) buf += "transient ";
      if (Modifier.isVolatile(mod)) buf += "volatile ";
      if (buf.length() != 0) {
	 emit(buf);
      }
   }

   static void jigloo_constructor4(Constructor constr, int override_index, String pkg) {
      emit("     (constructor ");
      jigloo_modifiers(constr.getModifiers());
      emit(unpackage(constr.getName(), pkg));
      if (override_index != 0)
	 emit(Integer.toString(override_index));
      emit(" (");
      jigloo_args(constr.getParameterTypes(), pkg);
      emit("))\n");
   }
    
   static void jigloo_constructor5(Constructor constr, int override_index, String pkg) {
      Class[] args = constr.getParameterTypes();
      emit("     (");
      jigloo_modifiers(constr.getModifiers());
      
      emit("constructor ");

      emit(unpackage(constr.getName(), pkg));
      if ((override_index != 0) && (args.length > 0)) {
	 emit("_");
	 emit(types_descr(args));
      }

      if (args.length > 0) {
	 emit(" ");
	 jigloo_args(args, pkg);
      }
      
      emit(")\n");
   }
    
   static void jigloo_constructor(Constructor constr, int override_index, String pkg) {
      if (moduleVersion == 4) {
	 jigloo_constructor4(constr, override_index, pkg);
      } else {
	 jigloo_constructor5(constr, override_index, pkg);
      }
   }
   
   static void jigloo_field(Field field, String pkg) {
      emit("     (field ");
      jigloo_modifiers(field.getModifiers());
      emit(field.getName());
      jigloo_type(field.getType(), pkg);
      if (moduleVersion == 4) {
	 emit(" \"" + field.getName() + "\"");
      }
      emit (")\n");
   }

   static void jigloo_method4(Class owner, Method method, Hashtable overrides, String pkg) {
      int mod = method.getModifiers();
      if (! Modifier.isNative(mod)) {
	 Integer override_index= (Integer)overrides.get(method.getName());
	 emit("     (method ");
	 jigloo_modifiers(mod);
	 emit(method.getName());
	 if (override_index.intValue() != 0) {
	    emit(override_index.toString());
	    overrides.put(method.getName(),
			  Integer.valueOf(override_index.intValue() + 1));
	 }
	 jigloo_type(method.getReturnType(), pkg);
	 emit(" ");
	 if (Modifier.isStatic(mod))
	    jigloo_args(method.getParameterTypes(), pkg);
	 else
	    jigloo_args(owner, method.getParameterTypes(), pkg);
	 emit(" \"" + method.getName() + "\")\n");
      }
   }

   static void jigloo_method5(Class owner, Method method, Hashtable overrides, String pkg) {
      int mod = method.getModifiers();
      Class[] args = method.getParameterTypes();
      
      if (! Modifier.isNative(mod)) {
	 Integer override_index = (Integer)overrides.get(method.getName());
	 emit("     (");
	 jigloo_modifiers(mod);
	 
	 emit(method.getName());
	 if ((override_index.intValue() != 0) && (args.length > 0)) {
	    emit("_");
	    emit(types_descr(args));
	    emit("_");
	    emit(type_descr(method.getReturnType()));
	 }
	 jigloo_type(method.getReturnType(), pkg);

	 if (args.length > 0) {
	    emit(" ");
	    jigloo_args(args, pkg);
	 } 
	 
	 if (override_index.intValue() != 0) {
	    emit(" \"" + method.getName() + "\"");
	 }
	 
	 emit(")\n");
      }
   }

   static void jigloo_method(Class owner, Method method, Hashtable overrides, String pkg) {
      if (moduleVersion == 4) {
	 jigloo_method4(owner, method, overrides, pkg);
      } else {
	 jigloo_method5(owner, method, overrides, pkg);
      }
   }
   
   static void jigloo_array(Class an_array) {
      Class component = an_array.getComponentType();
      arrays.put(type_name(an_array, null), an_array);
      if (component.isArray()) {
	 jigloo_array(component);
      }
   }

   static int jigloo_array_depth(Class an_array) {
      int res = 0;
      Class item = an_array.getComponentType();

      while(item.isArray()) {
	 res++;
	 item = item.getComponentType();
      }

      return res;
   }

   static void jigloo_emit_array(Class an_array, String pkg) {
      String tname = type_name(an_array, null);

      if (!tname.equals("bstring")) {
	 emit("   (array " +  tname + " ");
	 jigloo_type(an_array.getComponentType(), pkg);
	 emit(")\n");
      }
   }

   static void jigloo_emit_arrays() {
      Object[] tab = arrays.values().toArray();
      int i = 0;
      int depth = 0;
      boolean empty = tab.length == 0;
	
      while(!empty) {
	 empty = true;

	 for (i = 0; i < tab.length; i++) {
	    Object an_obj = tab[i];
	    if (an_obj != null) {
	       Class a_class = (Class)an_obj;
	       if (jigloo_array_depth(a_class) == depth) {
		  jigloo_emit_array(a_class, null);
		  tab[i] = null;
	       } else {
		  empty = false;
	       }
	    }
	 }
	 depth++;
      }
   }

   static String unpackage(String name, String pkg) {
      if (pkg != null && name.startsWith(pkg)) {
	 return name.substring(pkg.length() + 1);
      } else {
	 return name;
      }
   }

   static String classTypedName(Class a_class) {
      String pkg = stripPackage ? a_class.getPackage().getName() : null;
      String name = a_class.getName();

      if (!superEmit) {
	 return name;
      } else {
	 String cname = name;
	 Class the_super = a_class.getSuperclass();

	 if (the_super == null) {
	    return cname;
	 } else {
	    String super_name = the_super.getName();
	    String spkg = the_super.getPackage().getName();
	    String sup = super_name;

	    if (sup.equals("Object")) {
	       return cname;
	    } else {
	       return cname + "::" + sup;
	    }
	 }
      }
   }
      
   static String classTypedName_TBR(Class a_class) {
      String pkg = stripPackage ? a_class.getPackage().getName() : null;
      String name = a_class.getName();

      if (!superEmit) {
	 return unpackage(name, pkg);
      } else {
	 String cname = unpackage(name, pkg);
	 Class the_super = a_class.getSuperclass();

	 if (the_super == null) {
	    return cname;
	 } else {
	    String super_name = the_super.getName();
	    String spkg = the_super.getPackage().getName();
	    String sup = unpackage(super_name, spkg);

	    if (sup.equals("Object")) {
	       return cname;
	    } else {
	       return cname + "::" + sup;
	    }
	 }
      }
   }
      
   static void jigloo_class(Class a_class) {
      if (a_class.isArray()) {
	 jigloo_array(a_class);
      } else {
	 Class the_super = a_class.getSuperclass();
	 String name = a_class.getName();
	 String pkg = stripPackage ? a_class.getPackage().getName() : null;
	 String cname = unpackage(name, pkg);
	 Class[] all_classes = a_class.getDeclaredClasses();
	 Constructor[] all_constructors =
	    inheritance ? a_class.getConstructors() : a_class.getDeclaredConstructors();
	 Field[] all_fields =
	    inheritance ? a_class.getFields() : a_class.getDeclaredFields();
	 Method[] all_methods =
	    inheritance ? a_class.getMethods() : a_class.getDeclaredMethods();
	 Hashtable overrides = new Hashtable();

	 // handling overrides
	 for (int i = 0; i < all_methods.length; i++) {
	    Integer override_count =
	       (Integer)overrides.get(all_methods[i].getName());

	    overrides.put(all_methods[i].getName(),
			  Integer.valueOf((override_count == null) ? 0 : 1));
	 }


	 for (int i = 0; i < all_classes.length; i++) {
	    jigloo_class(all_classes[i]);
	 }
	    

	 int mod = a_class.getModifiers();
	 isInterface = (Modifier.isInterface(mod));

	 // start the class emission
	 if (isInterface) {
            // must not do this for abstract classes!
	    // (otherwise java.lang.IncompatibleClassChangeError)
	    emit("   (abstract-class " + classTypedName(a_class) + "\n");
	 } else {
	    emit("   (class " + classTypedName(a_class) + "\n");
	 }

	 for (int i = 0; i < all_constructors.length; i++) {
	    jigloo_constructor(
	       all_constructors[i],
	       ((all_constructors.length == 1) ? 0 : (i+1)),
	       pkg);
	 }

	 for (int i = 0; i < all_fields.length; i++) {
	    jigloo_field(all_fields[i], pkg);
	 }

	 for (int i = 0; i < all_methods.length; i++) {
	    jigloo_method(a_class, all_methods[i], overrides, pkg);
	 }

	 // stop with this class
	 if (moduleVersion == 4) {
	    emit("     \"" + name + "\")\n");
	 } else {
	    emit("     )\n");
	 }
      }
   }

   static void jigloo_file(String filename) throws Exception {
      verbose(1, "  [" + filename + "]");
      try {
	 jigloo_class(Class.forName(new String(filename)));
      } catch (Throwable e) {
	 System.err.println("\n*** ERROR:jigloo:Cannot open class -- " + filename);
	 throw e;
      }
   }

   static String[] parse_args(String argv[]) throws Exception {
      String[] file = new String[argv.length];
      String[] res;
      int w = 0;
	
      for (int i = 0; i < argv.length; i++) {
	 if (argv[i].equals("-help") || (argv[i].equals("--help"))) {
	    System.err.println("usage: jigloo [-cp path] [options] [src_name]*");
	    System.err.println();
	    System.err.println("  -o <name>            --  Name the output file <name>.");
	    System.err.println("  -v[23]               --  Verbosity.");
	    System.err.println("  --module5            --  Generate module5 syntax (default).");
	    System.err.println("  --module4            --  Generate module4 syntax.");
	    System.err.println("  -n|--no-directives   --  Do not emit directives header.");
	    System.err.println("  -s|--strip-package   --  Remove the class package component.");
	    System.err.println("  -u|--super-class     --  Add super class annotation.");
	    System.err.println("  -d|--direct          --  Include only fields and methods of the class.");
	    System.err.println("");
	    System.err.println("Example: jigloo -cp obj org.foo.Utils");
	    System.exit(0);
	 } else { 
	    if (argv[i].equals("--no-directives") || argv[i].equals("-n")) {
	       directives = false;
	    } else if (argv[i].equals("--module5")) {
	       moduleVersion = 5;
	    } else if (argv[i].equals("--module4")) {
	       moduleVersion = 4;
	    } else if (argv[i].equals("--strip-package") || argv[i].equals("-s")) {
	       stripPackage = true;
	    } else if (argv[i].equals("--super-class") || argv[i].equals("-u")) {
	       superEmit = true;
	    } else if (argv[i].equals("--direct") || argv[i].equals("-d")) {
	       inheritance = false;
	    } else {
	       if (argv[i].equals("-o")) { 
		  try {
		     target = argv[i + 1];
		     out = new PrintStream(new FileOutputStream(target));
		     i++;
		  } catch(java.io.FileNotFoundException e) {
		     System.err.println("*** ERROR:jigloo:Cannot open file for output -- " + argv[i + 1]);
		     throw (e);
		  } 
	       } else {
		  if (argv[i].equals("-v"))
		     verbose = 1;
		  else {
		     if (argv[i].equals("-v2"))
			verbose = 2; 
		     else {
			if (argv[i].equals("-v3"))
			   verbose = 3;
			else {
			   file[w] = argv[i];
			   w++;
			}
		     }
		  }
	       }
	    }
	 }
      }
	
      res = new String[w];
      System.arraycopy(file, 0, res, 0, w);
      return res;
   }
		    

   public static void main(String argv[]) {
      try {
	 String file[] = parse_args(argv);
	 
	 if (directives) {
	    if (moduleVersion == 4) {
	       emit("(directives\n  (java\n");
	    } else {
	       emit("(extern \"java\"\n");
	    }
	 }

	 for (int i = 0; i < file.length; i++) 
	    jigloo_file(file[i]);

	 jigloo_emit_arrays();

	 if (directives) {
	    if (moduleVersion == 4) {
	       emit("))\n");
	    } else {
	       emit(")\n");
	    }
	 }
      } catch (Throwable e) {
	 e.printStackTrace();

	 if (target != null) {
	    File f = new File(target);
	    if (f.exists()) {
	       f.delete();
	    }
	 }
	 System.exit(1);
      }
   }
}
