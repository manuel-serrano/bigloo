/*=====================================================================*/
/*    serrano/prgm/project/bigloo/5.0.x/runtime/Jlib/regexp.java       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Dec  7 11:40:40 2011                          */
/*    Last change :  Tue Jul  7 13:13:36 2026 (serrano)                */
/*    Copyright   :  2011-26 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Regular Expressions                                              */
/*=====================================================================*/

package bigloo;

import java.io.*;
import java.util.regex.*;

public class regexp extends obj {
   public byte[] pat;
   public Object preg;
   Pattern reg;
   static Pattern posixp = Pattern.compile("\\[:([a-z]+):\\]");
   
   public static byte[] substring(String o, int start, int end) {
      byte[] res = new byte[ end - start ];

      for (int i = start; i < end; i++) {
	 res[ i - start ] = (byte)o.charAt(i);
      }

      return res;
   }

   public static String posix2java(String o) {
      Matcher m = posixp.matcher(o);
      int i = 0;
      String res = "";

      if (!m.find()) {
	 return o;
      } else {
	 do {
	    String c0 = o.substring(m.start(0) + 2, m.start(0) + 3);
	    if (c0.equals("x")) {
	       String c1 = o.substring(m.start(0) + 3, m.start(0) + 4);
	       res += o.substring(i, m.start(0)) + "\\p{" +
		  c0.toUpperCase() +
		  c1.toUpperCase() +
		  o.substring(m.start(0) + 4, m.end(0) - 2) + "}";
	       i = m.end(0);
	    } else {
	       res += o.substring(i, m.start(0)) + "\\p{" +
		  c0.toUpperCase() +
		  o.substring(m.start(0) + 3, m.end(0) - 2) + "}";
	       i = m.end(0);
	    }
	 } while(m.find());
	 
	 return res + o.substring(i);
      }
   }
   
   public regexp(byte[] p, boolean comp) {
      pat = p;
      
      if (comp) {
	 reg = Pattern.compile(posix2java(new String(p)));
      }
   }

   public Object match(String cs , boolean stringp, int offset) {
      Matcher m = reg.matcher(cs);

      if (!m.find()) {
	 return foreign.BFALSE;
      } else {
	 int c = m.groupCount();
	 pair r = pair.cons(foreign.BNIL, foreign.BNIL);
	 pair t = r;

	 if (stringp) {
	    for (int i = 0; i < c + 1; i++) {
	       pair p;
	       if (m.group(i) != null) {
		  p = pair.cons(m.group(i).getBytes(), foreign.BNIL);
	       } else {
		  p = pair.cons(foreign.BFALSE, foreign.BNIL);
	       }
	       foreign.SET_CDR(t, p);
	       t = p;
	    }
	 } else {
	    for (int i = 0; i < c + 1; i++) {
	       pair p;
	       if (m.group(i) != null) {
		  p = pair.cons(pair.cons(
				    foreign.BINT(m.start(i) + offset),
				    foreign.BINT(m.end(i) + offset)),
				 foreign.BNIL);
				
	       } else {
		  p = pair.cons(foreign.BFALSE, foreign.BNIL);
	       }

	       foreign.SET_CDR(t, p);
	       t = p;
	    }
	 }

	 return foreign.CDR(r);
      }
   }

   public int match_n(String cs, Object[] v, int offset) {
      Matcher m = reg.matcher(cs);

      if (!m.find()) {
	 return -1;
      } else {
	 int c = m.groupCount();
	 int len = v.length & ~1;

	 for (int i = 0; i < c + 1 && i < len; i++) {
	    pair p;
	    if (m.group(i) != null) {
	       v[i * 2] = foreign.BINT(m.start(i) + offset);
	       v[i*2 + 1] = foreign.BINT(m.end(i) + offset);
	    } else {
	       v[i*2] = foreign.BINT(-1);
	       v[i*2 + 1] = foreign.BINT(-1);
	    }
	 }

	 return c + 1;
      }
   }
   
   
   public void write(final output_port p) {
      p.write("#<regexp:" + pat + ">");
   }
}
 
