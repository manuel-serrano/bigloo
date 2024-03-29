/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Jlib/bclass.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 28 10:19:34 2012                          */
/*    Last change :  Sun Nov 14 13:03:20 2021 (serrano)                */
/*    Copyright   :  2012-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Java Bigloo class implementation                                 */
/*=====================================================================*/

package bigloo;

public class bclass extends obj {
   public symbol name;
   public procedure alloc_fun;
   public Object new_fun;
   public int hash;
   public procedure nil_fun;
   public Object nil;
   public Object constructor;
   public Object[] virtual_fields;
   public Object shrink;
   public Object[] direct_fields;
   public Object[] all_fields;
   public symbol module;
   public int index;
   public int depth;
   public Object evdata;
   public Object bsuper;
   public Object subclasses;
   public Object[] ancestors;

   public bclass( symbol name, symbol module, int num,
		  Object bsuper, Object sub,
		  procedure alloc_fun, int hash,
		  Object[] fd, Object[] allfd,
		  Object constr, Object[] virt, Object new_fun,
		  procedure nil_fun, Object shrink,
		  int depth, 
		  Object evdata ) {
      this.name = name;
      this.module = module;
      this.index = num;
      this.bsuper = bsuper;
      this.subclasses = sub;
      this.alloc_fun = alloc_fun;
      this.hash = hash;
      this.direct_fields = fd;
      this.all_fields = allfd;
      this.constructor = constr;
      this.virtual_fields = virt;
      this.new_fun = new_fun;
      this.nil_fun = nil_fun;
      this.shrink = shrink;
      this.depth = depth;
      this.nil = foreign.BFALSE;
      this.evdata = evdata;
      this.ancestors = new Object[depth + 1];

      if( depth > 0 ) {
	 bclass sup = (bclass)bsuper;
	 System.arraycopy( sup.ancestors, 0, this.ancestors, 0, depth );
      }
      this.ancestors[depth] = this;
   }
}
