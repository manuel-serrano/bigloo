package bigloo;

// For space saving we can make a PROCEDURE_LIGHT without arity...

public class generic extends procedure {
   public procedure proc;

   public generic() {
   }

   public generic(int index, int arity, int size, procedure proc) {
      this.index = index;
      this.eval = unspecified.unspecified;
      this.arity = arity;
      this.env = new Object[size];
      this.proc = proc;
   }
     
   public Object apply(Object args ) {
      return(proc.apply(args));
   }

   public Object funcall0() {
      return(proc.funcall0());
   }

   public Object funcall1(Object a1) {
      return(proc.funcall1(a1));
   }

   public Object funcall2(Object a1, Object a2) {
      return(proc.funcall2(a1, a2));
   }

   public Object funcall3(Object a1, Object a2, Object a3) {
      return(proc.funcall3(a1, a2, a3));
   }

   public Object funcall4(Object a1, Object a2, Object a3, Object a4){
      return(proc.funcall4(a1, a2, a3, a4));
   }
}

   
