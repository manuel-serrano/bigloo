using System;

namespace bigloo {
  public class procedure: obj {
    public int index;     // use as entry point
    public int arity;
    public Object eval;
    public Object[] env;

    public procedure() {
    }

    public procedure(int index, int arity) {
      this.index= index;
      this.eval= unspecified._unspecified;
      this.arity= arity;
      this.env= new Object[0];
    }

    public procedure(int index, int arity, Object[]  env) {
      this.index= index;
      this.eval= unspecified._unspecified;
      this.arity= arity;
      this.env= env;
    }

    public procedure(int index, int arity, int size) {
      this.index= index;
      this.eval= unspecified._unspecified;
      this.arity= arity;
      this.env= new Object[size];
    }

    public virtual Object error(int n) {
      foreign.Error("Wrong number of arguments (" + n + ") for a function of "
		    + GetType()
		    + " with index " + index + " and arity " + arity );
      return(null);
    }
      
    public virtual Object apply(Object args ) {
      return(error(-1));
    }

    public virtual Object funcall0() {
      return(error(0));
    }

    public virtual Object funcall1(Object a1) {
      return(error(1));
    }

    public virtual Object funcall2(Object a1, Object a2) {
      return(error(2));
    }

    public virtual Object funcall3(Object a1, Object a2, Object a3) {
      return(error(3));
    }

    public virtual Object funcall4(Object a1, Object a2, Object a3, Object a4){
      return(error(4));
    }

    public override void write(output_port p) {
      p.write("#<procedure:" + arity + "(" + index + ":" + env.Length + ")>");
    }
  }
}
