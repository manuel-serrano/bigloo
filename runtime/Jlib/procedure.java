package bigloo;

// For space saving we can make a PROCEDURE_LIGHT without arity...

public class procedure extends obj {
    public int index;     // used as entry point
    public int arity;
    public Object eval;
    public Object[] env;

    public static final bigloo.nil nil= bigloo.nil.nil;

    public procedure() {
    }

    public procedure(int index, int arity) {
	this.arity = arity;
	this.eval = unspecified.unspecified;
	this.index = index;
	this.env = new Object[0];
    }

    public procedure(int index, int arity, Object[] env) {
	this.arity = arity;
	this.eval = unspecified.unspecified;
	this.index = index;
	this.env = env;
    }

    public procedure(int index, int arity, int size) {
	this.arity = arity;
	this.eval = unspecified.unspecified;
	this.index = index;
	this.env = new Object[size];
    }

    private Object error(int n) {
	// [BPS] This is too violent. Use throw(new bexception(..).
	foreign.Error("Wrong number of arguments (" + n + ") for a function of "
		      + this.getClass().getName()
		      + " with index " + index + " and arity " + arity );
	return(null);
    }

    public Object apply(Object args) {
	return(error(-1));
    }

    public Object funcall0() {
	return(error(0));
    }

    public Object funcall1(Object a1) {
	return(error(1));
    }

    public Object funcall2(Object a1, Object a2) {
	return(error(2));
    }

    public Object funcall3(Object a1, Object a2, Object a3) {
	return(error(3));
    }

    public Object funcall4(Object a1, Object a2, Object a3, Object a4){
      return(error(4));
    }

    public void write(output_port p) {
	p.write("#<procedure in " + this.getClass().getName() +
		" index " + index +
		" arity " + arity + ">");
    }
}
