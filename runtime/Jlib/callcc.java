package bigloo;

public class callcc extends procedure {
    public callcc() {
	arity= 1;
    }

    public Object apply(Object args) {
	throw new bexception( bgldynamic.abgldynamic.get().exitd_top, ((pair)args).car );
    }

    public Object funcall1(Object a1) {
	throw(new bexception(bgldynamic.abgldynamic.get().exitd_top, a1));
    }
}
