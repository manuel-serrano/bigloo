package bigloo;

import java.lang.ref.WeakReference;


public class weakptr extends obj {
  public WeakReference data;
  public WeakReference ref;

  public weakptr(final Object d, final Object r){
     data = new WeakReference(d);
     ref = new WeakReference(r);
  }

  public Object getData(){
    Object d = ref.get();
    return d == null ? unspecified.unspecified : d;
  }

  public void setData(Object o){
    data = new WeakReference(o);
  }

  public Object getRef(){
    Object d = data.get();
    return d == null ? unspecified.unspecified : d;
  }

  public void setRef(Object o){
    ref = new WeakReference(o);
  }

  public void write( final output_port p ) {
    p.write("#<weakptr:");
    foreign.write_object(getData(), p);
    p.write(">");
  }
}
