package bigloo;

import java.lang.ref.WeakReference;


public class weakptr extends obj {
  public WeakReference ref;

  public weakptr(final Object data){
    ref = new WeakReference(data);
  }

  public Object getData(){
    Object d = ref.get();
    return d == null ? unspecified.unspecified : d;
  }

  public void setData(Object o){
    ref = new WeakReference(o);
  }

  public void write( final output_port p ) {
    p.write("#<weakptr:");
    foreign.write_object(getData(), p);
    p.write(">");
  }
}