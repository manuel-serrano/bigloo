using System;

namespace bigloo
{
  public class weakptr: obj
  {
    public WeakReference mref;
    
    public weakptr( Object  data)
    {
      this.mref = new WeakReference(data);
    }

    public Object getData(){
      Object d = mref.Target;
      return d == null ? unspecified._unspecified : d;
    }

    public void setData(Object o){
      mref = new WeakReference(o);
    }

    public override void write( output_port p ) {
      p.write("#<weakptr:");
      foreign.write_object(getData(), p);
      p.write(">");
    }
  }
}
