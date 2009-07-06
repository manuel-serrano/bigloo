using System;

namespace bigloo
{
  public sealed class callcc: procedure
  {
    public callcc()
    {
      arity= 1;
    }

    public override Object apply( Object  args )
    {
      throw new bexception( bgldynamic.abgldynamic.get().exitd_top, ((pair)args).car );
    }

    public override Object funcall1(Object a1) {
	throw new bexception(bgldynamic.abgldynamic.get().exitd_top, a1);
    }
  }
}
