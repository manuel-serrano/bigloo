using System;

namespace bigloo
{
  // Since I can't call back Scheme from C# I have to do some IL by hand..
  public class callback 
  {
    public virtual void failure( Object o, Object m, Object e )
    {
      foreign.Error( "failure" );
    }

    public virtual bool mangledp( byte[] id )
    {
      return false;
    }

    public virtual Object demangle( byte[] id )
    {
      return id;
    }

    public virtual Object exit_apply( Object v )
    {
      return null;
    }

    public virtual byte[] obj_to_string( Object v )
    {
      return null;
    }

    public virtual Object string_to_obj( byte[] v )
    {
      return null;
    }
  }
}
