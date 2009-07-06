/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/dlopen.cs              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 11 15:42:09 2000                          */
/*    Last change :  Mon Apr 21 08:31:29 2008 (serrano)                */
/*    Copyright   :  2000-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Dynamic class loading for the .Net back-end.                     */
/*=====================================================================*/
using System;
using System.Reflection;

namespace bigloo
{
  /*---------------------------------------------------------------------*/
  /*    dlopen ...                                                       */
  /*---------------------------------------------------------------------*/
  public abstract class dlopen
  {
    private const String NO_ERROR_YET = "No error (yet)";

    private static String bgl_dload_error = NO_ERROR_YET;

    static String suffix( byte[]  filename )
    {
      int len = filename.Length;
      int i;

      for ( i= len-1 ; (0 <= i) && (filename[i] != '.') ; --i )
        ;

      if (0 <= i)
        return foreign.newstring( filename, 0, i );
      else
        return foreign.newstring( filename );
    }

     public static int dload( byte[] filename, byte[] init_sym, byte[] mod_sym ) 
    {
      bgl_dload_error= NO_ERROR_YET;

      try
      {
        // !!!!! faut-il réellement virer le suffixe ?????
        String type_name = suffix( filename );

        Type type = Type.GetType( type_name, true, false );

        if (!type.IsClass)
        {
          /***** type is not a class *****/
          bgl_dload_error= ("Type [" + type_name + "] is not a class.");
          return 1;
        }


	if( init_sym.Length > 0 ) {
	   MethodInfo init = type.GetMethod( foreign.newstring( init_sym ) );

	   init.Invoke( null, null );
	}
	
	if( mod_sym.Length > 0 ) {
	   MethodInfo init = type.GetMethod( foreign.newstring( mod_sym ) );

	   init.Invoke( null, null );
	}
	
	return 0;
      }
      catch (Exception e)
      {
        bgl_dload_error= e.Message;
        return 1;
      }
    }

    public static byte[] dload_error()
    {
      return foreign.getbytes( bgl_dload_error );
    }
  }
}
