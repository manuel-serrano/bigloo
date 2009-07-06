/*=====================================================================*/
/*    serrano/prgm/project/bigloo/examples/DNet/lib.cs                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jan  9 07:39:50 2008                          */
/*    Last change :  Wed Jan  9 08:29:39 2008 (serrano)                */
/*    Copyright   :  2008 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Simple C# lib.                                                   */
/*=====================================================================*/
using System;
using System.IO;
using Bigloo;
using Bigloo.foreign;

public class lib {
   Object proc;

   public lib( bigloo.procedure p ) {
      Console.Out.WriteLine( "C#: constructor..." );
      proc = p;
   }

   static public Boolean init() {
      Console.Out.WriteLine( "C#: init..." );

      return true;
   }
   
   public static String bytesToString( byte[] b ) {
      return (new System.Text.ASCIIEncoding()).GetString( b );
   }
   
   public static byte[] showString( String s ) {
      int len = s.Length;
      byte[] r = new byte[len];
      
      Console.Out.WriteLine( "C#: show string [" + s + "]" );
      for ( int i= 0 ; i < len ; ++i ) r[ i ]= (byte)s[ i ];
      
      return r;
   }
   
   public Object invoke( byte[] s ) {
      Object res;
	 
      Console.Out.WriteLine( "C#: invoking Bigloo closure..." );
      res = ((bigloo.procedure)proc).funcall1( s );
      Console.Out.WriteLine( "C#: Bigloo call complete." );

      return res;
   }

   
}
