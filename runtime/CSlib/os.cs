using System;
using System.IO;

namespace bigloo
{
  public class os
  {
    public static readonly byte[] BGL_DEFAULT_A_OUT;
    public static readonly byte[] BGL_DEFAULT_A_BAT;
     //public static readonly byte FILE_SEPARATOR= (byte)Path.DirectorySeparatorChar;
     // MS, 23 Jun 2006
    public static readonly byte FILE_SEPARATOR= (byte)'/';
    public static readonly byte PATH_SEPARATOR= (byte)Path.PathSeparator;
    public static readonly byte[] OS_CLASS;
    public static readonly byte[] OS_NAME;
    public static readonly byte[] OS_ARCH= foreign.getbytes( "???" );     // !!!!! TO DO !!!!!
    public static readonly byte[] OS_VERSION;
    public static readonly byte[] OS_TMP= foreign.getbytes( Path.GetTempPath() );
    public static readonly byte[] SHARED_LIB_SUFFIX;
    public static readonly byte[] STATIC_LIB_SUFFIX;
    public static readonly bool UCS2_DISPLAYABLE= true;
    public static readonly byte[] OS_CHARSET = configure.OS_CHARSET;

    static os()
    {
      try {
        // Environment.OSVersion not implemented in Rotor
        OS_NAME= GetOSName();
      }
      catch {
        OS_NAME= foreign.getbytes( "???" );
      }

      try {
        // Environment.OSVersion not implemented in Rotor
        OS_VERSION= GetOSVersion();
      }
      catch {
        OS_VERSION= foreign.getbytes( "???" );
      }

      bool              unix= false;

      try
      {
        // Environment.OSVersion not implemented in Rotor
        unix= IsUnix();         
      }
      catch {
      }

      if (!unix)
      {
        /***** running on Win32 *****/
        BGL_DEFAULT_A_OUT= foreign.getbytes( "a.exe" );
        BGL_DEFAULT_A_BAT= foreign.getbytes( "a.bat" );
        OS_CLASS= foreign.getbytes( "win32" );
        SHARED_LIB_SUFFIX= foreign.getbytes( "dll" );
        STATIC_LIB_SUFFIX= foreign.getbytes( "lib" );
      }
      else
      {
        /***** running on Unix *****/
        BGL_DEFAULT_A_OUT= foreign.getbytes( "a.out" );
        BGL_DEFAULT_A_BAT= foreign.getbytes( "a.out" );
        OS_CLASS= foreign.getbytes( "unix" );
        SHARED_LIB_SUFFIX= foreign.getbytes( "so" );
        STATIC_LIB_SUFFIX= foreign.getbytes( "a" );
      }
    }

    private static byte[] GetOSName()
    {
      return foreign.getbytes( Environment.OSVersion.ToString() );
    }

    private static byte[] GetOSVersion()
    {
      return foreign.getbytes( Environment.OSVersion.Version.ToString() );
    }

    private static bool IsUnix()
    {
      PlatformID      platform= Environment.OSVersion.Platform;

      return (   (platform != PlatformID.Win32NT)
              && (platform != PlatformID.Win32S)
              && (platform != PlatformID.Win32Windows));
      }
  }
}
