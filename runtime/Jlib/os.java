package bigloo;

public class os
{
  public static final byte[] BGL_DEFAULT_A_OUT;
  public static final byte[] BGL_DEFAULT_A_BAT;
//  public static final byte FILE_SEPARATOR = (byte)java.io.File.separatorChar;
  // MS, 23 Jun 2006
  public static final byte FILE_SEPARATOR = (byte)'/';
  public static final byte PATH_SEPARATOR = (byte)java.io.File.pathSeparatorChar;
  public static final byte[] OS_CLASS;
  public static final byte[] OS_NAME = System.getProperty( "os.name" ).getBytes();
  public static final byte[] OS_ARCH = System.getProperty( "os.arch" ).getBytes();
  public static final byte[] OS_VERSION = System.getProperty( "os.version" ).getBytes();
  public static byte[] OS_TMP = null;
  public static final byte[] SHARED_LIB_SUFFIX;
  public static final byte[] STATIC_LIB_SUFFIX;
  public static final boolean UCS2_DISPLAYABLE = true;
  public static final byte[] OS_CHARSET = configure.OS_CHARSET;

  static
  {
    if (System.getProperty( "os.name" ).toLowerCase().startsWith( "windows" ))
    {
      /***** running on Win32 *****/
      BGL_DEFAULT_A_OUT = "a.exe".getBytes();
      BGL_DEFAULT_A_BAT = "a.bat".getBytes();
      OS_CLASS = "win32".getBytes();
      SHARED_LIB_SUFFIX = "dll".getBytes();
      STATIC_LIB_SUFFIX = "lib".getBytes();
      
      try {
	 OS_TMP = System.getProperty( "java.io.tmpdir", "C:\\Temp" ).getBytes();
      } catch( SecurityException _s ) {
	 OS_TMP = "".getBytes();
      }
    }
    else
    {
      /***** running on Unix *****/
      BGL_DEFAULT_A_OUT = "a.out".getBytes();
      BGL_DEFAULT_A_BAT = "a.out".getBytes();
      OS_CLASS = "unix".getBytes();
      SHARED_LIB_SUFFIX = "so".getBytes();
      STATIC_LIB_SUFFIX = "a".getBytes();
      
      try {
	 OS_TMP = System.getProperty( "java.io.tmpdir", "/tmp" ).getBytes();
      } catch( SecurityException _s ) {
	 OS_TMP = "".getBytes();
      }
    }
  }
}

