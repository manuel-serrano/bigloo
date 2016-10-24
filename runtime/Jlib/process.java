/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/process.java            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec  8 07:55:39 2000                          */
/*    Last change :  Mon Oct 24 13:46:07 2016 (serrano)                */
/*    Copyright   :  2000-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The simplified JVM Bigloo process implementation.                */
/*=====================================================================*/
package bigloo;

import java.io.IOException;
import java.lang.Process;
import java.util.Vector;

/*---------------------------------------------------------------------*/
/*    PROCESS ...                                                      */
/*---------------------------------------------------------------------*/
public class process extends obj
{
/*--- static fields ---------------------------------------------------*/
  private static int MAX_PROC_NUM = 255;
  private static Object[] proc_arr;
  private static process nil = new process();

/*--- non static fields -----------------------------------------------*/
  private final Process process;
  private final int index;
  private final Vector flushers;
  public obj input_port;
  public obj output_port;
  public obj error_port;

/*--- class constructor -----------------------------------------------*/
  static
  {
    final byte[] env = foreign.getenv( "BIGLOOLIVEPROCESS".getBytes() );

    if (env != null)
    {
      final int num = foreign.parseint( env, 0, env.length, 10 );

      if (0 < num)
        MAX_PROC_NUM = num;
    }

    proc_arr = new Object[MAX_PROC_NUM];

    for ( int i = 0 ; i < MAX_PROC_NUM ; ++i )
      proc_arr[i] = bigloo.foreign.BUNSPEC;
  }

/*--- nil constructor -------------------------------------------------*/
   public static process nil()
   {
      return nil;
   }
      
/*--- nil instance constructor ----------------------------------------*/
   public process()
   {
      process = null;
      index = 0;
      flushers = null;
   }
   
/*--- instance constructor --------------------------------------------*/
  public process( final byte[] host,
                  final boolean fork,
                  final boolean waiting,
                  final Object binput,
                  Object boutput,
                  Object berror,
                  final byte[] bcommand,
                  Object bargs,
                  final Object benv )
    throws IOException
  {
    Object aux;
    int env_len = 0;

    // we purge the process table and ask for a new index */
    PURGE_PROCESS_TABLE();
    index = find_process();
    if (index < 0)
      bigloo.runtime.Llib.error.the_failure( "run-process".getBytes(),
                                             "too many processes".getBytes(),
                                             bigloo.foreign.BUNSPEC );

    // converting "null:" keywords to null file names
    if ((boutput instanceof keyword) &&
        (foreign.KEYWORD_TO_STRING((keyword)boutput ).toString().equals("null:" )))
       boutput = (foreign.bigloo_strcmp( os.OS_CLASS, "unix".getBytes() )
		  ? "/dev/null"
		  : "NUL:" ).getBytes();
    if ((berror instanceof keyword) &&
        (foreign.KEYWORD_TO_STRING( (keyword)berror ).toString().equals( "null:" )))
       berror= (foreign.bigloo_strcmp( os.OS_CLASS, "unix".getBytes() )
		? "/dev/null"
		: "NUL:" ).getBytes();

    // building the command arguments
    final Vector cmd_vector = new Vector();

    if (host != null)
    {
      cmd_vector.add( "rsh" );
      cmd_vector.add( new String( host ) );
    }
    else
    {
      if (System.getProperty( "os.name" ).toLowerCase().startsWith( "windows" ))
      {
        cmd_vector.add( "cmd.exe" );
        cmd_vector.add( "/C" );
      }
      else
      {
        cmd_vector.add( "sh" );
        cmd_vector.add( "-c" );
      }
    }

    {
       String cmd = new String( bcommand );
       while (bargs instanceof pair)
       {
	  cmd += " \"" + new String( (byte[])(((pair)bargs).car) ) + "\"";
	  bargs = ((pair)bargs).cdr;
       }
       
    
/*     cmd_vector.add( new String( bcommand ) );                       */
/*     while (bargs instanceof pair)                                   */
/*     {                                                               */
/*       cmd_vector.add( new String( (byte[])((pair)bargs).car ) );    */
/*       bargs = ((pair)bargs).cdr;                                    */
/*     }                                                               */

       // re-directions
       if (!(binput instanceof keyword))
       {
	  input_port = bigloo.foreign.BFALSE;
	  if (binput instanceof byte[])
	     cmd += "<" + new String( (byte[])binput);
       }

       if (!(boutput instanceof keyword))
       {
	  output_port = bigloo.foreign.BFALSE;
	  if (boutput instanceof byte[])
	     cmd += ">" + new String( (byte[])boutput );
       }

       if (!(berror instanceof keyword))
       {
	  error_port = bigloo.foreign.BFALSE;
	  if (berror instanceof byte[])
	  {
	     cmd += " 2>" + new String( (byte[])berror );
	  }
       }
       
       cmd_vector.add( cmd );
    }

    final String[] argv = new String[cmd_vector.size()];

    cmd_vector.copyInto( argv );

    // Construct the process environment variable list
    aux = benv;
    while (aux instanceof pair)
    {
      ++env_len;
      aux= ((pair)aux).cdr;
    }

    final String[] envp = new String[env_len];

    aux = benv;
    env_len = 0;
    while (aux instanceof pair)
    {
      envp[env_len] = new String( (byte[])((pair)aux).car );
      ++env_len;
      aux= ((pair)aux).cdr;
    }

    // we create the system process
    if (!fork)
      System.err.println( "***WARNING: Can't run process without forking with the JVM back-end" );
    
    process = ((0 < envp.length)
               ? Runtime.getRuntime().exec( argv, envp )
               : Runtime.getRuntime().exec( argv ));

    // the re-directions
    if (binput instanceof keyword)
      input_port = new output_port( process.getOutputStream(), bcommand );

    if (boutput instanceof keyword)
      output_port = new input_pipe_port( process.getInputStream(), bcommand );

    if (berror instanceof keyword)
      error_port = new input_pipe_port( process.getErrorStream(), bcommand );

    // and we store the Java object into the process table
    proc_arr[index] = this;

    // if the output ports are not redirected to files or pipes, we
    // have to flush the input stream
    if ((boutput == bigloo.foreign.BUNSPEC) ||
	(berror == bigloo.foreign.BUNSPEC))
    {
       flushers = new Vector();
       if (boutput == bigloo.foreign.BUNSPEC)
	  flushers.add( new flusher( process.getInputStream(), System.out ) );
       if (berror == bigloo.foreign.BUNSPEC)
	  flushers.add( new flusher( process.getErrorStream(), System.err ) );
    }
    else
       flushers = null;

    // if requested, we wait for the process completion
    if (waiting)
      waitfor();
  }

/*--- private static methods ------------------------------------------*/
  private static void PURGE_PROCESS_TABLE()
  {
    for( int i = 0 ; i < MAX_PROC_NUM ; ++i )
    {
      final Object proc = proc_arr[i];

      if( (proc instanceof process) && !((process)proc).alivep() )
        proc_arr[i]= bigloo.foreign.BUNSPEC;
    }
  }

  private static int find_process()
  {
    for( int i = 0 ; i < MAX_PROC_NUM ; ++i )
      if( proc_arr[i] == bigloo.foreign.BUNSPEC )
        return i;

    return -1;
  }

/*--- public static methods -------------------------------------------*/
  public static obj process_list()
  {
    obj result= (obj)bigloo.foreign.BNIL;

    PURGE_PROCESS_TABLE();

    for( int i = 0 ; i < MAX_PROC_NUM ; ++i )
      if( proc_arr[i] != bigloo.foreign.BUNSPEC )
        result = bigloo.pair.cons( proc_arr[i], result );

    return result;
  }

/*--- public methods --------------------------------------------------*/
  public int pid()
  {
    return -1;
  }

  public boolean alivep()
  {
    try
    {
      process.exitValue();
      return false;
    }
    catch (IllegalThreadStateException _i)
    {
      return true;
    }
  }

  public obj xstatus()
  {
    try
    {
       int r = process.exitValue();

       if( r > 255 )
	  return new bint( (r >> 8) & 0xff );
       else
	  return new bint( r & 0xff );
    }
    catch (IllegalThreadStateException _i)
    {
      return foreign.BFALSE;
    }
  }

  public obj kill()
  {
    process.destroy();
    return (obj)bigloo.foreign.BUNSPEC;
  }

  public void waitfor()
  {
    try
    {
      process.waitFor();
      if (flushers != null)
        while (!flushers.isEmpty())
          ((flusher)flushers.remove( 0 )).join();
    }
    catch (InterruptedException _i)
    {
    }
  }

  public obj stop()
  {
    System.err.println( "***WARNING: Can't stop a process using the JVM back-end" );
    return (obj)bigloo.foreign.BUNSPEC;
  }

  public obj cont()
  {
    System.err.println( "***WARNING: Can't continue a process using the JVM back-end" );
    return (obj)bigloo.foreign.BUNSPEC;
  }

  public obj send_signal( int  s )
  {
    System.err.println( "***WARNING: Can't send signal to process using the JVM back-end" );
    return (obj)bigloo.foreign.BUNSPEC;
  }
}
