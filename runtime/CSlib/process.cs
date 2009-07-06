/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Jlib/process.java            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec  8 07:55:39 2000                          */
/*    Last change :  Sat Jul 14 07:02:50 2001 (serrano)                */
/*    Copyright   :  2000-01 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The simplified JVM Bigloo process implementation.                */
/*=====================================================================*/
using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;

/*---------------------------------------------------------------------*/
/*    PROCESS ...                                                      */
/*---------------------------------------------------------------------*/
namespace bigloo
{
  public sealed class process: obj
  {
    /*--- static fields ---------------------------------------------------*/
    private static int MAX_PROC_NUM= 255;
    private static Object[] processes;
    private static process _nil = new process();

    /*--- non static fields -----------------------------------------------*/
    private readonly Process _process;
    private readonly int index;
    private readonly ArrayList flushers;
    public readonly obj input_port;
    public readonly obj output_port;
    public readonly obj error_port;

    /*--- class constructor -----------------------------------------------*/
    static process()
    {
      byte[]       env= foreign.getenv( foreign.getbytes( "BIGLOOLIVEPROCESS" ) );

      if (env != null)
      {
        int        num= foreign.parseint( env, 0, env.Length, 10 );

        if (0 < num)
          MAX_PROC_NUM= num;
      }

      processes= new Object[MAX_PROC_NUM];

      for ( int i= 0 ; i < processes.Length ; ++i ) 
        processes[i]= bigloo.foreign.BUNSPEC;
    }

    /*--- nil constructor -------------------------------------------------*/
    public static process nil()
    {
      return _nil;
    }
      
    /*--- nil instance constructor ----------------------------------------*/
    public process()
    {
      _process = null;
      index = 0;
      flushers = null;
    }
   
    /*--- instance constructor --------------------------------------------*/
    public process( byte[]  host,
                    bool    fork,
                    bool    waiting, 
                    Object  binput,
                    Object  boutput,
                    Object  berror,
                    byte[]  bcommand,
                    Object  bargs,
                    Object  benv ) 
    {
      StringBuilder          cmd= new StringBuilder();
      ProcessStartInfo       process_start_info= new ProcessStartInfo();

      // we purge the process table and ask for a new index
      PURGE_PROCESS_TABLE();
      index= find_process();
      if (index < 0)
        foreign.fail( foreign.getbytes( "run-process" ),
                      foreign.getbytes( "too many processes" ), 
                      unspecified._unspecified );

      // converting "null:" keywords to null file names
      if (   (boutput is keyword)
          && (foreign.KEYWORD_TO_STRING( (keyword)boutput ).ToString().Equals( "null:" )))
        boutput= foreign.getbytes( foreign.bigloo_strcmp( os.OS_CLASS, foreign.getbytes( "unix" ) )
                                   ? "/dev/null"
                                   : "NUL:" );
      if (   (berror is keyword)
          && (foreign.KEYWORD_TO_STRING( (keyword)berror ).ToString().Equals( "null:" )))
        berror= foreign.getbytes( foreign.bigloo_strcmp( os.OS_CLASS, foreign.getbytes( "unix" ) )
                                  ? "/dev/null"
                                  : "NUL:" );

      // sh or rsh ?
      if (host != null)
      {
        process_start_info.FileName= "rsh";
        cmd.Append( foreign.newstring( host ) );
        cmd.Append( " " );
      }
      else
      {
        String               comspec= Environment.GetEnvironmentVariable( "COMSPEC" );

        if (comspec != null)
        {
          process_start_info.FileName= comspec;
          cmd.Append( "/C " );
        }
        else
        {
          process_start_info.FileName= "sh";
          cmd.Append( "-c " );
        }
      }

      // command-line and arguments
      cmd.Append( foreign.newstring( bcommand ) );
      while (bargs is pair)
      {
        cmd.Append( " \"" );
        cmd.Append( foreign.newstring( (byte[])((pair)bargs).car ) );
        cmd.Append( "\"" );
        bargs= ((pair)bargs).cdr;
      }

      // the re-directions
      if (binput is keyword)
        process_start_info.RedirectStandardInput= true;
      else
      {
        input_port= bigloo.foreign.BFALSE;
        if (binput is byte[])
        {
          cmd.Append( " < " );
          cmd.Append( foreign.newstring( (byte[])binput ) );
        }
      }

      if (   (boutput is keyword)
          || (boutput == bigloo.foreign.BUNSPEC))
        process_start_info.RedirectStandardOutput= true;
      else
      {
        output_port= bigloo.foreign.BFALSE;
        if (boutput is byte[])
        {
          cmd.Append( " > " );
          cmd.Append( foreign.newstring( (byte[])boutput ) );
        }
      }

      if (   (berror is keyword)
          || (berror == bigloo.foreign.BUNSPEC))
        process_start_info.RedirectStandardError= true;
      else
      {
        error_port= bigloo.foreign.BFALSE;
        if (berror is byte[])
        {
          cmd.Append( " 2> " );
          cmd.Append( foreign.newstring( (byte[])berror ) );
        }
      }

      process_start_info.Arguments= cmd.ToString();

      if (!fork)
        Console.Error.WriteLine( "***WARNING: Can't run process without forking with the .NET back-end" );

      // Construct the process environment variable list
      while (benv is pair)
      {
        String               current_env_association= foreign.newstring( (byte[])((pair)benv).car );
        String[]             splitted_env= current_env_association.Split( '=' );

        if (splitted_env.Length != 2)
          Console.Error.WriteLine( "***WARNING: Can't split environment string [{0})", current_env_association );
        else
          process_start_info.EnvironmentVariables.Add( splitted_env[0], splitted_env[1] );

        benv= ((pair)benv).cdr;
      }

      // we create the system process
      process_start_info.CreateNoWindow= true;
      process_start_info.UseShellExecute= false;
      _process= Process.Start( process_start_info );

      // the re-directions
      if (binput is keyword)
        // !!!!! should check that direct access to the BaseStream does not mess everything !!!!!
        input_port= new output_stream_port( _process.StandardInput.BaseStream, bcommand );

      if (boutput is keyword)
        // !!!!! should check that direct access to the BaseStream does not mess everything !!!!!
        output_port= new input_pipe_port( _process.StandardOutput.BaseStream, bcommand );

      if (berror is keyword)
        // !!!!! should check that direct access to the BaseStream does not mess everything !!!!!
        error_port= new input_pipe_port( _process.StandardError.BaseStream, bcommand );

      // and we store the .NET object into the process table
      processes[index]= this;

      // if the output ports are not redirected to files or pipes, we
      // have to flush the input stream
      if (   (boutput == bigloo.foreign.BUNSPEC)
          || (berror == bigloo.foreign.BUNSPEC))
      {
        flushers= new ArrayList();
        if (boutput == bigloo.foreign.BUNSPEC)
          flushers.Add( new flusher( _process.StandardOutput, Console.Out ) );
        if (berror == bigloo.foreign.BUNSPEC)
          flushers.Add( new flusher( _process.StandardError, Console.Error ) );
      }

      // if requested, we wait for the process completion
      if (waiting)
        try
        {
          waitfor();
        }
        catch (Exception)
        {
        }
    }

    /*--- private static methods ------------------------------------------*/
    private static void PURGE_PROCESS_TABLE() 
    {
      for ( int i= 0 ; i < processes.Length ; ++i )
      {
        Object          proc= processes[i];

        if (   (proc is process)
            && !((process)proc).alivep())
          processes[i]= unspecified._unspecified;
      }
    }

    private static int find_process() 
    {
      for ( int i= 0 ; i < processes.Length ; ++i )
        if (processes[i] == unspecified._unspecified)
          return i;
      return -1;
    }

    /*--- public static methods -------------------------------------------*/
    public static obj process_list() 
    {
      obj          result= bigloo.nil._nil;

      PURGE_PROCESS_TABLE();

      for ( int i= 0 ; i < processes.Length ; ++i ) 
        if (processes[i] != bigloo.foreign.BUNSPEC)
          result= pair.cons( processes[i], result );

      return result;
    }

    /*--- public methods --------------------------------------------------*/
    public int pid()
    {
      return _process.Id;
    }

    public bool alivep()
    {
      /*
      if (foreign.running_on_mono_vm)       // !!!!! Mono bug :  process.HasExited is always false
      {
        Console.Error.WriteLine( "~~~~~~ process-alive? always returns true on Mono VM :  returning false instead" );
        return false;
      }
      else
      */
        try
        {
          return !_process.HasExited;
        } 
        catch (Exception)
        {
          return false;
        }
    }

    public obj xstatus() 
    {
      try
      {
        if (_process.HasExited)
          return new bint( _process.ExitCode & 0xff );
      }
      catch (Exception)
      {
      }
      return foreign.BFALSE;
    }

    public obj kill() 
    {
      try
      {
        _process.Kill();
      }
      catch (Exception)
      {
      }

      return unspecified._unspecified;
    }

    public void waitfor() 
    {
      try
      {
        _process.WaitForExit();
        if (flushers != null)
        {
          for ( int i= 0 ; i < flushers.Count ; ++i )
            ((flusher)flushers[i]).Join();
          flushers.Clear();
        }
      }
      catch (Exception)
      {
      }
    }

    public obj stop() 
    {
      // !!!!! it may be possible to write Win32 specific code !!!!!
      foreign.print( "***WARNING: Can't stop a process using the .NET back-end" );
      return unspecified._unspecified;
    }
	
    public obj cont() 
    {
      // !!!!! it may be possible to write Win32 specific code !!!!!
      foreign.print( "***WARNING: Can't continue a process using the .NET back-end" );
      return unspecified._unspecified;
    }

    public obj send_signal( int  s ) 
    {
      foreign.print( "***WARNING: Can't send signal to process using the .NET back-end" );
      return unspecified._unspecified;
    }
  }
}
