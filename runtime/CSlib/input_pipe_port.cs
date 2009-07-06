/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/CSlib/input_pipe_port.cs     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Dec  9 11:49:41 2000                          */
/*    Last change :  Mon Aug 25 09:27:30 2008 (serrano)                */
/*    Copyright   :  2000-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo JVM input pipe ports.                                     */
/*=====================================================================*/
using System;
using System.Diagnostics;
using System.IO;

namespace bigloo {
   /*---------------------------------------------------------------------*/
   /*    input_pipe_port ...                                              */
   /*---------------------------------------------------------------------*/
   public sealed class input_pipe_port: input_port {
      public Stream _in;

      /*--- public constructors ---------------------------------------------*/
      public input_pipe_port( Stream _in, byte[]  c )
	 : base( foreign.newstring( c ), (int)bigloo.foreign.default_io_bufsiz ) {
	 this._in = _in;
      }

      public input_pipe_port( byte[] cmd, byte[] buf ) : base( foreign.newstring( cmd ), buf ) {
	 try {
	    Process process = new Process();
	    
	    process.StartInfo.FileName = "/bin/sh";
	    process.StartInfo.Arguments = "-c " + foreign.newstring( cmd );
	    process.Start();
	    _in= process.StandardInput.BaseStream;   // !!!!! BaseStream !!!!!
	 } catch (Exception) {
	    String scmd = foreign.newstring( cmd, 2, cmd.Length-2 );
	    foreign.fail( "open-input-pipe",
			  "Can't execute command",
			  foreign.getbytes( scmd ) );
	 }
      }

      public input_pipe_port( String cmd, byte[] buf ) : base( cmd, buf ) {
	 try {
	    Process process = new Process();

	    process.StartInfo.FileName = "/bin/sh";
	    process.StartInfo.Arguments = "-c \"" + foreign.newstring( cmd ) + "\"";
	    process.Start();
	    _in= process.StandardInput.BaseStream;   // !!!!! BaseStream !!!!!
	 } catch (Exception e) {
	    foreign.fail( "open-input-pipe",
			  "Can't execute command",
			  foreign.getbytes( cmd ) );
	 }
      }

      /*--- public methods --------------------------------------------------*/
      public override void close() {
	 base.close();
	 eof = true;
	 other_eof = true;
	 _in.Close();
      }

      public override bool rgc_charready() {
	 try {
	    return (((forward+1) < bufpos) || (_in.Position < _in.Length));
	 } 
	 catch (Exception) {
	    return false;
	 }
      }

      public override bool rgc_fill_buffer() {
	 int          bufsize = this.bufsiz;
	 int          bufpose = this.bufpos;
	 int          matchstart = this.matchstart;
	 byte[]       buffer = this.buffer;

	 if (0 < matchstart) {
	    // we shift the buffer left and we fill the buffer
	    int movesize = bufpose-matchstart;

	    for ( int i = 0 ; i < movesize ; ++i )
	       buffer[i]= buffer[matchstart+i];

	    bufpose -= matchstart;
	    this.matchstart = 0;
	    this.matchstop -= matchstart;
	    this.forward -= matchstart;
	    this.lastchar = buffer[ matchstart - 1 ];

	    return rgc_size_fill_file_buffer( bufpose, bufsize-bufpose );
	 }

	 if(bufpose < bufsize)
	    return rgc_size_fill_file_buffer( bufpose, bufsize-bufpose );

	 // we current token is too large for the buffer
	 // we have to enlarge it.
	 rgc_double_buffer();

	 return rgc_fill_buffer();
      }

      bool rgc_size_fill_file_buffer( int bufpose, int size ) {
	 int nbread = _in.Read( buffer, bufpose-1, size );

	 if (nbread == 0)
	    eof = true;
	 else
	    bufpose += nbread;

	 this.bufpos = bufpose;

	 if (0 < bufpose) {
	    buffer[bufpose-1]= (byte)0;
	    return true;
	 }

	 return false;
      }

      public override void write( output_port  p ) {
	 p.write( "#<input_pipe_port:" + name + ">" );
      }

      /*--- static methods --------------------------------------------------*/
      public static bool pipe_name_p( byte[] name ) {
	 return ((name[0] == (byte)'|') && (name[1] == (byte)' '));
      }
   }
}
