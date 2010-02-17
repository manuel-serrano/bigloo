package bigloo;

import java.io.*;

public class input_procedure_port extends input_port
{

  public procedure in;

  public input_procedure_port( final procedure p, byte[] buf ) {
    super( "[procedure]", buf );
    in = p;
  }

  public input_procedure_port( final procedure p, String id, byte[] buf ) {
    super( id, buf );
    in = p;
  }

  public boolean rgc_charready()
  {
    return true;
  }

  public void close() 
  {
     super.close();
     eof = true;
     other_eof = true;
  }

  public boolean rgc_fill_buffer()
  {
    int bufsize = this.bufsiz;
    int bufpose = this.bufpos;
    final int matchstart = this.matchstart;
    final Object value = foreign.eval_funcall_0(this.in);
    int value_len = 0;

    if (value instanceof byte[]) {
      value_len = ((byte[])value).length;
    } else {
       if (value instanceof bchar ) {
	  value_len = 1;
       } else {
	  if (foreign.EOF_OBJECTP( value ) ||
	     (value == bigloo.foreign.BFALSE)) {
	     eof = true;
	     return false;
	  } else {
	     foreign.fail( "input-procedure-port",
			   "Procedure result must be a string, or a char, or #f, or the eof-object",
			   this );
	  }
       }
    }

    final int already_used = (bufpose - matchstart);

    // first let's see if the buffer is big enough
    rgc_enlarge_buffer_size(value_len + already_used);

    bufsize = this.bufsiz;

    // do we need to shift?
    if ((bufpose + value_len) > bufsiz) {
      // we shift the buffer left
      for ( int i = 0 ; i < already_used ; ++i )
        buffer[i] = buffer[matchstart+i];

      bufpose -= matchstart;
      this.matchstart = 0;
      this.matchstop -= matchstart;
      this.forward -= matchstart;
      this.lastchar = buffer[matchstart-1];
    }

    // finally we insert our object
    if (value instanceof byte[]) {
      for (int i = 0; i < value_len; i++) {
        buffer[i + bufpose - 1] = ((byte[])value)[i];
      }
      buffer[bufpose + value_len - 1] = 0;
    } else {
       if (value instanceof bchar) {
	  buffer[bufpose - 1] = ((bchar)value).value;
       }
    }
    
    this.bufpos = bufpose + value_len;

    return true;
  }
}


