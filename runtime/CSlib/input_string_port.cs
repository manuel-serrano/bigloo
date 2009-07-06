namespace bigloo {
   public sealed class input_string_port: input_port {
      public input_string_port( byte[] s, int start )
	 : base( "[string]", (s.Length+1) - start ) {
	 int size = s.Length - start;

	 for ( int i= 0 ; i < size ; ++i )
	    buffer[i] = s[i + start];

	 buffer[size] = 0;
	 bufpos= bufsiz;
	 eof= true;
      }

      public override bool rgc_charready() {
	 return ((forward+1) < bufpos);
      }

      public void reopen_input_c_string( byte[]  s ) {
	 int len= s.Length;

	 if (bufsiz < (len + 1)) {
	    bufsiz = len + 1;
	    buffer = new byte[len + 1];
	 }

	 bufpos= len + 1;
	 matchstart= 0;
	 matchstop= 0;
	 forward= 0;
	 lastchar= (byte)'\n';
	 buffer[len]= 0;
	 --len;

	 while (0 <= len) {
	    buffer[len]= s[len];
	    --len;
	 }
      }

      public override object bgl_input_port_seek( int  pos ) {
	 if (pos < bufsiz) {
	    filepos= pos;
	    matchstart= pos;
	    matchstop= pos;
	    forward= pos;
	    return foreign.BTRUE;
	 }

	 return foreign.BFALSE;
      }

      public override object bgl_input_port_reopen()  {
	 eof = false;
	 filepos= 0;
	 matchstart = 0;
	 matchstop = 0;
	 forward = 0;
	 lastchar = (byte)'\n';
	 return foreign.BTRUE;
      }

      public override void close() {
	 eof= true;
	 other_eof = true;
	 base.close();
      }

      public override bool rgc_fill_buffer() {
	 return false;
      }
   }
}
