package bigloo;

import java.io.*;

public class input_mmap_port extends input_port {

    private int start;
    private int offset;
    private int end;
    private bigloo.mmap mm;
    
    public input_mmap_port( mmap mm, byte[] buffer, int offset, int end ) {
      super(new String(mm.name), buffer);
       
      this.mm = mm;
      this.start = offset;
      this.offset = offset;
      this.end = end;
      this.length = end - offset;
   }

   public boolean rgc_charready() {
       if (eof || other_eof) {
           return false;
       }

       return ((forward+1 < bufpos) || (offset < length));
   }

   Object bgl_input_port_seek( final int  pos ) throws IOException {
       obj res = bigloo.foreign.BFALSE;
       if (pos >= 0 && pos < length) {
           filepos = start+pos;
           offset = start+pos;
           eof = false;
           matchstart = 0;
           matchstop = 0;
           forward = 0;
           bufpos = 0;
           lastchar = (byte)'\n';
           res = bigloo.foreign.BTRUE;
       } else if (pos == length) {
           eof = true;
           res = bigloo.foreign.BTRUE;
       } else {
           throw new IOException("illegal seek offset");
       }

       return res;
   }

   Object bgl_input_port_reopen()
       throws IOException
    {
      offset = start;
      bufpos = 0;
      eof = false;
      filepos = 0;
      matchstart = 0;
      matchstop = 0;
      forward = 0;
      lastchar = (byte)'\n';
      return bigloo.foreign.BTRUE;
   }

   public void close() {
      eof = true;
      other_eof= true;
      super.close();
   }

   public boolean rgc_fill_buffer()
       throws IOException
    {
        final int bufsize = this.buffer.length;
        int bufpose = this.bufpos;
        final int matchstart = this.matchstart;
        final byte[] buffer = this.buffer;
        
        if (matchstart > 0)
            {
                // we shift the buffer left and we fill the buffer */
                final int movesize = bufpose-matchstart;
                
                for ( int i= 0 ; i < movesize ; ++i )
                    buffer[i] = buffer[matchstart+i];
                
                bufpose -= matchstart;
                this.matchstart = 0;
                this.matchstop -= matchstart;
                this.forward -= matchstart;
                this.lastchar = buffer[matchstart-1];
                
                return rgc_size_fill_mmap_buffer( bufpose, bufsize-bufpose );
            }
        
        if (bufpose < bufsize)
            {
                return rgc_size_fill_mmap_buffer( bufpose, bufsize-bufpose );
            }
        
        // we current token is too large for the buffer */
        // we have to enlarge it.                       */
        rgc_double_buffer();
        
        return rgc_fill_buffer();
    } 
    
    final boolean rgc_size_fill_mmap_buffer( int bufpose, final int  size )
        throws IOException
    {
        final int nbcopy = ((offset + size) < length) ? size : (int)(length - offset);
        boolean res = true;
        if (nbcopy == 0) {
            eof = true;
            res = false;
        } else {
            for(int i = 0; i < nbcopy; i++) {
                buffer[bufpose+i] = (byte)foreign.BGL_MMAP_REF(mm, offset+i);
            }
            offset += nbcopy;
            bufpose += nbcopy;
        }
        this.bufpos = bufpose;
        
        return res;        
    }

    public Object bgl_input_port_clone( input_port src )
    {
     super.bgl_input_port_clone( src );
     mm = ((input_mmap_port)src).mm;
     offset = ((input_mmap_port)src).offset;
     end = ((input_mmap_port)src).end;
     length = src.length;
     return this;
    }

    public void write( final output_port  p )
    {
        p.write( "#<input_mmap_port:" + name + ">" );
    }
}
