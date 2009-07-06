using System;

namespace bigloo 
{
  public sealed class bstruct: obj 
  {
    public Object key;
    public readonly Object[] values;
    
    public bstruct( Object  key,
                    int     size ) 
    {
      this.key= key;
      values= new Object[size];
    }
    
    public bstruct( Object  key,
                    int     size,
                    Object  init ) 
    {
      this.key= key;
      values= new Object[size];
      for ( int i= 0 ; i < size ; ++i ) 
        values[i]= init;
    }
  
    public override void write( output_port  p ) 
    {
      p.write( "#<" );
      foreign.write_object( key, p );
      for ( int i= 0 ; i < values.Length ; ++i ) 
      {
        p.write( " " );
        foreign.write_object( values[i], p );
      }
      p.write( ">" );
    }
  }
}
