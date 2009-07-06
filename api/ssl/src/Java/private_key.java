package bigloo.ssl;

import bigloo.*;

public class private_key extends obj{

  public String fname;
  
  private private_key(byte[] file){
    fname = new String(file);
  }

  public static Object load(byte[] file){
    return ssl.make_private_key(new private_key(file));
  }
}
