package bigloo.ssl;

import bigloo.foreign;
import bigloo.obj;

import java.security.PrivateKey;

public class private_key extends obj {

   public String fname;
   public PrivateKey key;

   private private_key(byte[] file) {
      fname = new String(file);
      key = pem_utils.loadPrivateKeyFromFile(fname);
      if (key == null) {
         foreign.fail("private-key-load",
                      "Could not load private key (expected PKCS#8 or PKCS#1 RSA PEM)",
                      fname);
      }
   }

   public static Object load(byte[] file) {
      return ssl.make_private_key(new private_key(file));
   }
}
