package bigloo.ssl;

import bigloo.foreign;
import bigloo.obj;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

/**
 * JVM wrapper for ssl-hmac operations.
 */
public class hmac_ctx extends obj {

   private Mac mac;

   public boolean init(byte[] type, byte[] key) {
      try {
         String algo = ssl_utils.mapHmacName(new String(type));
         mac = Mac.getInstance(algo);
         SecretKeySpec keySpec = new SecretKeySpec(
            (key != null && key.length > 0) ? key : new byte[]{0}, algo);
         mac.init(keySpec);
         return true;
      } catch (Exception x) {
         foreign.fail("ssl-hmac-init", "Digest method not supported", new String(type));
         return false;
      }
   }

   public boolean update(byte[] data, int offset, int len) {
      if (mac == null) return false;
      mac.update(data, offset, len);
      return true;
   }

   public byte[] digest() {
      if (mac == null) return null;
      byte[] result = mac.doFinal();
      mac = null;
      return result;
   }
}
