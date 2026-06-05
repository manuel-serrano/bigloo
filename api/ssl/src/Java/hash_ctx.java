package bigloo.ssl;

import bigloo.foreign;
import bigloo.obj;

import java.security.MessageDigest;

/**
 * JVM wrapper for ssl-hash operations.
 * Maps OpenSSL digest names to JDK MessageDigest.
 */
public class hash_ctx extends obj {

   private MessageDigest md;

   public boolean init(byte[] type) {
      try {
         md = MessageDigest.getInstance(ssl_utils.mapDigestName(new String(type)));
         return true;
      } catch (Exception x) {
         foreign.fail("ssl-hash-init", "Digest method not supported", new String(type));
         return false;
      }
   }

   public boolean update(byte[] data, int offset, int len) {
      if (md == null) return false;
      md.update(data, offset, len);
      return true;
   }

   public byte[] digest() {
      if (md == null) return null;
      byte[] result = md.digest();
      md = null;
      return result;
   }
}
