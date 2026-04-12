package bigloo.ssl;

import bigloo.foreign;
import bigloo.obj;

import java.io.ByteArrayOutputStream;
import java.security.PrivateKey;
import java.security.Signature;

/**
 * JVM wrapper for ssl-sign operations.
 * Buffers update data because Java's Signature requires the key
 * before updates, but the C API provides the key at sign time.
 */
public class sign_ctx extends obj {

   private String digestName;
   private ByteArrayOutputStream buffer;

   public boolean init(byte[] type) {
      try {
         digestName = new String(type);
         Signature.getInstance(ssl_utils.signatureAlgorithm(digestName, "RSA"));
         buffer = new ByteArrayOutputStream();
         return true;
      } catch (Exception x) {
         foreign.fail("ssl-sign-init", "Sign method not supported", new String(type));
         return false;
      }
   }

   public boolean update(byte[] data, int offset, int len) {
      if (buffer == null) return false;
      buffer.write(data, offset, len);
      return true;
   }

   public byte[] sign(byte[] keyPem, int offset, int len) {
      if (buffer == null) return null;
      try {
         byte[] keyBytes = new byte[len];
         System.arraycopy(keyPem, offset, keyBytes, 0, len);

         String pem = new String(keyBytes);
         PrivateKey key = pem.contains("-----BEGIN")
            ? pem_utils.loadPrivateKeyFromPem(pem) : null;
         if (key == null) {
            foreign.fail("ssl-sign", "Could not load private key", "sign");
            return null;
         }

         String sigAlgo = ssl_utils.signatureAlgorithm(digestName, key.getAlgorithm());
         Signature sig = Signature.getInstance(sigAlgo);
         sig.initSign(key);
         sig.update(buffer.toByteArray());
         buffer = null;
         return sig.sign();
      } catch (Exception x) {
         buffer = null;
         foreign.fail("ssl-sign", x.getMessage(), "sign");
         return null;
      }
   }
}
