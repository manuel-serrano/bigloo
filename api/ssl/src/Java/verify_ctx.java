package bigloo.ssl;

import bigloo.foreign;
import bigloo.obj;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.Signature;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.security.spec.X509EncodedKeySpec;

/**
 * JVM wrapper for ssl-verify operations.
 * Buffers update data because Java's Signature requires the key
 * before updates, but the C API provides the key at verify time.
 */
public class verify_ctx extends obj {

   private String digestName;
   private ByteArrayOutputStream buffer;

   public boolean init(byte[] type) {
      try {
         digestName = new String(type);
         Signature.getInstance(ssl_utils.signatureAlgorithm(digestName, "RSA"));
         buffer = new ByteArrayOutputStream();
         return true;
      } catch (Exception x) {
         foreign.fail("ssl-verify-init", "Verify method not supported", new String(type));
         return false;
      }
   }

   public boolean update(byte[] data, int offset, int len) {
      if (buffer == null) return false;
      buffer.write(data, offset, len);
      return true;
   }

   public boolean verify(byte[] keyPem, int koffset, int klen,
                         byte[] sigBytes, int soffset, int slen) {
      if (buffer == null) return false;
      try {
         byte[] keyData = new byte[klen];
         System.arraycopy(keyPem, koffset, keyData, 0, klen);

         PublicKey pubKey = loadPublicKey(new String(keyData));
         if (pubKey == null) return false;

         byte[] sig = new byte[slen];
         System.arraycopy(sigBytes, soffset, sig, 0, slen);

         String sigAlgo = ssl_utils.signatureAlgorithm(digestName, pubKey.getAlgorithm());
         Signature verifier = Signature.getInstance(sigAlgo);
         verifier.initVerify(pubKey);
         verifier.update(buffer.toByteArray());
         buffer = null;
         return verifier.verify(sig);
      } catch (Exception x) {
         buffer = null;
         // Verification failures are not errors — return false
         return false;
      }
   }

   private static PublicKey loadPublicKey(String pem) {
      try {
         if (pem.contains("BEGIN PUBLIC KEY")) {
            byte[] der = pem_utils.decodePem(pem);
            for (String algo : new String[]{"RSA", "EC", "DSA"}) {
               try {
                  return KeyFactory.getInstance(algo)
                     .generatePublic(new X509EncodedKeySpec(der));
               } catch (Exception ignored) {
               }
            }
         } else if (pem.contains("BEGIN CERTIFICATE")) {
            CertificateFactory cf = CertificateFactory.getInstance("X.509");
            X509Certificate cert = (X509Certificate) cf.generateCertificate(
               new ByteArrayInputStream(pem.getBytes()));
            return cert.getPublicKey();
         }
      } catch (Exception ignored) {
      }
      return null;
   }
}
