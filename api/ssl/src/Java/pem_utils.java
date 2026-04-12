package bigloo.ssl;

import bigloo.foreign;

import java.io.BufferedReader;
import java.io.FileReader;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Base64;

/**
 * PEM file utilities shared across SSL classes.
 */
public class pem_utils {

   /**
    * Read a PEM file, strip header/footer lines, and base64-decode
    * the body to raw DER bytes.
    */
   public static byte[] readPemBytes(String fname) {
      try {
         StringBuilder sb = new StringBuilder();
         try (BufferedReader br = new BufferedReader(new FileReader(fname))) {
            String line;
            while ((line = br.readLine()) != null) {
               sb.append(line);
               sb.append('\n');
            }
         }
         return decodePem(sb.toString());
      } catch (Exception x) {
         foreign.fail("pem-read", "Could not read PEM file", x);
         return null;
      }
   }

   /**
    * Strip PEM header/footer lines from a string and base64-decode
    * the body to raw DER bytes.
    */
   public static byte[] decodePem(String pem) {
      StringBuilder sb = new StringBuilder();
      for (String line : pem.split("\n")) {
         if (!line.startsWith("-----")) {
            sb.append(line.trim());
         }
      }
      return Base64.getDecoder().decode(sb.toString());
   }

   // PKCS#8 header for wrapping a PKCS#1 RSA key:
   //   SEQUENCE { version, AlgorithmIdentifier { rsaEncryption, NULL }, OCTET STRING }
   private static final byte[] PKCS8_RSA_PREFIX = {
      0x30, (byte)0x82, 0x00, 0x00,  // SEQUENCE, length placeholder
      0x02, 0x01, 0x00,              // INTEGER 0 (version)
      0x30, 0x0d,                    // SEQUENCE (AlgorithmIdentifier)
      0x06, 0x09,                    //   OID 1.2.840.113549.1.1.1
      0x2a, (byte)0x86, 0x48, (byte)0x86, (byte)0xf7, 0x0d, 0x01, 0x01, 0x01,
      0x05, 0x00,                    //   NULL
      0x04, (byte)0x82, 0x00, 0x00   // OCTET STRING, length placeholder
   };

   /**
    * Wrap PKCS#1 RSA DER bytes in a PKCS#8 envelope.
    */
   private static byte[] wrapPkcs1InPkcs8(byte[] pkcs1) {
      byte[] prefix = PKCS8_RSA_PREFIX.clone();
      int totalLen = prefix.length + pkcs1.length;
      // outer SEQUENCE length (totalLen - 4 for the tag+length bytes)
      int seqLen = totalLen - 4;
      prefix[2] = (byte) (seqLen >> 8);
      prefix[3] = (byte) (seqLen & 0xff);
      // OCTET STRING length
      prefix[prefix.length - 2] = (byte) (pkcs1.length >> 8);
      prefix[prefix.length - 1] = (byte) (pkcs1.length & 0xff);

      byte[] pkcs8 = new byte[totalLen];
      System.arraycopy(prefix, 0, pkcs8, 0, prefix.length);
      System.arraycopy(pkcs1, 0, pkcs8, prefix.length, pkcs1.length);
      return pkcs8;
   }

   /**
    * Load a private key from PEM content (as a string).
    * Supports both PKCS#8 (BEGIN PRIVATE KEY) and PKCS#1 (BEGIN RSA PRIVATE KEY).
    */
   public static PrivateKey loadPrivateKeyFromPem(String pem) {
      byte[] der = decodePem(pem);
      boolean isPkcs1 = pem.contains("BEGIN RSA PRIVATE KEY");

      if (isPkcs1) {
         // Try as PKCS#1 RSA wrapped in PKCS#8
         try {
            byte[] pkcs8 = wrapPkcs1InPkcs8(der);
            return KeyFactory.getInstance("RSA")
               .generatePrivate(new PKCS8EncodedKeySpec(pkcs8));
         } catch (Exception ignored) {
         }
      }

      // Try as PKCS#8 with each algorithm
      for (String algo : new String[]{"RSA", "EC", "DSA"}) {
         try {
            return KeyFactory.getInstance(algo)
               .generatePrivate(new PKCS8EncodedKeySpec(der));
         } catch (Exception ignored) {
         }
      }
      return null;
   }

   /**
    * Load a private key from a PEM file.
    */
   public static PrivateKey loadPrivateKeyFromFile(String fname) {
      try {
         StringBuilder sb = new StringBuilder();
         try (BufferedReader br = new BufferedReader(new FileReader(fname))) {
            String line;
            while ((line = br.readLine()) != null) {
               sb.append(line);
               sb.append('\n');
            }
         }
         return loadPrivateKeyFromPem(sb.toString());
      } catch (Exception x) {
         return null;
      }
   }
}
