package bigloo.ssl;

import bigloo.foreign;
import bigloo.obj;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * JVM wrapper for ssl-cipher operations.
 * Maps OpenSSL cipher names to JCE transformation strings.
 */
public class cipher_ctx extends obj {

   private Cipher cipher;
   private String transformation;
   private byte[] savedKey;
   private byte[] savedIv;
   private int savedMode;

   /**
    * Replicate OpenSSL's EVP_BytesToKey with MD5, no salt, 1 iteration.
    * Derives key and IV from the input key material.
    */
   private static byte[][] evpBytesToKey(int keyLen, int ivLen, byte[] data) {
      try {
         java.security.MessageDigest md =
            java.security.MessageDigest.getInstance("MD5");
         byte[] buf = new byte[keyLen + ivLen];
         int offset = 0;
         byte[] prev = new byte[0];
         while (offset < buf.length) {
            md.reset();
            if (prev.length > 0) md.update(prev);
            md.update(data);
            prev = md.digest();
            int toCopy = Math.min(prev.length, buf.length - offset);
            System.arraycopy(prev, 0, buf, offset, toCopy);
            offset += toCopy;
         }
         byte[] key = new byte[keyLen];
         byte[] iv = new byte[ivLen];
         System.arraycopy(buf, 0, key, 0, keyLen);
         System.arraycopy(buf, keyLen, iv, 0, ivLen);
         return new byte[][]{key, iv};
      } catch (Exception x) {
         foreign.fail("ssl-cipher-init", "EVP_BytesToKey failed: " + x.getMessage(), x);
         return null;
      }
   }

   public boolean init(byte[] type, byte[] key, int offset, int len,
                       boolean encrypt) {
      try {
         transformation = mapCipherName(new String(type));
         cipher = Cipher.getInstance(transformation);

         byte[] keyData = new byte[len];
         System.arraycopy(key, offset, keyData, 0, len);

         int keyLen = getKeyLength(transformation, len);
         int ivLen = cipher.getBlockSize();
         if (transformation.contains("/ECB/")) ivLen = 0;

         byte[][] derived = evpBytesToKey(keyLen, ivLen, keyData);
         savedKey = derived[0];
         savedIv = ivLen > 0 ? derived[1] : null;
         savedMode = encrypt ? Cipher.ENCRYPT_MODE : Cipher.DECRYPT_MODE;

         SecretKeySpec keySpec = new SecretKeySpec(savedKey,
            transformation.split("/")[0]);

         if (savedIv != null) {
            cipher.init(savedMode, keySpec, new IvParameterSpec(savedIv));
         } else {
            cipher.init(savedMode, keySpec);
         }
         return true;
      } catch (Exception x) {
         foreign.fail("ssl-cipher-init", x.getMessage(), new String(type));
         return false;
      }
   }

   public boolean initiv(byte[] type,
                         byte[] key, int koffset, int klen,
                         byte[] iv, int ivoffset, int ivlen,
                         boolean encrypt) {
      try {
         transformation = mapCipherName(new String(type));
         cipher = Cipher.getInstance(transformation);

         savedKey = new byte[klen];
         System.arraycopy(key, koffset, savedKey, 0, klen);

         savedIv = new byte[ivlen];
         System.arraycopy(iv, ivoffset, savedIv, 0, ivlen);

         savedMode = encrypt ? Cipher.ENCRYPT_MODE : Cipher.DECRYPT_MODE;

         SecretKeySpec keySpec = new SecretKeySpec(savedKey,
            transformation.split("/")[0]);
         cipher.init(savedMode, keySpec, new IvParameterSpec(savedIv));
         return true;
      } catch (Exception x) {
         foreign.fail("ssl-cipher-initiv", x.getMessage(), new String(type));
         return false;
      }
   }

   public byte[] update(byte[] data, int offset, int len) {
      if (cipher == null) return null;
      byte[] result = cipher.update(data, offset, len);
      return (result != null) ? result : new byte[0];
   }

   public boolean setAutoPadding(boolean padding) {
      if (cipher == null || transformation == null) return false;
      try {
         String newTransformation = padding
            ? transformation.replace("NoPadding", "PKCS5Padding")
            : transformation.replace("PKCS5Padding", "NoPadding");
         if (!newTransformation.equals(transformation)) {
            transformation = newTransformation;
            cipher = Cipher.getInstance(transformation);
            SecretKeySpec keySpec = new SecretKeySpec(savedKey,
               transformation.split("/")[0]);
            if (savedIv != null) {
               cipher.init(savedMode, keySpec, new IvParameterSpec(savedIv));
            } else {
               cipher.init(savedMode, keySpec);
            }
         }
         return true;
      } catch (Exception x) {
         foreign.fail("ssl-cipher-set-auto-padding", x.getMessage(), padding);
         return false;
      }
   }

   public byte[] doFinal() {
      if (cipher == null) {
         foreign.fail("cipher-final", "uninitialized cipher", "cipher");
         return null;
      }
      try {
         byte[] result = cipher.doFinal();
         cipher = null;
         return result;
      } catch (Exception x) {
         cipher = null;
         foreign.fail("cipher-final", x.getMessage(), "cipher");
         return null;
      }
   }

   /**
    * Determine the correct key length for a JCE transformation.
    */
   private static int getKeyLength(String transformation, int providedLen) {
      String algo = transformation.split("/")[0];
      switch (algo) {
         case "AES":      return providedLen; // 16, 24, or 32
         case "DESede":   return 24;
         case "DES":      return 8;
         case "Blowfish": return providedLen; // variable, 4-56
         case "RC4":      return providedLen; // variable
         default:         return providedLen;
      }
   }

   static String mapCipherName(String name) {
      String lower = name.toLowerCase();

      // AES variants: aes-128-cbc, aes-256-gcm, aes-128-ctr, etc.
      if (lower.startsWith("aes")) {
         String[] parts = lower.split("-");
         if (parts.length >= 3) {
            String mode = parts[2].toUpperCase();
            String padding = (mode.equals("GCM") || mode.equals("CTR")
               || mode.equals("OFB") || mode.equals("CFB"))
               ? "NoPadding" : "PKCS5Padding";
            return "AES/" + mode + "/" + padding;
         }
         return "AES/CBC/PKCS5Padding";
      }

      // ChaCha20-Poly1305 (Java 11+)
      if (lower.equals("chacha20-poly1305")) {
         return "ChaCha20-Poly1305/None/NoPadding";
      }
      if (lower.equals("chacha20")) {
         return "ChaCha20";
      }

      // Camellia
      if (lower.startsWith("camellia")) {
         String[] parts = lower.split("-");
         if (parts.length >= 3) {
            String mode = parts[2].toUpperCase();
            String padding = (mode.equals("GCM") || mode.equals("CTR")
               || mode.equals("OFB") || mode.equals("CFB"))
               ? "NoPadding" : "PKCS5Padding";
            return "Camellia/" + mode + "/" + padding;
         }
         return "Camellia/CBC/PKCS5Padding";
      }

      // DES
      if (lower.equals("des-cbc") || lower.equals("des")) {
         return "DES/CBC/PKCS5Padding";
      }
      if (lower.equals("des-ecb")) {
         return "DES/ECB/PKCS5Padding";
      }
      if (lower.equals("des-ofb")) {
         return "DES/OFB/NoPadding";
      }
      if (lower.equals("des-cfb")) {
         return "DES/CFB/NoPadding";
      }

      // Triple DES
      if (lower.equals("des-ede3-cbc") || lower.equals("des3")) {
         return "DESede/CBC/PKCS5Padding";
      }
      if (lower.equals("des-ede3") || lower.equals("des-ede3-ecb")) {
         return "DESede/ECB/PKCS5Padding";
      }
      if (lower.equals("des-ede3-ofb")) {
         return "DESede/OFB/NoPadding";
      }
      if (lower.equals("des-ede3-cfb")) {
         return "DESede/CFB/NoPadding";
      }

      // Blowfish
      if (lower.startsWith("bf-")) {
         String mode = lower.substring(3).toUpperCase();
         String padding = (mode.equals("OFB") || mode.equals("CFB"))
            ? "NoPadding" : "PKCS5Padding";
         return "Blowfish/" + mode + "/" + padding;
      }

      // RC4 (stream cipher)
      if (lower.equals("rc4")) {
         return "RC4";
      }

      // RC2
      if (lower.startsWith("rc2")) {
         String[] parts = lower.split("-");
         if (parts.length >= 2) {
            String mode = parts[parts.length - 1].toUpperCase();
            return "RC2/" + mode + "/PKCS5Padding";
         }
         return "RC2/CBC/PKCS5Padding";
      }

      // fallback: pass through as-is
      return name;
   }
}
