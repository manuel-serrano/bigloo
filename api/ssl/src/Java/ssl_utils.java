package bigloo.ssl;

import bigloo.bbool;
import bigloo.foreign;
import bigloo.nil;
import bigloo.pair;

import java.security.KeyStore;
import java.security.cert.X509Certificate;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManagerFactory;

/**
 * Shared SSL utilities for client and server socket classes.
 */
public class ssl_utils {

   public static final int BGLSSL_SSLV2 = 1;
   public static final int BGLSSL_SSLV3 = 2;
   public static final int BGLSSL_SSLV23 = 3;
   public static final int BGLSSL_TLSV1 = 4;
   public static final int BGLSSL_DTLSV1 = 5;
   public static final int BGLSSL_TLSV1_1 = 6;
   public static final int BGLSSL_TLSV1_2 = 7;
   public static final int BGLSSL_TLSV1_3 = 8;

   public static String protoToString(int proto) {
      switch (proto) {
         case BGLSSL_SSLV2:
            foreign.fail("ssl", "SSLv2 is not supported (insecure, removed from modern JDKs)", proto);
            return null;
         case BGLSSL_SSLV3:
            foreign.fail("ssl", "SSLv3 is not supported (insecure, disabled in modern JDKs)", proto);
            return null;
         case BGLSSL_SSLV23:  return "TLS";
         case BGLSSL_TLSV1:   return "TLSv1";
         case BGLSSL_TLSV1_1: return "TLSv1.1";
         case BGLSSL_TLSV1_2: return "TLSv1.2";
         case BGLSSL_TLSV1_3: return "TLSv1.3";
         case BGLSSL_DTLSV1:  return "DTLSv1.0";
         default:
            foreign.fail("ssl", "Unsupported SSL protocol", proto);
            return null;
      }
   }

   public static SSLContext buildSSLContext(int proto,
                                           Object cert,
                                           Object pkey,
                                           Object caList) throws Exception {
      SSLContext ctx = SSLContext.getInstance(protoToString(proto));

      // NOTE: When no CAs are provided, the JVM backend validates against
      // the system trust store (cacerts), which is stricter than the C backend.
      // The C backend uses SSL_VERIFY_NONE by default, accepting any certificate.
      // To match the C backend's insecure default, replace the null tmf case
      // below with a trust-all X509TrustManager that no-ops checkServerTrusted.
      // This is intentionally not done here for security reasons.
      TrustManagerFactory tmf = null;
      if (caList != nil.nil) {
         KeyStore ts = KeyStore.getInstance(KeyStore.getDefaultType());
         ts.load(null, null);
         int i = 0;
         while (caList != nil.nil) {
            pair cell = (pair) caList;
            certificate ca = (certificate) ssl.certificate_native(cell.car);
            ts.setCertificateEntry("ca-" + i, ca.x509);
            caList = cell.cdr;
            i++;
         }
         tmf = TrustManagerFactory.getInstance(
            TrustManagerFactory.getDefaultAlgorithm());
         tmf.init(ts);
      }

      KeyManagerFactory kmf = null;
      if (cert != bbool.faux) {
         certificate c = (certificate) ssl.certificate_native(cert);
         private_key pk = (private_key) ssl.private_key_native(pkey);
         KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
         ks.load(null, null);
         ks.setKeyEntry("key", pk.key, new char[0],
                        new X509Certificate[]{c.x509});
         kmf = KeyManagerFactory.getInstance(
            KeyManagerFactory.getDefaultAlgorithm());
         kmf.init(ks, new char[0]);
      }

      ctx.init(kmf != null ? kmf.getKeyManagers() : null,
               tmf != null ? tmf.getTrustManagers() : null,
               null);

      return ctx;
   }

   public static void checkAcceptedCerts(SSLSocket sslsocket,
                                         Object acceptedCerts) throws Exception {
      if (acceptedCerts == bbool.faux) return;

      sslsocket.startHandshake();
      java.security.cert.X509Certificate peerCert =
         (java.security.cert.X509Certificate)
         sslsocket.getSession().getPeerCertificates()[0];
      while (acceptedCerts != nil.nil) {
         pair cell = (pair) acceptedCerts;
         certificate c = (certificate) ssl.certificate_native(cell.car);
         if (peerCert.equals(c.x509)) {
            return;
         }
         acceptedCerts = cell.cdr;
      }
      foreign.fail("ssl",
                   "Presented certificate is not in the accept list",
                   peerCert.getSubjectX500Principal().getName());
   }

   public static byte[] pkcs5Pbkdf2HmacSha1(byte[] pass, byte[] salt,
                                              int iter, int keylen) {
      try {
         javax.crypto.SecretKeyFactory skf =
            javax.crypto.SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
         javax.crypto.spec.PBEKeySpec spec =
            new javax.crypto.spec.PBEKeySpec(
               new String(pass).toCharArray(), salt, iter, keylen * 8);
         return skf.generateSecret(spec).getEncoded();
      } catch (Exception x) {
         foreign.fail("pkcs5-pbkdf2-hmac-sha1", x.getMessage(), pass);
         return null;
      }
   }

   private static final java.security.SecureRandom secureRandom =
      new java.security.SecureRandom();

   public static byte[] randBytes(int size) {
      byte[] buf = new byte[size];
      secureRandom.nextBytes(buf);
      return buf;
   }

   public static byte[] sslVersion() {
      try {
         java.security.Provider p =
            javax.net.ssl.SSLContext.getDefault().getProvider();
         return (p.getName() + " " + p.getVersion()).getBytes();
      } catch (Exception x) {
         return "JSSE (unknown version)".getBytes();
      }
   }

   public static Object[] getSslCiphers() {
      try {
         String[] suites = javax.net.ssl.SSLContext.getDefault()
            .getDefaultSSLParameters().getCipherSuites();
         Object[] result = new Object[suites.length];
         for (int i = 0; i < suites.length; i++) {
            result[i] = suites[i].getBytes();
         }
         return result;
      } catch (Exception x) {
         foreign.fail("ssl-get-ciphers", x.getMessage(), x);
         return null;
      }
   }

   public static Object getEvpCiphers() {
      bigloo.obj ret = bigloo.nil.nil;
      for (java.security.Provider p : java.security.Security.getProviders()) {
         for (java.security.Provider.Service s : p.getServices()) {
            if ("Cipher".equals(s.getType())) {
               ret = new bigloo.pair(s.getAlgorithm().getBytes(), ret);
            }
         }
      }
      return ret;
   }

   public static Object getEvpHashes() {
      bigloo.obj ret = bigloo.nil.nil;
      for (java.security.Provider p : java.security.Security.getProviders()) {
         for (java.security.Provider.Service s : p.getServices()) {
            if ("MessageDigest".equals(s.getType())) {
               ret = new bigloo.pair(s.getAlgorithm().getBytes(), ret);
            }
         }
      }
      return ret;
   }

   static String mapDigestName(String name) {
      switch (name.toLowerCase()) {
         case "sha1":       return "SHA-1";
         case "sha224":     return "SHA-224";
         case "sha256":     return "SHA-256";
         case "sha384":     return "SHA-384";
         case "sha512":     return "SHA-512";
         case "sha3-224":   return "SHA3-224";
         case "sha3-256":   return "SHA3-256";
         case "sha3-384":   return "SHA3-384";
         case "sha3-512":   return "SHA3-512";
         case "blake2b512": return "BLAKE2B-512";
         case "blake2s256": return "BLAKE2S-256";
         case "md5":        return "MD5";
         case "md4":        return "MD4";
         case "ripemd160":  return "RIPEMD160";
         default:           return name.toUpperCase();
      }
   }

   static String mapHmacName(String name) {
      switch (name.toLowerCase()) {
         case "sha1":     return "HmacSHA1";
         case "sha224":   return "HmacSHA224";
         case "sha256":   return "HmacSHA256";
         case "sha384":   return "HmacSHA384";
         case "sha512":   return "HmacSHA512";
         case "sha3-224": return "HmacSHA3-224";
         case "sha3-256": return "HmacSHA3-256";
         case "sha3-384": return "HmacSHA3-384";
         case "sha3-512": return "HmacSHA3-512";
         case "md5":      return "HmacMD5";
         default:         return "Hmac" + mapDigestName(name).replace("-", "");
      }
   }

   static String signatureAlgorithm(String digestName, String keyAlgorithm) {
      if ("EdDSA".equals(keyAlgorithm) || "Ed25519".equals(keyAlgorithm)) {
         return "Ed25519";
      }
      if ("Ed448".equals(keyAlgorithm)) {
         return "Ed448";
      }

      // JCA signature names use the digest without the legacy SHA-1/SHA-2
      // dash (e.g. "SHA256withRSA") but keep the SHA-3 dash
      // (e.g. "SHA3-256withRSA").
      String jdkDigest = mapDigestName(digestName);
      String base = jdkDigest.startsWith("SHA3-")
         ? jdkDigest
         : jdkDigest.replace("-", "");
      switch (keyAlgorithm) {
         case "EC":    return base + "withECDSA";
         case "DSA":   return base + "withDSA";
         case "RSASSA-PSS":
         case "RSA":   return base + "withRSA";
         default:      return base + "withRSA";
      }
   }
}
