package bigloo.ssl;

import bigloo.foreign;
import bigloo.obj;

import java.io.ByteArrayInputStream;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.cert.CertificateFactory;
import java.security.cert.CRL;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManagerFactory;

/**
 * SSL secure context for the JVM backend.
 * Wraps javax.net.ssl.SSLContext and accumulates configuration
 * (certs, keys, CAs, CRLs) that are applied at init time.
 */
public class ctx extends obj {

   public SSLContext sslContext;
   private KeyStore trustStore;
   private KeyStore keyStore;
   private List<CRL> crls;
   private String ciphers;
   private int caCount;
   private PrivateKey pendingKey;
   private boolean useServerCipherOrder = true;

   public ctx() {
      try {
         trustStore = KeyStore.getInstance(KeyStore.getDefaultType());
         trustStore.load(null, null);
         keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
         keyStore.load(null, null);
         crls = new ArrayList<CRL>();
         caCount = 0;
      } catch (Exception x) {
         foreign.fail("secure-context", "Could not initialize context", x);
      }
   }

   /**
    * Initialize the SSLContext from accumulated configuration.
    * Called from secure-context-init on the Scheme side.
    */
   public void init(byte[] method) {
      try {
         String proto = methodToProto(new String(method));
         sslContext = SSLContext.getInstance(proto);

         TrustManagerFactory tmf = TrustManagerFactory.getInstance(
            TrustManagerFactory.getDefaultAlgorithm());

         if (!crls.isEmpty()) {
            // Enable CRL checking via PKIX parameters
            java.security.cert.PKIXBuilderParameters pkixParams =
               new java.security.cert.PKIXBuilderParameters(trustStore, null);
            java.security.cert.CertStore crlStore =
               java.security.cert.CertStore.getInstance("Collection",
                  new java.security.cert.CollectionCertStoreParameters(crls));
            pkixParams.addCertStore(crlStore);
            pkixParams.setRevocationEnabled(true);
            tmf.init(new javax.net.ssl.CertPathTrustManagerParameters(pkixParams));
         } else {
            tmf.init(trustStore);
         }

         KeyManagerFactory kmf = KeyManagerFactory.getInstance(
            KeyManagerFactory.getDefaultAlgorithm());
         kmf.init(keyStore, new char[0]);

         sslContext.init(kmf.getKeyManagers(), tmf.getTrustManagers(), null);
      } catch (Exception x) {
         foreign.fail("secure-context-init",
                      "Could not initialize SSL context", x);
      }
   }

   /**
    * Free resources.
    */
   public void close() {
      sslContext = null;
      trustStore = null;
      keyStore = null;
      crls = null;
   }

   /**
    * Add the JVM's default trusted root certificates to the trust store.
    */
   public boolean addRootCerts() {
      try {
         TrustManagerFactory tmf = TrustManagerFactory.getInstance(
            TrustManagerFactory.getDefaultAlgorithm());
         tmf.init((KeyStore) null);

         // The default TrustManagerFactory (initialized with null) uses
         // the JVM's cacerts. We need to extract those certs and add them
         // to our trust store.
         javax.net.ssl.TrustManager[] tms = tmf.getTrustManagers();
         for (javax.net.ssl.TrustManager tm : tms) {
            if (tm instanceof javax.net.ssl.X509TrustManager) {
               for (X509Certificate c :
                     ((javax.net.ssl.X509TrustManager) tm).getAcceptedIssuers()) {
                  trustStore.setCertificateEntry("root-" + caCount, c);
                  caCount++;
               }
            }
         }
         return true;
      } catch (Exception x) {
         return false;
      }
   }

   /**
    * Add a CA certificate from a PEM/DER buffer.
    */
   public boolean addCaCert(byte[] buf, int offset, int len) {
      try {
         CertificateFactory cf = CertificateFactory.getInstance("X.509");
         ByteArrayInputStream bis = new ByteArrayInputStream(
            buf, offset, len);
         X509Certificate cert = (X509Certificate) cf.generateCertificate(bis);
         trustStore.setCertificateEntry("ca-" + caCount, cert);
         caCount++;
         return true;
      } catch (Exception x) {
         return false;
      }
   }

   /**
    * Add a CRL from a PEM/DER buffer.
    */
   public boolean addCrl(byte[] buf, int offset, int len) {
      try {
         CertificateFactory cf = CertificateFactory.getInstance("X.509");
         ByteArrayInputStream bis = new ByteArrayInputStream(
            buf, offset, len);
         CRL crl = cf.generateCRL(bis);
         crls.add(crl);
         return true;
      } catch (Exception x) {
         return false;
      }
   }

   /**
    * Set the private key from a PEM/DER buffer.
    * Supports PKCS#8 and PKCS#1 RSA formats.
    */
   public boolean setKey(byte[] buf, int offset, int len, Object passphrase) {
      try {
         byte[] keyBytes = new byte[len];
         System.arraycopy(buf, offset, keyBytes, 0, len);

         String pem = new String(keyBytes);
         PrivateKey key = pem.contains("-----BEGIN")
            ? pem_utils.loadPrivateKeyFromPem(pem) : null;
         if (key == null) return false;

         // Try to associate with existing cert chain
         java.security.cert.Certificate[] chain =
            keyStore.getCertificateChain("key");
         if (chain != null && chain.length > 0) {
            keyStore.setKeyEntry("key", key, new char[0], chain);
         } else {
            // No cert yet — store key for later association by setCert
            pendingKey = key;
         }
         return true;
      } catch (Exception x) {
         return false;
      }
   }

   /**
    * Set the certificate (and optional chain) from a PEM/DER buffer.
    */
   public boolean setCert(byte[] buf, int offset, int len) {
      try {
         CertificateFactory cf = CertificateFactory.getInstance("X.509");
         ByteArrayInputStream bis = new ByteArrayInputStream(
            buf, offset, len);
         List<X509Certificate> certs = new ArrayList<X509Certificate>();
         for (java.security.cert.Certificate c : cf.generateCertificates(bis)) {
            certs.add((X509Certificate) c);
         }
         if (certs.isEmpty()) return false;

         X509Certificate[] chain = certs.toArray(new X509Certificate[0]);

         // Check for pending key or existing key in keystore
         PrivateKey key = pendingKey;
         if (key == null) {
            try {
               key = (PrivateKey) keyStore.getKey("key", new char[0]);
            } catch (Exception ignored) {
            }
         }

         if (key != null) {
            keyStore.setKeyEntry("key", key, new char[0], chain);
            pendingKey = null;
         } else {
            keyStore.setCertificateEntry("cert", chain[0]);
         }
         return true;
      } catch (Exception x) {
         return false;
      }
   }

   /**
    * Set the session ID context for session resumption.
    */
   public boolean setSessionIdContext(byte[] buf, int offset, int len) {
      // JVM SSLContext handles session management internally.
      // Session ID context is an OpenSSL-specific concept.
      // Accept the call but no-op for compatibility.
      return true;
   }

   /**
    * Load a PKCS#12 bundle containing key, cert, and optional CA certs.
    */
   public boolean loadPkcs12(byte[] pfx, byte[] pass) {
      try {
         KeyStore p12 = KeyStore.getInstance("PKCS12");
         ByteArrayInputStream bis = new ByteArrayInputStream(pfx);
         char[] password = (pass != null) ? new String(pass).toCharArray() : new char[0];
         p12.load(bis, password);

         // Transfer all entries to our stores
         java.util.Enumeration<String> aliases = p12.aliases();
         while (aliases.hasMoreElements()) {
            String alias = aliases.nextElement();
            if (p12.isKeyEntry(alias)) {
               PrivateKey key = (PrivateKey) p12.getKey(alias, password);
               java.security.cert.Certificate[] chain =
                  p12.getCertificateChain(alias);
               keyStore.setKeyEntry("key", key, new char[0], chain);
            } else if (p12.isCertificateEntry(alias)) {
               X509Certificate cert =
                  (X509Certificate) p12.getCertificate(alias);
               trustStore.setCertificateEntry("ca-" + caCount, cert);
               caCount++;
            }
         }
         return true;
      } catch (Exception x) {
         foreign.fail("load-pkcs12", "Could not load PKCS#12 bundle", x);
         return false;
      }
   }

   /**
    * Set SSL options. Maps OpenSSL option flags to JSSE equivalents.
    * Currently supports SSL_OP_CIPHER_SERVER_PREFERENCE (value 1 on JVM).
    */
   public boolean setOptions(int options) {
      if ((options & 1) != 0) {
         useServerCipherOrder = true;
      }
      return true;
   }

   /**
    * Set the cipher list.
    * TODO: When ssl-connection is implemented, apply ciphers to sockets
    * created from this context via SSLParameters.setCipherSuites().
    */
   public boolean setCiphers(byte[] cipherList) {
      this.ciphers = new String(cipherList);
      return true;
   }

   private static String methodToProto(String method) {
      if (method == null || method.equals("default")
            || method.equals("SSLv23_method")
            || method.equals("SSLv23_server_method")
            || method.equals("SSLv23_client_method")
            || method.equals("TLS_method")
            || method.equals("TLS_server_method")
            || method.equals("TLS_client_method")) {
         return "TLS";
      } else if (method.equals("TLSv1_method")
            || method.equals("TLSv1_server_method")
            || method.equals("TLSv1_client_method")) {
         return "TLSv1";
      } else if (method.equals("TLSv1_1_method")
            || method.equals("TLSv1_1_server_method")
            || method.equals("TLSv1_1_client_method")) {
         return "TLSv1.1";
      } else if (method.equals("TLSv1_2_method")
            || method.equals("TLSv1_2_server_method")
            || method.equals("TLSv1_2_client_method")) {
         return "TLSv1.2";
      } else if (method.equals("TLSv1_3_method")
            || method.equals("TLSv1_3_server_method")
            || method.equals("TLSv1_3_client_method")) {
         return "TLSv1.3";
      }
      foreign.fail("secure-context", "Unsupported method", method);
      return null;
   }
}
