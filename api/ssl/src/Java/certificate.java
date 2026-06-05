package bigloo.ssl;

import bigloo.foreign;
import bigloo.nil;
import bigloo.obj;
import bigloo.pair;

import java.io.FileInputStream;
import java.util.Collection;
import java.util.ArrayList;
import java.util.List;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

public class certificate extends obj {

   public X509Certificate x509;
   public String fname;

   private certificate(X509Certificate cert, String fname) {
      this.x509 = cert;
      this.fname = fname;
   }

   private static List<X509Certificate> loadCerts(String fname) {
      try {
         CertificateFactory cf = CertificateFactory.getInstance("X.509");
         try (FileInputStream fis = new FileInputStream(fname)) {
            Collection<? extends java.security.cert.Certificate> certs =
               cf.generateCertificates(fis);
            List<X509Certificate> result = new ArrayList<X509Certificate>();
            for (java.security.cert.Certificate c : certs) {
               result.add((X509Certificate) c);
            }
            return result;
         }
      } catch (Exception x) {
         foreign.fail("certificate-load", "Could not load certificate", x);
         return null;
      }
   }

   public static Object load(byte[] file) {
      String fname = new String(file);
      List<X509Certificate> certs = loadCerts(fname);
      if (certs == null || certs.isEmpty()) {
         foreign.fail("certificate-load", "No certificate found", file);
      }
      return ssl.make_certificate(new certificate(certs.get(0), fname));
   }

   public static Object load_pem(byte[] file) {
      String fname = new String(file);
      List<X509Certificate> certs = loadCerts(fname);
      obj ret = nil.nil;
      for (X509Certificate c : certs) {
         ret = new pair(ssl.make_certificate(new certificate(c, fname)), ret);
      }
      return ret;
   }

   public static byte[] subject(Object cert) {
      certificate c = (certificate) ssl.certificate_native(cert);
      return c.x509.getSubjectX500Principal().getName().getBytes();
   }

   public static byte[] issuer(Object cert) {
      certificate c = (certificate) ssl.certificate_native(cert);
      return c.x509.getIssuerX500Principal().getName().getBytes();
   }
}
