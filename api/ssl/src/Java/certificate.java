package bigloo.ssl;

import bigloo.*;

import java.util.*;
import java.security.cert.*;
import javax.naming.ldap.*;
import javax.naming.*;

import org.apache.commons.ssl.*;

public class certificate extends obj{

  public X509Certificate x509;
  // used only for building KeyMaterial (grrr)
  public String fname;
  
  private certificate(byte[] file){
    try{
      String fname = new String(file);
      TrustMaterial cert = new TrustMaterial(fname);
      if(cert.getSize() != 1)
        foreign.fail("certificate-load",
                     "wrong number of certificates in PEM file (!= 1)",
                     file);
      SortedSet set = cert.getCertificates();
      this.x509 = (X509Certificate)set.first();
      this.fname = fname;
    }catch(Exception x ){
      foreign.fail("certificate-load",
                   "Could not load certificate",
                   x);
    }
  }

  private certificate(X509Certificate cert){
    this.x509 = cert;
  }

  public static Object load(byte[] file){
    return ssl.make_certificate(new certificate(file));
  }

  public static Object load_pem(byte[] file){
    try{
      String fname = new String(file);
      TrustMaterial cert = new TrustMaterial(fname);
      SortedSet set = cert.getCertificates();
      Iterator it = set.iterator();
      obj ret = nil.nil;
      
      while(it.hasNext()){
        certificate c = new certificate((X509Certificate)it.next());
        ret = new pair(ssl.make_certificate(c), ret);
      }
      
      return ret;
    }catch(Exception x ){
      foreign.fail("certificate-load",
                   "Could not load certificates",
                   x);
      return nil.nil;
    }
  }

/*   public static byte[] subject(Object cert){                        */
/*     String RFC2253Name =                                            */
/*       ((certificate)cert).x509.getSubjectX500Principal().getName(); */
/*     byte[] ret = getRFC2253Value(RFC2253Name, "CN");                */
/*     return ret != null ? ret : RFC2253Name.getBytes();              */
/*   }                                                                 */
/*                                                                     */
/*   public static byte[] issuer(Object cert){                         */
/*     String RFC2253Name =                                            */
/*       ((certificate)cert).x509.getIssuerX500Principal().getName();  */
/*     byte[] ret = getRFC2253Value(RFC2253Name, "CN");                */
/*     return ret != null ? ret : RFC2253Name.getBytes();              */
/*   }                                                                 */
/*                                                                     */
/*   private static byte[] getRFC2253Value(String RFC2253Name, String prefix){ */
/*     try{                                                            */
/*       LdapName name = new LdapName(RFC2253Name);                    */
/*       for(int i=0;i<name.size();i++){                               */
/*         Rdn part = new Rdn(name.get(i));                            */
/*         if(prefix.equals(part.getType()))                           */
/*           return part.getValue().toString().getBytes();             */
/*       }                                                             */
/*     }catch(InvalidNameException x){                                 */
/*       x.printStackTrace();                                          */
/*     }                                                               */
/*     return null;                                                    */
/*   }                                                                 */
}
