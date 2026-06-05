package bigloo.ssl;

import bigloo.foreign;
import bigloo.obj;

import java.math.BigInteger;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import javax.crypto.KeyAgreement;
import javax.crypto.spec.DHParameterSpec;
import javax.crypto.spec.DHPublicKeySpec;

/**
 * JVM wrapper for Diffie-Hellman operations.
 */
public class dh_ctx extends obj {

   // These fields hold BigInteger values but are typed as Object because
   // Bigloo's JVM FFI maps ::obj fields to Object. Typing them as
   // BigInteger would cause a mismatch with the generated field accessors.
   public Object p;
   public Object g;
   public Object publicKey;
   public Object privateKey;

   public dh_ctx() {
      p = BigInteger.ZERO;
      g = BigInteger.ZERO;
   }

   public int size() {
      BigInteger bp = (BigInteger) p;
      if (bp.equals(BigInteger.ZERO)) return 0;
      return (bp.bitLength() + 7) / 8;
   }

   public boolean generateParameters(int bits, int generator) {
      try {
         g = BigInteger.valueOf(generator);
         p = BigInteger.probablePrime(bits, new java.security.SecureRandom());
         return true;
      } catch (Exception x) {
         return false;
      }
   }

   public boolean generateKey() {
      try {
         BigInteger bp = (BigInteger) p;
         BigInteger bg = (BigInteger) g;
         DHParameterSpec params = new DHParameterSpec(bp, bg);
         KeyPairGenerator kpg = KeyPairGenerator.getInstance("DH");
         kpg.initialize(params);
         KeyPair kp = kpg.generateKeyPair();

         javax.crypto.interfaces.DHPublicKey pub =
            (javax.crypto.interfaces.DHPublicKey) kp.getPublic();
         javax.crypto.interfaces.DHPrivateKey priv =
            (javax.crypto.interfaces.DHPrivateKey) kp.getPrivate();

         publicKey = pub.getY();
         privateKey = priv.getX();
         return true;
      } catch (Exception x) {
         foreign.fail("dh-generate-key", x.getMessage(), "dh");
         return false;
      }
   }

   public byte[] computeKey(Object otherPubKey) {
      try {
         BigInteger bp = (BigInteger) p;
         BigInteger bg = (BigInteger) g;
         BigInteger otherPublicKey = (BigInteger) otherPubKey;

         javax.crypto.spec.DHPrivateKeySpec privSpec =
            new javax.crypto.spec.DHPrivateKeySpec((BigInteger) privateKey, bp, bg);
         KeyFactory kf = KeyFactory.getInstance("DH");
         java.security.PrivateKey privKey = kf.generatePrivate(privSpec);

         DHPublicKeySpec pubSpec = new DHPublicKeySpec(otherPublicKey, bp, bg);
         java.security.PublicKey pubKey = kf.generatePublic(pubSpec);

         KeyAgreement ka = KeyAgreement.getInstance("DH");
         ka.init(privKey);
         ka.doPhase(pubKey, true);
         byte[] secret = ka.generateSecret();

         int dhSize = size();
         if (secret.length == dhSize) {
            return secret;
         }
         byte[] padded = new byte[dhSize];
         System.arraycopy(secret, 0, padded, dhSize - secret.length,
                          secret.length);
         return padded;
      } catch (Exception x) {
         foreign.fail("dh-compute-key", x.getMessage(), "dh");
         return null;
      }
   }

   public Object check() {
      BigInteger bp = (BigInteger) p;
      if (bp.equals(BigInteger.ZERO)) return "DH-CHECK-P-NOT-PRIME".getBytes();
      if (!bp.isProbablePrime(20)) return "DH-CHECK-P-NOT-PRIME".getBytes();
      return bigloo.foreign.BFALSE;
   }

   public Object checkPubKey(Object pub) {
      BigInteger bpub = (BigInteger) pub;
      BigInteger bp = (BigInteger) p;
      if (bpub.compareTo(BigInteger.ONE) <= 0) return "DH-CHECK-INVALID-PUB-KEY".getBytes();
      if (bpub.compareTo(bp.subtract(BigInteger.ONE)) >= 0) return "DH-CHECK-INVALID-PUB-KEY".getBytes();
      return bigloo.foreign.BFALSE;
   }

   // BigInteger ↔ byte[] conversions for the bn-* Scheme functions

   public static Object binToBn(byte[] buf, int len) {
      byte[] data = new byte[len];
      System.arraycopy(buf, 0, data, 0, len);
      return new BigInteger(1, data);
   }

   public static byte[] bnToBin(Object bn) {
      byte[] b = ((BigInteger) bn).toByteArray();
      // BigInteger.toByteArray() may have a leading zero byte for sign
      if (b.length > 0 && b[0] == 0) {
         byte[] trimmed = new byte[b.length - 1];
         System.arraycopy(b, 1, trimmed, 0, trimmed.length);
         return trimmed;
      }
      return b;
   }

   public static int bnNumBytes(Object bn) {
      return (((BigInteger) bn).bitLength() + 7) / 8;
   }

   public static Object bnNew() {
      return BigInteger.ZERO;
   }

   public static Object bnFromWord(int w) {
      return BigInteger.valueOf(w);
   }
}
