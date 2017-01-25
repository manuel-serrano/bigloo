/*=====================================================================*/
/*    .../project/bigloo/api/ssl/src/Java/ssl_client_socket15.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jul  8 11:11:03 2005                          */
/*    Last change :  Wed Jan 25 09:44:06 2017 (serrano)                */
/*    Copyright   :  2005-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JDK>=1.5 SSL Java socket                                         */
/*=====================================================================*/

package bigloo.ssl;

import bigloo.*;

import java.io.*;
import java.net.*;
import javax.net.ssl.*;
import java.security.*;

import org.apache.commons.ssl.*;

public class ssl_client_socket extends client_socket {
   
   public static final int BGLSSL_SSLV2 = 1;
   public static final int BGLSSL_SSLV3 = 2;
   public static final int BGLSSL_SSLV23 = 3;
   public static final int BGLSSL_TLSV1 = 4;
   public static final int BGLSSL_DTLSV1 = 5;
   public static final int BGLSSL_TLSV1_1 = 6;
   public static final int BGLSSL_TLSV1_2 = 7;

   public ssl_client_socket(final byte[] hostname,
			    final int port,
			    final int ms,
			    final int proto,
			    final Object cert,
			    final Object pkey,
			    Object caList,
			    Object acceptedCerts,
			    byte[] inbuf,
			    byte[] outbuf ) {
      super();
      
      try {
	 SSLClient client = new SSLClient();

	 client.setCheckHostname(false);

	 // first the trusted CAs
	 if (caList != nil.nil) {
	    while (caList != nil.nil) {
	       pair cell = (pair)caList;
	       certificate ca = (certificate)ssl.certificate_native(cell.car);
	       client.addTrustMaterial(new TrustMaterial(ca.x509));
	       caList = cell.cdr;
	    }
	    // if we have trusted CAs we want authentication
	    client.setNeedClientAuth(true);
	 }
	 // then the key material
	 if (cert != bbool.faux) {
	    certificate c = (certificate)ssl.certificate_native(cert);
	    private_key pk = (private_key)ssl.private_key_native(pkey);
	    client.setKeyMaterial(new KeyMaterial(c.fname, pk.fname, 
						  new char[0]));
	 }
	 // now make the socket
	 SSLSocket sslsocket =
	    (SSLSocket)client.createSocket( new String( hostname ), port );

	 // check the accepted certs if any
	 if (acceptedCerts != bbool.faux) {
	    Principal p = sslsocket.getSession().getPeerPrincipal();
	    boolean ok = false;
	    while(acceptedCerts != nil.nil){
	       pair cell = (pair)acceptedCerts;
	       certificate c = (certificate)ssl.certificate_native(cell.car);
	       if (p.equals(c.x509.getSubjectX500Principal())){
		  ok = true;
		  break;
	       }
	       acceptedCerts = cell.cdr;
	    }
	    if (!ok) {
	       socket_error("make-ssl-client-socket",
			    "Presented certificate is not in the accept list",
			    p.getName());
	    }
	 }
      
	 socket = sslsocket;

	 set_socket_io_ports( socket, inbuf, outbuf );
      }
      catch (final Exception _e) {
	 _e.printStackTrace();
	 socket_error( "make-ssl-client-socket",
		       "cannot create socket",
		       unspecified.unspecified );
      }
   }

   public ssl_client_socket(final client_socket sock,
			    final int proto,
			    final Object cert,
			    final Object pkey,
			    Object caList,
			    Object acceptedCerts) {
      super();
      
      try {
	 SSLClient client = new SSLClient();
	 Socket socket = sock.socket;

	 client.setCheckHostname(false);

	 // first the trusted CAs
	 if (caList != nil.nil) {
	    while (caList != nil.nil) {
	       pair cell = (pair)caList;
	       certificate ca = (certificate)ssl.certificate_native(cell.car);
	       client.addTrustMaterial(new TrustMaterial(ca.x509));
	       caList = cell.cdr;
	    }
	    // if we have trusted CAs we want authentication
	    client.setNeedClientAuth(true);
	 }
	 // then the key material
	 if (cert != bbool.faux) {
	    certificate c = (certificate)ssl.certificate_native(cert);
	    private_key pk = (private_key)ssl.private_key_native(pkey);
	    client.setKeyMaterial(new KeyMaterial(c.fname, pk.fname, 
						  new char[0]));
	 }
	 // now make the socket
	 SSLSocket sslsocket =
	    (SSLSocket)client.createSocket(socket,
					   socket.getInetAddress().getHostName(),
					   socket.getPort(),
					   true);

	 // check the accepted certs if any
	 if (acceptedCerts != bbool.faux) {
	    Principal p = sslsocket.getSession().getPeerPrincipal();
	    boolean ok = false;
	    while(acceptedCerts != nil.nil){
	       pair cell = (pair)acceptedCerts;
	       certificate c = (certificate)ssl.certificate_native(cell.car);
	       if (p.equals(c.x509.getSubjectX500Principal())){
		  ok = true;
		  break;
	       }
	       acceptedCerts = cell.cdr;
	    }
	    if (!ok) {
	       socket_error("make-ssl-client-socket",
			    "Presented certificate is not in the accept list",
			    p.getName());
	    }
	 }
      
	 socket = sslsocket;

	 set_socket_io_ports( socket, new byte[ 8192 ], new byte[ 8192 ] );
      }
      catch (final Exception _e) {
	 _e.printStackTrace();
	 socket_error( "make-ssl-client-socket",
		       "cannot create socket",
		       unspecified.unspecified );
      }
   }

   public ssl_client_socket(final Socket socket,
			    final byte[] inbuf,
			    final byte[] outbuf ) {
      super(socket, inbuf, outbuf);
   }
   
   public boolean client_socketp( Object o ) {
      return (o instanceof client_socket) &&
	 (((client_socket)o).socket instanceof SSLSocket);
   }
}


   
