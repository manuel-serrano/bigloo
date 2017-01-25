/*=====================================================================*/
/*    .../project/bigloo/api/ssl/src/Java/ssl_server_socket15.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 10:53:03 2000                          */
/*    Last change :  Wed Jan 25 09:44:39 2017 (serrano)                */
/*    Copyright   :  2000-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Server Socket implementation for the JVM back-end.           */
/*=====================================================================*/
package bigloo.ssl;

import bigloo.*;

import java.io.*;
import java.net.*;
import javax.net.ssl.*;
import java.security.*;

import org.apache.commons.ssl.*;

public class ssl_server_socket extends server_socket {

  private Object acceptedCerts;

   public ssl_server_socket( Object name, final int port ,
                             final int proto,
                             final Object cert,
                             final Object pkey,
                             Object caList,
                             Object acceptedCerts,
			     int backlog,
			     boolean ipv6 ) {
      super();
      try {
        SSLServer server = new SSLServer();

        server.setCheckHostname(false);

        // first the trusted CAs
        if(caList != nil.nil){
          while(caList != nil.nil){
            pair cell = (pair)caList;
            certificate ca = (certificate)ssl.certificate_native(cell.car);
            server.addTrustMaterial(new TrustMaterial(ca.x509));
            caList = cell.cdr;
          }
          // if we have trusted CAs we want authentication
          server.setNeedClientAuth(true);
        }
        // then the key material
        if(cert != bbool.faux){
          certificate c = (certificate)ssl.certificate_native(cert);
          private_key pk = (private_key)ssl.private_key_native(pkey);
          server.setKeyMaterial(new KeyMaterial(c.fname, pk.fname, new char[0]));
        }
        // now make the socket
        if(name != bbool.faux){
          InetAddress addr = InetAddress.getByName(new String((byte[])name));
          server_socket = server.createServerSocket(port, 10, addr);
        }else
          server_socket = server.createServerSocket( port );

        // remember the accepted certs
        this.acceptedCerts = acceptedCerts;
      }
      catch( final Exception _e ) {
        _e.printStackTrace();
	 socket_error( "make-ssl-server-socket",
		       "cannot create socket",
		       unspecified.unspecified );
      }

   }

   public client_socket accept( byte[] inbuf, byte[] outbuf, boolean errp )
      throws IOException, SecurityException {
      // WARNING this blocking parameter is not called blocking in C
      // it is called buffered, and C does not do non-blocking [yet], so
      // we don't support it [yet] (but we can)
     final SSLSocket accepted_socket = (SSLSocket)server_socket.accept();
      if( accepted_socket == null )
	 throw new IOException("Nothing to accept");

      // check the accepted certs if any
      if(acceptedCerts != bbool.faux){
        Principal p = accepted_socket.getSession().getPeerPrincipal();
        boolean ok = false;
        while(acceptedCerts != nil.nil){
          pair cell = (pair)acceptedCerts;
          certificate c = (certificate)ssl.certificate_native(cell.car);
          if(p.equals(c.x509.getSubjectX500Principal())){
            ok = true;
            break;
          }
          acceptedCerts = cell.cdr;
        }
        if(!ok){
          socket_error( "server-socket-accept",
                        "Presented certificate is not in the accept list",
                        p.getName() );
        }
      }

      return new ssl_client_socket( accepted_socket, inbuf, outbuf );
   }

   public static boolean server_socketp( Object o ) {
      return (o instanceof ssl_server_socket);
   }
}
