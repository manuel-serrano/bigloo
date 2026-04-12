package bigloo.ssl;

import bigloo.bbool;
import bigloo.client_socket;
import bigloo.nil;
import bigloo.server_socket;
import bigloo.symbol;
import bigloo.unspecified;

import java.io.IOException;
import java.net.InetAddress;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;

public class ssl_server_socket extends server_socket {

   private Object acceptedCerts;

   public ssl_server_socket(Object name, final int port,
                            final int proto,
                            final Object cert,
                            final Object pkey,
                            Object caList,
                            Object acceptedCerts,
                            int backlog,
                            bigloo.symbol domain) {
      super();
      try {
         SSLContext ctx = ssl_utils.buildSSLContext(proto, cert, pkey, caList);

         if (name != bbool.faux) {
            InetAddress addr = InetAddress.getByName(new String((byte[]) name));
            server_socket = ctx.getServerSocketFactory()
               .createServerSocket(port, backlog, addr);
         } else {
            server_socket = ctx.getServerSocketFactory()
               .createServerSocket(port, backlog);
         }

         if (caList != nil.nil) {
            ((SSLServerSocket) server_socket).setNeedClientAuth(true);
         }

         this.acceptedCerts = acceptedCerts;
      } catch (Exception _e) {
         socket_error("make-ssl-server-socket",
                      "cannot create socket",
                      unspecified.unspecified);
      }
   }

   public client_socket accept(byte[] inbuf, byte[] outbuf, boolean errp)
      throws IOException, SecurityException {
      final SSLSocket accepted_socket = (SSLSocket) server_socket.accept();
      if (accepted_socket == null)
         throw new IOException("Nothing to accept");

      try {
         ssl_utils.checkAcceptedCerts(accepted_socket, acceptedCerts);
      } catch (Exception _e) {
         socket_error("server-socket-accept",
                      "certificate check failed",
                      _e.getMessage());
      }

      return new ssl_client_socket(accepted_socket, inbuf, outbuf);
   }

   public static boolean bgl_ssl_server_socketp(Object o) {
      return (o instanceof ssl_server_socket);
   }
}
