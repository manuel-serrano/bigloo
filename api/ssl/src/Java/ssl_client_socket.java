package bigloo.ssl;

import bigloo.client_socket;
import bigloo.symbol;
import bigloo.unspecified;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

public class ssl_client_socket extends client_socket {

   public ssl_client_socket(final byte[] hostname,
                            final int port,
                            final int timeoutUs,
                            final int proto,
                            final Object cert,
                            final Object pkey,
                            Object caList,
                            Object acceptedCerts,
                            byte[] inbuf,
                            byte[] outbuf,
                            symbol domain) {
      super();

      try {
         SSLContext ctx = ssl_utils.buildSSLContext(proto, cert, pkey, caList);
         SSLSocketFactory factory = ctx.getSocketFactory();
         String host = new String(hostname);
         InetAddress addr = resolveAddress(host, domain);
         int timeoutMs = timeoutUs / 1000;

         SSLSocket sslsocket;
         if (timeoutMs > 0) {
            Socket plain = new Socket();
            plain.connect(new InetSocketAddress(addr, port), timeoutMs);
            sslsocket = (SSLSocket) factory.createSocket(
               plain, addr.getHostAddress(), port, true);
         } else {
            sslsocket = (SSLSocket) factory.createSocket(addr, port);
         }

         ssl_utils.checkAcceptedCerts(sslsocket, acceptedCerts);

         socket = sslsocket;
         set_socket_io_ports(socket, inbuf, outbuf);
      } catch (java.net.UnknownHostException e) {
         bigloo.runtime.Llib.error.bgl_system_failure(
            bigloo.foreign.BGL_IO_UNKNOWN_HOST_ERROR,
            "make-ssl-client-socket".getBytes(),
            "unknown or misspelled host name".getBytes(),
            hostname);
      } catch (java.net.SocketTimeoutException e) {
         bigloo.runtime.Llib.error.bgl_system_failure(
            bigloo.foreign.BGL_IO_TIMEOUT_ERROR,
            "make-ssl-client-socket".getBytes(),
            "connection timed out".getBytes(),
            hostname);
      } catch (javax.net.ssl.SSLHandshakeException e) {
         bigloo.runtime.Llib.error.bgl_system_failure(
            bigloo.foreign.BGL_IO_ERROR,
            "make-ssl-client-socket".getBytes(),
            ("SSL handshake failed: " + e.getMessage()).getBytes(),
            hostname);
      } catch (Exception _e) {
         bigloo.runtime.Llib.error.bgl_system_failure(
            bigloo.foreign.BGL_IO_ERROR,
            "make-ssl-client-socket".getBytes(),
            (_e.getMessage() != null ? _e.getMessage() : "cannot create socket").getBytes(),
            hostname);
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
         SSLContext ctx = ssl_utils.buildSSLContext(proto, cert, pkey, caList);
         Socket existing = sock.socket;
         SSLSocket sslsocket = (SSLSocket) ctx.getSocketFactory()
            .createSocket(existing,
                          existing.getInetAddress().getHostName(),
                          existing.getPort(),
                          true);

         ssl_utils.checkAcceptedCerts(sslsocket, acceptedCerts);

         socket = sslsocket;
         set_socket_io_ports(socket, new byte[8192], new byte[8192]);
      } catch (javax.net.ssl.SSLHandshakeException e) {
         bigloo.runtime.Llib.error.bgl_system_failure(
            bigloo.foreign.BGL_IO_ERROR,
            "client-socket-use-ssl!".getBytes(),
            ("SSL handshake failed: " + e.getMessage()).getBytes(),
            unspecified.unspecified);
      } catch (Exception _e) {
         bigloo.runtime.Llib.error.bgl_system_failure(
            bigloo.foreign.BGL_IO_ERROR,
            "client-socket-use-ssl!".getBytes(),
            (_e.getMessage() != null ? _e.getMessage() : "cannot create socket").getBytes(),
            unspecified.unspecified);
      }
   }

   public ssl_client_socket(final Socket socket,
                            final byte[] inbuf,
                            final byte[] outbuf) {
      super(socket, inbuf, outbuf);
   }

   public static bigloo.socket bgl_client_socket_use_ssl(bigloo.socket sock,
                                                          int proto,
                                                          Object cert,
                                                          Object pkey,
                                                          Object caList,
                                                          Object acceptedCerts) {
      return new ssl_client_socket((client_socket) sock,
                                   proto, cert, pkey, caList, acceptedCerts);
   }

   public static boolean bgl_ssl_client_socketp(Object o) {
      return (o instanceof client_socket)
         && (((client_socket) o).socket instanceof SSLSocket);
   }
}
