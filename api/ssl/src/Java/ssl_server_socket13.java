package bigloo.ssl;

import bigloo.*;

import java.io.*;
import java.net.*;

public class ssl_server_socket extends server_socket {

   public ssl_server_socket( Object name, final int port,
                             final int proto,
                             final Object cert,
                             final Object pkey,
                             Object caList,
                             Object acceptedCerts,
			     int backlog,
			     boolean ipv6 ) {
      super();
   }
   
   public static boolean server_socketp( Object o ) {
      return false;
   }
}
