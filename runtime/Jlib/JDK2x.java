/*=====================================================================*/
/*    serrano/prgm/project/bigloo/5.0.x/runtime/Jlib/JDK2x.java        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Mar 11 08:51:26 2008                          */
/*    Last change :  Fri Jun 26 14:54:09 2026 (serrano)                */
/*    Copyright   :  2008-26 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JDK 2x specifics                                                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The package                                                      */
/*---------------------------------------------------------------------*/
package bigloo;

import java.lang.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.Set;
import java.util.EnumSet;
import java.io.*;
import java.nio.channels.*;
import java.nio.file.*;
import java.nio.file.attribute.*;

/*---------------------------------------------------------------------*/
/*    JDK2x ...                                                        */
/*---------------------------------------------------------------------*/
public class JDK2x extends JDK {
   
   public Method getDeclaredMethodImpl(Class c, byte[] m) throws Exception {
      return c.getDeclaredMethod(new String(m));
   }
   
   public Object invokeImpl(Method m) throws Exception {
      return m.invoke(null);
   }
   
   public Object getExceptionCauseImpl(Throwable v) {
      if (v.getCause() != null && v.getCause() != v)
	 return v.getCause().getMessage().getBytes();
      else
	 return unspecified.unspecified; 
   }
   
   public Object invoke3Impl(Method m, int n, byte[] a) throws Exception {
      Object[] args = { n, a };
      return m.invoke(null, args);
   }
   
   public ServerSocket makeServerSocketImpl(String name, int port)
      throws IOException {

      InetSocketAddress addr;
      if (name != null) {
	 addr = new InetSocketAddress(name, port);
      } else {
	 addr = new InetSocketAddress(port);
      }
      
      ServerSocketChannel sch = ServerSocketChannel.open();
      ServerSocket sock = sch.socket();
      sock.bind(addr);
      return sock;
   }

   public Socket acceptImpl(ServerSocket sock, boolean blocking)
      throws IOException {
      ServerSocketChannel ssch = sock.getChannel();
      ssch.configureBlocking(blocking);
      SocketChannel sch = ssch.accept();

      if (sch != null) {
	 return sch.socket();
      }
      
      return null;
   }

   public byte[] passwordImpl(byte[] prompt) {
      String p = new String(prompt);
      return new String(System.console().readPassword(p)).getBytes();
   }
   
   public boolean truncateImpl(FileOutputStream stream, long size) {
      try {
	 FileChannel outChan = stream.getChannel();
	 try {
	    outChan.truncate(size);
	    
	    return true;
	 } catch (Exception _e) {
	    return false;
	 }
      } catch (Exception _e) {
	 return false;
      }
   }

   public int symlinkImpl(byte[] from, byte[] to) {
      Path target = Paths.get(new String(from));
      Path link = Paths.get(new String(to));
      try {
	 Files.createSymbolicLink(link, target);
	 return 0;
      } catch (Throwable e) {
	 return 1;
      }
   }

   public long accesstimeImpl(byte[] file) {
      Path path = Paths.get(new String(file));

      try {
	 BasicFileAttributes attrs =
	 Files.readAttributes(path, BasicFileAttributes.class);

	 return attrs.lastAccessTime().toMillis();
      } catch (Throwable e) {
	 return 0;
      }
   }
   
   public long modiftimeImpl(byte[] file) {
      Path path = Paths.get(new String(file));

      try {
	 BasicFileAttributes attrs =
	    Files.readAttributes(path, BasicFileAttributes.class);

	 return attrs.lastModifiedTime().toMillis();
      } catch (Throwable e) {
	 return 0;
      }
   }
   
   public long changetimeImpl(byte[] file) {
      Path path = Paths.get(new String(file));

      try {
	 BasicFileAttributes attrs =
	    Files.readAttributes(path, BasicFileAttributes.class);

	 return attrs.lastModifiedTime().toMillis();
      } catch (Throwable e) {
	 return 0;
      }
   }
   
   public long creationtimeImpl(byte[] file) {
      Path path = Paths.get(new String(file));

      try {
	 BasicFileAttributes attrs =
	    Files.readAttributes(path, BasicFileAttributes.class);

	 return attrs.creationTime().toMillis();
      } catch (Throwable e) {
	 return 0;
      }
   }

   public int utimesImpl(byte[] file, long atime, long mtime) {
      Path path = Paths.get(new String(file));

      try {
	 BasicFileAttributeView view =
	    Files.getFileAttributeView(path, BasicFileAttributeView.class);

	 view.setTimes(
	    FileTime.fromMillis(mtime),
	    FileTime.fromMillis(atime),
	    null);
	 return 0;
      } catch (Throwable e) {
	 return 0;
      }
   }

   public int filemodeImpl(byte[] file) throws IOException {
      Path path = Paths.get(new String(file));

      PosixFileAttributes attrs =
	 Files.readAttributes(path, PosixFileAttributes.class);
      
      Set<PosixFilePermission> perms = attrs.permissions();
      int mode = 0;

      if (perms.contains(PosixFilePermission.OWNER_READ)) mode |= 0400;
      if (perms.contains(PosixFilePermission.OWNER_WRITE)) mode |= 0200;
      if (perms.contains(PosixFilePermission.OWNER_EXECUTE)) mode |= 0100;

      if (perms.contains(PosixFilePermission.GROUP_READ)) mode |= 0040;
      if (perms.contains(PosixFilePermission.GROUP_WRITE)) mode |= 0020;
      if (perms.contains(PosixFilePermission.GROUP_EXECUTE)) mode |= 0010;

      if (perms.contains(PosixFilePermission.OTHERS_READ)) mode |= 0004;
      if (perms.contains(PosixFilePermission.OTHERS_WRITE)) mode |= 0002;
      if (perms.contains(PosixFilePermission.OTHERS_EXECUTE)) mode |= 0001;

      return mode;
   }

   public boolean chmodImpl(byte[] file, int mode) throws IOException {
      EnumSet<PosixFilePermission> perms = EnumSet.noneOf(PosixFilePermission.class);

      if ((mode & 0400) != 0) perms.add(PosixFilePermission.OWNER_READ);
      if ((mode & 0200) != 0) perms.add(PosixFilePermission.OWNER_WRITE);
      if ((mode & 0100) != 0) perms.add(PosixFilePermission.OWNER_EXECUTE);

      if ((mode & 0040) != 0) perms.add(PosixFilePermission.GROUP_READ);
      if ((mode & 0020) != 0) perms.add(PosixFilePermission.GROUP_WRITE);
      if ((mode & 0010) != 0) perms.add(PosixFilePermission.GROUP_EXECUTE);

      if ((mode & 0004) != 0) perms.add(PosixFilePermission.OTHERS_READ);
      if ((mode & 0002) != 0) perms.add(PosixFilePermission.OTHERS_WRITE);
      if ((mode & 0001) != 0) perms.add(PosixFilePermission.OTHERS_EXECUTE);

      Path path = Paths.get(new String(file));

      Files.setPosixFilePermissions(path, perms);
      return true;
   }
}
