#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/autoconf/Makefile             */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Jun 19 20:48:50 1997                          */
#*    Last change :  Tue Mar 24 12:22:05 2020 (serrano)                */
#*    Copyright   :  1997-2020 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The global autoconf Makefile.                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config
include ../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile bigloo.sh.in bglafile.sh.in bglmco.sh.in \
		  bgljfile.sh.in bgltags.sh.in bgldepend.sh.in \
                  bglrun.sh.in bglld.sh.in bglmake.sh.in bglprof.sh.in \
                  bglpp.sh.in bgljas.sh.in \
                  bigloo_config.h.in bigloo_gc.h.in \
                  configure.java.in \
                  bigloo_config.sch.in \
                  mkruntest runtest.in \
                  os osversion cpu \
                  checkmember checkinclude \
                  cnstalign longlong labs llabs strtoll strtoull \
                  sigpipe sigsetmask \
                  alignment enforcedcnstalign pipe sigsetmask.ucb \
                  alloca allocah getcwd ptrsize stackdown \
                  ascpp getwd ranlib unix dirname \
                  bcopy indent setjmp setjmp.sparc winreg \
                  bigendian ldshare ldpreload ldsoname sigprocmask \
                  sigchld sigaction siginfo \
                  doubleprec xemacspath ldstaticopt ldsharedopt \
                  readline ldextraopt \
                  socket getaddrinfo getifaddrs gethwaddrs socklen setsockopt \
                  inet_aton inet_pton \
                  unixsocket info cctest ccoptim ccbracket ccdiv0 ccwarning \
                  ccprof termio termios installinfo installinfodir dlopen \
                  user xemacsbin gmaketest ccpic ccpic2 ccssex86 \
                  ziptest javatest jartest \
                  oscharset strip cygwindospath select fcntl ccstrip \
                  emacsbrand emacslisppath stacksize gcstd \
                  thread pthread pthreadlock pthreadspinlock pthreadcancel \
                  pthreadmutexrec pthreadtimedjoin pthreadlocalstorage \
                  pthreadcondvtimedwait processor sendfile sleep posixfileops \
                  timezone timeval gcflags mmap macosx sqlite \
                  openssl openssl-dtls openssl-tls11 openssl-tls1 \
                  openssl-tls12 openssl-tls13 openssl-rand \
                  openssl-v2 openssl-v3 \
                  openssl-v23 openssl-getter \
                  stdint stdlibint unistdint maemo.arm android \
                  gcversion gchavedoblocking gchavestartblocking nbprocs \
                  gstreamer gstreameraudio \
                  alsa pulseaudio mpg123 flac pcre pcrefreestudy avahi \
                  dbus gmp resolv resolvsym unistring limbs signbit rdynamic \
                  getuid getgid gecos getprotoent machdep arflags mixer \
                  config.guess ccfortify environ setenv symlink \
                  androidmkdir bgl-pkg-config phidget phidgetlibdir \
                  gmtoff localtime_r gmtime_r timegm libuv getgroups ioctl \
                  pthreadkill syslog overflow semaphore lockf likely \
                  gccattrs getrlimit c99stackalloc tools

#*---------------------------------------------------------------------*/
#*    pop ...                                                          */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=autoconf/%)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean
clean:
	@ find . \( -name '*[~%]'                   \
                       -o -name '.??*[~%]'          \
                       -o -name '#*#'               \
                       -o -name '?*#'               \
                       -o -name \*core \)           \
                     -type f -exec rm {} \;   
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
distclean: clean

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	@ if [ `pwd` = $(BOOTDIR)autoconf ]; then     \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`";   \
             exit 1;                                                 \
          fi
	@ $(MAKE) clean
	@ chmod a+rx $(POPULATION)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	cp runtest $(DESTDIR)$(LIBDIR)/$(FILDIR)/runtest && \
          chmod $(MODEXE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/runtest; \

uninstall:
	$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/runtest
