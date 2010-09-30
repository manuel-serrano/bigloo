#*=====================================================================*/
#*    serrano/prgm/project/bigloo/autoconf/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Thu Jun 19 20:48:50 1997                          */
#*    Last change :  Thu Sep 23 14:32:18 2010 (serrano)                */
#*    Copyright   :  1997-2010 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The global autoconf Makefile.                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.config
include ../Makefile.misc

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
POPULATION	= Makefile \
                  bigloo_config.h.in bigloo_gc.h.in \
                  configure.java.in configure.cs.in \
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
                  sigchld sigaction \
                  doubleprec xemacspath ldstaticopt ldsharedopt \
                  readline ldextraopt \
                  socket getaddrinfo socklen setsockopt inet_aton inet_pton \
                  unixsocket info cctest ccoptim ccdiv0 \
                  ccprof termio termios installinfo installinfodir dlopen \
                  user xemacsbin gmaketest ccpic ccpic2 \
                  ziptest javatest jartest \
                  oscharset strip cygwindospath select fcntl ccstrip \
                  emacsbrand emacslisppath stacksize gcstd \
                  thread pthread pthreadlock pthreadlocalstorage \
                  processor dotnettest sendfile sleep posixfileops \
                  timezone timeval \
                  gcflags mmap macosx openssl openssl-dtls sqlite \
                  stdint stdlibint unistdint maemo.arm android \
                  gcversion gchavedoblocking gchavestartblocking rdynamic \
                  gstreamer gstreameraudio dbus gmp limbs signbit \
                  getuid gecos getprotoent machdep arflags mixer \
                  gcconfigureopt ccfortify environ setenv androidmkdir

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
	cp runtest $(LIBDIR)/$(FILDIR)/runtest && \
          chmod $(MODEXE) $(LIBDIR)/$(FILDIR)/runtest; \

uninstall:
	$(RM) -f $(LIBDIR)/$(FILDIR)/runtest
