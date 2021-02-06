#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/Makefile                      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 13:40:15 1998                          */
#*    Last change :  Fri May  1 09:55:43 2020 (serrano)                */
#*    Copyright   :  1998-2020 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The main Bigloo Makefile. Here is a short description of the     */
#*    makefile entries.                                                */
#*                                                                     */
#*    Public entries:                                                  */
#*      boot........... Compile Bigloo on a bare plateform.            */
#*      boot-api....... Compile Bigloo APIs.                           */
#*      compile-bee0...                                                */
#*      compile-bee.... Compile BEE on a bare plateform.               */
#*                      The entry uses auxiliary public points.        */
#*      install........ Install a compiled Bigloo.                     */
#*      install-bee0...                                                */
#*      install-bee.... Install a compiled Bigloo. This uses           */
#*                      install-xxx entry points).                     */
#*      install-api.... Install Bigloo APIs.                           */
#*      uninstall...... Uninstall the bigloo system and Bee.           */
#*      uninstall-bee0.                                                */
#*      uninstall-bee.. Uninstall Bee. This uses uninstall-xxx points. */
#*      unconfigure.... Unconfigure the bigloo system and Bee.         */
#*      test........... Test a compiled Bigloo.                        */
#*      clean.......... Cleaning.                                      */
#*      distclean...... Cleaning.                                      */
#*                                                                     */
#*    Private entries:                                                 */
#*      fullbootstrap.. Bootstrap a development compiler (private)     */
#*      c-fullbootstrap Bootstrap a development compiler (private)     */
#*      bigboot........ Compile Bigloo on already provided plateform.  */
#*                      This is not guarantee to work because it       */
#*                      requires a compatible installed Bigloo         */
#*                      compiler (public)                              */
#*      distrib........ Produces an official bigloo tarball file       */
#*                      (private). The tarball file is built from a    */
#*                      bootstrapped compiler that is freshly checked  */
#*                      out from the revision system. Uses (true-      */
#*                      distrib, bigloo.tar.gz, bigloo.tar).           */
#*      distrib-jvm.... Produces an official bigloo zip file with      */
#*                      default back-end set to jvm (for Windows and   */
#*                      MacIntosh boxes).                              */
#*      zip............ aka for distrib-jvm                            */
#*      rpm............ Produces an rpm file for Bigloo.               */   
#*      newrevision.... Changes the development Bigloo revision.       */
#*      pop............ The population that composes Bigloo.           */
#*      popfilelist.... The sort file list of the Bigloo population.   */
#*      revision....... commit a revision (uses prcs-revision and      */
#*                      mercurial-revision).                           */
#*      checkout....... commit a checkout (uses prcs-checkout and      */
#*                      mercurial-checkout).                           */
#*      checkgmake..... Check that make is gmake.                      */
#*=====================================================================*/
 
#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
-include Makefile.buildconfig
-include Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the shell to be used
SHELL           = /bin/sh
# The directory where to build and install a distribution
DISTRIBTMPDIR	= /tmp
DISTRIBDIR	= $$HOME/prgm/distrib
BUILDDIRNAME 	= bigloo
# The Bigloo html page directory
HTMLPAGEDIR	= $$HOME/public_html/bigloo
# The library to be installed on the ftp server
FTP_LIBRARIES	= contrib/lib-example.tar.gz
# the libc we are testing for version
LIBC		= /usr/lib/libc.so
# the library C version
LIBCVERSION	= 6
# the rpm architecture (for now only i386 is supported)
RPMARCH		= i686
# the root directory where to remove directory after the rpm contruction
RPMBASEDIR	= /usr/src/RPM
RPMSOURCEDIR	= $(RPMBASEDIR)/SOURCES
RPMBUILDDIR	= $(RPMBASEDIR)/BUILD
# The rpm source directory
RPMSOURCESDIR	= $(RPMBASEDIR)/SOURCES
RPMRPMDIR	= $(RPMBASEDIR)/RPMS
# The JVM standard installation directory
JVMBASEDIR	= "C:\\\\\\\\Bgl`echo $(RELEASE) | sed -e 's/[.]//'`"
# gzip
GZIP		= gzip -9
# zip
ZIP		= zip
# The revision system, either prcs, or mercurial
REVISIONSYSTEM	= git
# The message to log the revision
LOGMSG		= ""
# Sudo command
SUDO		= sudo

BOOTCAPI	= no

#*---------------------------------------------------------------------*/
#*    The directory that compose a version                             */
#*---------------------------------------------------------------------*/
DIRECTORIES	= cigloo \
                  jigloo \
                  bench \
	          autoconf \
                  etc \
                  examples \
                  recette \
                  tools \
                  xlib \
                  runtime \
	          comptime \
		  bdb \
                  bde \
                  bmacs \
                  manuals \
                  doc \
                  win32 \
                  arch \
                  www \
                  api \
                  srfi \
                  bdl \
                  bglpkg \
                  gc \
                  gmp \
                  pcre \
                  libunistring \
                  libuv

#*---------------------------------------------------------------------*/
#*    The file that have to be removed when building a distrib.        */
#*---------------------------------------------------------------------*/
NO_DIST_FILES	= .bigloo.prcs_aux \
		  bigloo.lsm \
		  www

#*---------------------------------------------------------------------*/
#*    boot ...                                                         */
#*    -------------------------------------------------------------    */
#*    Boot a new Bigloo system on a new host. This boot makes use      */
#*    of the pre-compiled C files.                                     */
#*---------------------------------------------------------------------*/
.PHONY: checkconf boot boot-jvm boot-bde boot-api boot-bglpkg

build: checkconf boot

checkconf:
	@ if ! [ -f "lib/bigloo/$(RELEASE)/bigloo.h" ]; then \
	  echo "you must configure before building!"; \
	  exit 1; \
	fi

boot: boot-c

boot-c: checkgmake
	if [ "$(GMPCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gmp boot; \
        fi
	if [ "$(LIBUVCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C libuv boot; \
        fi
	if [ "$(GCCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gc boot; \
        fi
	if [ "$(PCRECUSTOM)" = "yes" ]; then \
	  $(MAKE) -C pcre boot; \
        fi
	if [ "$(UNISTRINGCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C libunistring boot; \
        fi
	if [ -x $(BGLBUILDBINDIR)/bigloo ]; then \
	  $(MAKE) -C runtime .afile && \
	  $(MAKE) -C runtime heap && \
	  $(MAKE) -C runtime boot && \
	  $(MAKE) -C comptime bigloo && \
	  $(MAKE) -C comptime doboot; \
	else \
	  $(MAKE) -C runtime boot && \
	  $(MAKE) -C comptime boot && \
	  $(MAKE) -C runtime heap; \
	fi
	if [ "$(JVMBACKEND)" = "yes" ]; then \
	  $(MAKE) boot-jvm; \
        fi
	$(MAKE) boot-bde
	$(MAKE) boot-api
	if [ "$(ENABLE_BGLPKG)" = "yes" ]; then \
	  $(MAKE) boot-bglpkg; \
        fi
	@ echo "Boot done..."
	@ echo "-------------------------------"

boot-jvm: checkgmake
	$(MAKE) -C runtime boot-jvm

boot-bde:
	$(MAKE) -C bde boot BFLAGS="$(BFLAGS) $(SHRD_BDE_OPT)"

boot-api:
	$(MAKE) -C api boot BFLAGS="$(BFLAGS)"

boot-bglpkg:
	$(MAKE) -C bglpkg BFLAGS="$(BFLAGS)"

#*---------------------------------------------------------------------*/
#*    cross-rts ...                                                    */
#*    -------------------------------------------------------------    */
#*    This entry builds the Bigloo runtime system using a cross        */
#*    C compiler. It needs an operational pre-install native Bigloo    */
#*    compiler and a cross C compiler on the current host.             */
#*---------------------------------------------------------------------*/
cross-rts: checkgmake
	$(MAKE) -C runtime boot
	$(MAKE) -C runtime heap
	if [ "$(JVMBACKEND)" = "yes" ]; then \
           (PATH=$(BOOTBINDIR):$$PATH; export PATH; \
            $(MAKE) -C runtime boot-jvm); \
        fi
	$(MAKE) -C api boot)
	@ echo "CROSS-RTS done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    manuals                                                          */
#*---------------------------------------------------------------------*/
manuals:
	@ (cd manuals && $(MAKE) html ps)
	@ (cd manuals && $(MAKE) compile-bee)

#*---------------------------------------------------------------------*/
#*    manual-pdf                                                       */
#*---------------------------------------------------------------------*/
manual-pdf:
	@ (cd manuals && $(MAKE) bigloo.pdf)

#*---------------------------------------------------------------------*/
#*    bigboot ...                                                      */
#*    -------------------------------------------------------------    */
#*    Boot a new Bigloo system on a an host. This boot uses an already */
#*    installed Bigloo on that host. That is, it recompiles, all the   */
#*    Scheme source files.                                             */
#*    -------------------------------------------------------------    */
#*    To use this entry:                                               */
#*      1- configure the system:                                       */
#*          ./configure --bootconfig                                   */
#*      2- type something like:                                        */
#*          make bigboot BGLBUILDBINDIR=/usr/local/bin                 */
#*---------------------------------------------------------------------*/
bigboot: 
	@ if [ "$(BGLBUILDBINDIR) " = " " ]; then \
            echo "*** Error, the variable BGLBUILDBINDIR is unbound"; \
            echo "Use \"$(MAKE) dobigboot\" if you know what you are doing!"; \
            exit 0; \
          else \
            $(MAKE) dobigboot; \
          fi

dobigboot:
	@ $(MAKE) -C gc clean
	@ $(MAKE) -C gc uninstall
	@ $(MAKE) -C gc boot
	@ if [ "$(GMPCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gmp clean; \
	  $(MAKE) -C gmp boot; \
        fi
	@ if [ "$(PCRECUSTOM)" = "yes" ]; then \
	  $(MAKE) -C pcre clean; \
	  $(MAKE) -C pcre boot; \
        fi
	@ if [ "$(UNISTRINGCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C libunistring clean; \
	  $(MAKE) -C libunistring boot; \
        fi
	@ mkdir -p bin
	@ $(MAKE) -C runtime bigboot BBFLAGS="-w"
	@ $(MAKE) -C comptime -i touchall
	@ $(MAKE) -C comptime bigboot BBFLAGS="-w -unsafeh"
	@ $(MAKE) -C runtime heap-c BIGLOO=$(BOOTDIR)/bin/bigloo
	@ $(MAKE) -C comptime BIGLOO=$(BOOTDIR)/bin/bigloo
	@ $(MAKE) -C runtime clean-quick
	@ $(MAKE) -C runtime heap libs BIGLOO=$(BOOTDIR)/bin/bigloo
	@ $(MAKE) -C bde clean boot BIGLOO=$(BOOTDIR)/bin/bigloo
	@ $(MAKE) -C api clean-quick BIGLOO=$(BOOTDIR)/bin/bigloo
	@ echo "Big boot done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    compile-bee                                                      */
#*    -------------------------------------------------------------    */
#*    Once the system is booted. It is now possible to compile the     */
#*    Bee. This is the role of this entry.                             */
#*    Compile bee on an bootstrapped plateform                         */
#*---------------------------------------------------------------------*/
compile-bee0: 
	$(MAKE) -C bdl
	$(MAKE) -C cigloo
	if [ "$(JVMBACKEND) " = "yes " ]; then \
            $(MAKE) -C jigloo; \
        fi
	if [ "$(EMACSDIR) " != " " ]; then \
            $(MAKE) -C bmacs compile-bee; \
        fi

compile-bee1:
	$(MAKE) -C bdb
	$(MAKE) -C runtime compile-bee

compile-bee: compile-bee0
	@ if [ "$(INSTALLBEE)" = "full" ]; then \
            $(MAKE) compile-bee1; \
          fi

#*---------------------------------------------------------------------*/
#*    fullbootstrap ...                                                */
#*    -------------------------------------------------------------    */
#*    Bootstrap the compiler. This is a development entry point. It    */
#*    should be used only when testing a new unstable compiler.        */
#*---------------------------------------------------------------------*/
fullbootstrap:
	@ if [ "$(LOGMSG) " = " " ]; then \
            echo "Error, No MSG provided using standard revision message"; \
            echo "use \"make fullbootstrap LOGMSG=a-message\""; \
	    echo ""; \
            echo "To bootstrap without creating a revision use \"$(MAKE) fullbootstrap-sans-log\""; \
            echo "To bootstrap with an editable log \"$(MAKE) fullbootstrap-edit-log\""; \
            exit -1; \
          fi
	@ $(MAKE) fullbootstrap-sans-log
	@ $(MAKE) -s revision LOGMSG="$(LOGMSG) (bootstrap)"
	@ $(MAKE) ChangeLog

fullbootstrap-edit-log:
	@ $(MAKE) fullbootstrap-sans-log
	@ $(MAKE) -s revision

fullbootstrap-sans-log:
	(dt=`date '+%d%b%y'`; \
           $(RM) -f $(BOOTBINDIR)/bigloo.???????.gz > /dev/null 2>&1; \
           $(RM) -f $(BOOTBINDIR)/bigloo.????????.gz > /dev/null 2>&1; \
           $(RM) -f $(BOOTBINDIR)/bigloo.?????????.gz > /dev/null 2>&1; \
           cp $(BOOTBINDIR)/bigloo$(EXE_SUFFIX) $(BOOTBINDIR)/bigloo.$$dt$(EXE_SUFFIX); \
           $(GZIP) $(BOOTBINDIR)/bigloo.$$dt$(EXE_SUFFIX))
	@ ./configure --bootconfig $(CONFIGUREOPTS)
	if [ "$(GCCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gc clean; \
	  $(MAKE) -C gc boot; \
        fi
	if [ "$(GMPCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gmp clean; \
	  $(MAKE) -C gmp boot; \
        fi
	if [ "$(PCRECUSTOM)" = "yes" ]; then \
	  $(MAKE) -C pcre clean; \
	  $(MAKE) -C pcre boot; \
        fi
	if [ "$(UNISTRINGCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C libunistring clean; \
	  $(MAKE) -C libunistring boot; \
        fi
	if [ "$(LIBUVCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C libuv clean; \
	  $(MAKE) -C libuv boot; \
        fi
	$(MAKE) -C comptime -i touchall; $(MAKE) -C comptime EFLAGS+=-gself
	$(MAKE) -C runtime -i touchall; $(MAKE) -C runtime heap libs-c
	$(MAKE) -C comptime -i touchall; $(MAKE) -C comptime
	$(MAKE) -C comptime -i touchall; $(MAKE) -C comptime
	if [ "$(JVMBACKEND)" = "yes" ]; then \
	  $(MAKE) -C runtime heap-jvm libs-jvm; \
        fi
	$(MAKE) -C bde -i clean; $(MAKE) -C bde
	$(MAKE) -C api fullbootstrap
	$(MAKE) -C cigloo -i clean; $(MAKE) -C cigloo
	if [ "$(ENABLE_BGLPKG)" = "yes" ]; then \
	  $(MAKE) -C bglpkg -i clean; \
	  $(MAKE) -C bglpkg; \
	fi
	$(MAKE) -C recette -i touchall
	$(MAKE) -C recette && (cd recette && ./recette$(EXE_SUFFIX))
	if [ "$(JVMBACKEND)" = "yes" ]; then \
	  $(MAKE) -C recette jvm && (cd recette && ./recette-jvm$(SCRIPTEXTENSION)); \
        fi
	$(MAKE) -C recette clean
	@ echo "Bigloo full bootstrap done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    c-fullbootstrap ...                                              */
#*    -------------------------------------------------------------    */
#*    Bootstrap the compiler using the C backend. This is a            */
#*    development entry point. It should be used only when testing a   */
#*    new unstable compiler.                                           */
#*---------------------------------------------------------------------*/
c-fullbootstrap:
	(dt=`date '+%d%b%y'`; \
           $(RM) -f $(BOOTBINDIR)/bigloo.???????.gz > /dev/null 2>&1; \
           $(RM) -f $(BOOTBINDIR)/bigloo.????????.gz > /dev/null 2>&1; \
           $(RM) -f $(BOOTBINDIR)/bigloo.?????????.gz > /dev/null 2>&1; \
           cp $(BOOTBINDIR)/bigloo$(EXE_SUFFIX) $(BOOTBINDIR)/bigloo.$$dt$(EXE_SUFFIX); \
           $(GZIP) $(BOOTBINDIR)/bigloo.$$dt$(EXE_SUFFIX))
	@ ./configure --bootconfig $(CONFIGUREOPTS)
	@ (cd comptime && $(MAKE) -i touchall; $(MAKE))
	@ (cd runtime && $(MAKE) -i touchall; $(MAKE) heap libs-c gcs)
	@ (cd comptime && $(MAKE) -i touchall; $(MAKE))
	@ (cd comptime && $(MAKE) -i touchall; $(MAKE))
	@ if [ "$(BOOTCAPI)" = "yes" ]; then \
            (cd api && $(MAKE) -i clean && $(MAKE) boot-c); \
          fi
	@ (cd cigloo && $(MAKE) -i clean; $(MAKE))
	@ (cd bglpkg && $(MAKE) -i clean; $(MAKE))
	@ (cd recette && $(MAKE) -i touchall; \
           $(MAKE) recette && ./recette$(EXE_SUFFIX))
	@ $(MAKE) -s revision LOGMSG="C Full Bootstrap succeeded at `date '+%d%b%y'`"
	@ echo "Bigloo C full bootstrap done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    newrevision ...                                                  */
#*    -------------------------------------------------------------    */
#*    Change the development Bigloo version number.                    */
#*---------------------------------------------------------------------*/
newrevision:
	(cd runtime && $(MAKE) includes)
	(cd comptime && $(MAKE))
	(cd runtime && $(MAKE) touchall && $(MAKE) all)
	(cd comptime && $(MAKE) touchall && $(MAKE))
	@ $(MAKE) $(REVISIONSYSTEM)-branch

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    This entry build a distribution (biglooXXX.tar.gz file).         */
#*    This rule uses dependencies that cannot be satisfied by this     */
#*    current makefile. The dependencies are here present just to      */
#*    check that everything is ready for a distribution.               */
#*---------------------------------------------------------------------*/
.PHONY: distrib ChangeLog

include Makefile.$(REVISIONSYSTEM)

#*---------------------------------------------------------------------*/
#*    distrib ...                                                      */
#*---------------------------------------------------------------------*/
distrib: ChangeLog
	@ (cd $(DISTRIBTMPDIR) && \
	   $(RM) -rf bigloo-$(RELEASE) && $(RM) -rf bigloo && \
           $(MAKE) -I $(BOOTDIR) -f $(BOOTDIR)/Makefile checkout BUILDDIRNAME=$(BUILDDIRNAME) && \
           cd $(BUILDDIRNAME) && \
           cat $(BOOTDIR)/Makefile.config | sed 's/BFEATUREFLAGS=.*/BFEATUREFLAGS=-srfi enable-gmp/' | sed 's/BOOTFLAGS=.*/BOOTFLAGS=/' > Makefile.config \
           && cp $(BOOTDIR)/Makefile.buildconfig Makefile.buildconfig \
           && $(MAKE) true-distrib)
	@ $(RM) -rf $(DISTRIBTMPDIR)/bigloo-$(RELEASE)

true-distrib: $(DISTRIBDIR)/bigloo-$(RELEASE)$(VERSION).tar.gz

$(DISTRIBDIR)/bigloo-$(RELEASE)$(VERSION).tar.gz:
	@ $(RM) -f $(DISTRIBDIR)/bigloo-$(RELEASE)$(VERSION).tar.gz
	@ for p in $(NO_DIST_FILES); do \
             $(RM) -rf $$p; \
          done
	@ for d in $(DIRECTORIES); do \
             if [ -d $$d ]; then \
               echo "distribution $$d ..."; \
	       ($(MAKE) -C $$d distrib) || exit 1; \
             fi \
          done
	@ $(MAKE) -C $(BOOTDIR) log > $$PWD/ChangeLog
	@ $(RM) -f Makefile.config;
	@ $(RM) -f Makefile.buildconfig;
	@ (cd .. && \
           mv $(BUILDDIRNAME) bigloo-$(RELEASE)$(VERSION) && \
           tar cfz $(DISTRIBDIR)/bigloo-$(RELEASE)$(VERSION).tar.gz --sort=name bigloo-$(RELEASE)$(VERSION))
	@ echo "$@ done..."
	@ echo "-------------------------------"

ChangeLog:
	$(MAKE) log > $@

#*---------------------------------------------------------------------*/
#*    distrib-jvm                                                      */
#*---------------------------------------------------------------------*/
.PHONY: zip distrib-jvm

zip: distrib-jvm
distrib-jvm: 
	@ (cd $(DISTRIBTMPDIR) && \
	   $(RM) -rf bigloo && \
	   $(RM) -rf bigloo-$(RELEASE) && \
           $(RM) -rf bigloo && mkdir bigloo && cd bigloo && \
           $(MAKE) -I $(BOOTDIR) -f $(BOOTDIR)/Makefile checkout && \
           cd bigloo && \
           cp $(BOOTDIR)/Makefile.config Makefile.config && \
           $(MAKE) true-distrib-jvm)
	@ $(RM) -rf $(DISTRIBTMPDIR)/bigloo
	@ $(RM) -rf $(DISTRIBTMPDIR)/bigloo-$(RELEASE)

true-distrib-jvm: $(DISTRIBDIR)/bigloo-$(RELEASE).zip

$(DISTRIBDIR)/bigloo-$(RELEASE).zip: manual-pdf
	@ mkdir -p bin
	@ mkdir -p bigloo-$(RELEASE)/lib/-$(RELEASE)
	@ mkdir -p bigloo-$(RELEASE)/bin
	@ (ver=`echo -$(RELEASE) | sed -e 's/[.]//'` && \
           cat win32/install.bat | sed -e "s/THE-VERSION/$$ver/g" > bigloo-$(RELEASE)/install.bat && \
           cat win32/uninstall.bat | sed -e "s/THE-VERSION/$$ver/g" > bigloo-$(RELEASE)/uninstall.bat)
	@ (cp $(BOOTBINDIR)/bigloo bin/bigloo)
	@ (cp $(BOOTBINDIR)/bglafile bin/bglafile)
	@ (cp $(BOOTBINDIR)/bgljfile bin/bgljfile)
	@ (cp $(BOOTBINDIR)/bgltags bin/bgltags)
	@ (cp $(BOOTBINDIR)/bgldepend bin/bgldepend)
	@ (./configure --os-win32 \
                       --jvm-default-backend \
                       --prefix="$(JVMBASEDIR)" \
                       --libdir="$(JVMBASEDIR)"//lib \
                       --fildir="$(JVMBASEDIR)"//lib \
                       --zipdir="$(JVMBASEDIR)"//lib \
                       --javashell=msdos --java=java \
                       --javac=javac --jvm=force \
                       --a.bat=a.bat --a.out=a.exe)
	(cd runtime && $(MAKE) obj all-jvm)
	(cd api && $(MAKE) boot-jvm)
	@ $(MAKE) -f $(BOOTDIR)/Makefile true-comptime-jvm
	@ (cd bde && $(MAKE) jvm)
	@ (cd jigloo && $(MAKE) jvm)
	@ mkdir bigloo-$(RELEASE)/lib/BGL-TMP
	@ cp lib/-$(RELEASE)/*.jheap bigloo-$(RELEASE)/lib/BGL-TMP
	@ cp lib/-$(RELEASE)/*.zip bigloo-$(RELEASE)/lib/BGL-TMP
	@ mkdir bigloo-$(RELEASE)/lib/bigloo
	@ mv bigloo-$(RELEASE)/lib/BGL-TMP bigloo-$(RELEASE)/lib/bigloo/`echo -$(RELEASE) | sed -e 's/[.]//'`
	@ cp bin/bigloo.jar bigloo-$(RELEASE)/bin/bigloo.jar
	@ cat bin/bigloo.jvm | sed -e "s/\/tmp\/bigloo\/bin\//$(JVMBASEDIR)\\\\bin\\\\/" > bigloo-$(RELEASE)/bin/bigloo.bat
	@ cp bde/afile.class bigloo-$(RELEASE)/bin
	@ cp bde/jfile.class bigloo-$(RELEASE)/bin
	@ cp jigloo/jigloo.class bigloo-$(RELEASE)/bin
	@ cp INSTALL.jvm bigloo-$(RELEASE)/INSTALL
	@ cp doc/README.jvm bigloo-$(RELEASE)/README
	@ cp manuals/bigloo.pdf bigloo-$(RELEASE)/bigloo.pdf
	@ (mkdir bigloo-$(RELEASE)/demo; \
           mkdir bigloo-$(RELEASE)/demo/awt; \
           mkdir bigloo-$(RELEASE)/demo/maze; \
           cp examples/Jawt/README.jvm bigloo-$(RELEASE)/demo/awt/README; \
           cp examples/Jawt/Utils.java bigloo-$(RELEASE)/demo/awt/Utils.java; \
           cp examples/Jawt/awt.scm bigloo-$(RELEASE)/demo/awt/awt.scm; \
           cp examples/Maze/README.jvm bigloo-$(RELEASE)/demo/maze/README; \
           cp examples/Maze/maze.scm bigloo-$(RELEASE)/demo/maze/maze.scm)
	@ $(RM) -r bigloo-$(RELEASE)/lib/-$(RELEASE)
	@ mv bigloo-$(RELEASE) bgl`echo -$(RELEASE) | sed -e 's/[.]//'`
	@ $(RM) -f $(DISTRIBDIR)/bigloo`echo -$(RELEASE) | sed -e 's/[.]//'`.zip
	@ $(ZIP) -r $(DISTRIBDIR)/bigloo`echo -$(RELEASE) | sed -e 's/[.]//'`.zip bgl`echo -$(RELEASE) | sed -e 's/[.]//'`
	@ echo "$(DISTRIBDIR)/bigloo-$(RELEASE)$(VERSION).zip done..."
	@ echo "-------------------------------"

# This entry as to be isolated from the general bigloo_s.zip rule
# Because, it is mandatory here, not to use the Makefile.config of
# of the Bigloo used to bootstrap the distribution. We have to use
# the locally configured Bigloo
true-comptime-jvm:
	 (cd comptime && \
           $(MAKE) .afile .jfile jvm \
                   JARPATH=`echo '$(BINDIR)' | sed -e 's/\\\\/\\\\\\\\/g'`\
                   CLASSPATH=`echo '$(DESTDIR)$(LIBDIR)' | sed -e 's/\\\\/\\\\\\\\/g'`\
                   LIBDIR=`echo '$(BOOTLIBDIR)' | sed -e 's/\\\\/\\\\\\\\/g'`)

#*---------------------------------------------------------------------*/
#*    rpm                                                              */
#*---------------------------------------------------------------------*/
rpm: bigloo$(RELEASE).spec bigloo$(RELEASE).$(RPMARCH).rpm

bigloo$(RELEASE)$(VERSION).$(RPMARCH).rpm: $(RPMSOURCESDIR)/bigloo$(RELEASE)$(VERSION).tar.gz
	@ $(SUDO) rpm -bb --rmsource bigloo$(RELEASE).spec
	@ $(SUDO) cp $(RPMRPMDIR)/$(RPMARCH)/bigloo-$(RELEASE)-$(LIBCVERSION).$(RPMARCH).rpm $(DISTRIBDIR)
	@ $(SUDO) chown $$LOGNAME $(DISTRIBDIR)/bigloo-$(RELEASE)-$(LIBCVERSION).$(RPMARCH).rpm
	@ $(SUDO) $(RM) $(RPMRPMDIR)/$(RPMARCH)/bigloo-$(RELEASE)-$(LIBCVERSION).$(RPMARCH).rpm
	@ echo "Removing $(RPMBUILDDIR)/bigloo$(RELEASE)"
	@ $(SUDO) $(RM) -rf $(RPMBUILDDIR)/bigloo$(RELEASE)
	@ echo "Removing $(RPMSOURCEDIR)/bigloo$(RELEASE)$(VERSION).tar.gz"
	@ $(SUDO) $(RM) -rf $(RPMSOURCEDIR)/bigloo$(RELEASE)$(VERSION).tar.gz
	@ $(RM) -f bigloo$(RELEASE).spec

.PHONY: $(RPMSOURCESDIR)/bigloo$(RELEASE).tar.gz

$(RPMSOURCESDIR)/bigloo$(RELEASE)$(VERSION).tar.gz:
	@ echo "building bigloo.spec for libc version: $(LIBCVERSION)"
	@ echo "Copying bigloo.tar.gz into $(RPMSOURCESDIR)/bigloo$(RELEASE)$(VERSION).tar.gz"
	@ $(SUDO) cp $(DISTRIBDIR)/bigloo$(RELEASE)$(VERSION).tar.gz $(RPMSOURCESDIR)/bigloo$(RELEASE)$(VERSION).tar.gz

#*---------------------------------------------------------------------*/
#*    bigloo$(RELEASE).spec                                            */
#*---------------------------------------------------------------------*/
bigloo$(RELEASE).spec: bigloo.spec configure
	@ cat bigloo.spec | sed -e s/@RELEASE@/$(RELEASE)/g > bigloo$(RELEASE).spec

#*---------------------------------------------------------------------*/
#*    ftplibrary                                                       */
#*    -------------------------------------------------------------    */
#*    This entry creates and fill an library directory. That directory */
#*    must be installed in the ftp server with the name library.       */
#*---------------------------------------------------------------------*/
ftplibrary:
	@ $(RM) -rf library
	@ mkdir library
	@ for d in $(FTP_LIBRARIES); do \
              echo "copying $$d..."; \
              cp $$d library; \
          done
	@ chmod a+r -R library
	@ chmod a+x library

#*---------------------------------------------------------------------*/
#*    test                                                             */
#*---------------------------------------------------------------------*/
.PHONY: test c-test jvm-test c-api-test

test:
	@if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	   ($(MAKE) c-test); \
         fi
	@if [ "$(JVMBACKEND)" = "yes" ]; then \
	   ($(MAKE) jvm-test); \
         fi

fulltest:
	@if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	   ($(MAKE) c-fulltest); \
         fi
	@if [ "$(JVMBACKEND)" = "yes" ]; then \
	   ($(MAKE) jvm-test); \
         fi

c-test: 
	(cd recette && \
         $(MAKE) recette-static && \
         $(BGLBUILDBINDIR)/bglrun.sh ./recette-static $(RECETTEFLAGS))

c-fulltest: 
	(cd recette && \
         $(MAKE) recette-static && \
         $(BGLBUILDBINDIR)/bglrun.sh ./recette-static $(RECETTEFLAGS))
	if [ "$(SHAREDCOMPILER) " = "no " -o "$(HOSTOS) " = "linux " ]; then \
           $(MAKE) c-api-test; \
         fi

c-api-test:
	for p in $(APIS); do \
          if [ -d api/$$p/recette ]; then \
            echo "*** $$p ********** "; \
            (cd api/$$p/recette && \
             $(MAKE) c && \
	     test -x ./recette && \
             $(BGLBUILDBINDIR)/bglrun.sh ./recette $(RECETTEFLAGS)) || exit 1; \
          fi; \
        done

jvm-test:
	(cd recette && \
         $(MAKE) EFLAGS="-jvm-bigloo-classpath $(BOOTLIBDIR)" jvm && \
         ./recette-jvm$(SCRIPTEXTENSION) $(RECETTEFLAGS)); \
         for p in $(APIS); do \
           if [ -d api/$$p/recette ]; then \
             echo "*** $$p ********** "; \
             (cd api/$$p/recette && \
              $(MAKE) EFLAGS="-jvm-bigloo-classpath $(BOOTLIBDIR)" jvm && \
	      test -x ./recette-jvm$(SCRIPTEXTENSION) && \
              ./recette-jvm$(SCRIPTEXTENSION) $(RECETTEFLAGS)) \
           fi; \
        done

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
.PHONY: install install-progs install-devel install-libs install-apis

.PHONY: uninstall

install: install-sans-docs install-docs

install-sans-docs: install-progs install-apis

install-progs: install-devel install-libs

install-devel: install-dirs
	$(MAKE) -C comptime install
	$(MAKE) -C bde install
	if [ "$(ENABLE_BGLPKG)" = "yes" ]; then \
	  $(MAKE) -C bglpkg install; \
	fi
	$(MAKE) -C autoconf install
	$(MAKE) -C api install-devel

install-libs: install-dirs
	$(MAKE) -C runtime install
	if [ "$(GCCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gc install; \
        fi
	if [ "$(GMPCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gmp install; \
        fi
	if [ "$(PCRECUSTOM)" = "yes" ]; then \
	  $(MAKE) -C pcre install; \
        fi
	if [ "$(UNISTRINGCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C libunistring install; \
        fi
	if [ "$(LIBUVCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C libuv install; \
        fi
	(cp Makefile.config $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.config && \
         chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.config)
	(if [ $(BOOTLIBDIR) != $(DESTDIR)$(LIBDIR)/$(FILDIR) ]; then \
           cp $(BOOTLIBDIR)/bigloo_config.sch $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo_config.sch && \
           chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bigloo_config.sch; \
         fi)
	(cp Makefile.misc $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.misc && \
         chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/Makefile.misc)

install-apis: install-dirs
	$(MAKE) -C api install

install-api: install-dirs
	for p in $(APIS); do \
	  if [ "$$p " = "$(API) " ]; then \
             $(MAKE) -C api install APIS=$(API); \
          fi \
	done

install-docs: install-dirs
	$(MAKE) -C manuals install

install-bee0: install-dirs
	$(MAKE) -C cigloo install
	$(MAKE) -C bdl install
	@ if [ "$(JVMBACKEND) " = "yes " ]; then \
            $(MAKE) -C jigloo install; \
          fi
	-$(MAKE) -C bmacs install

install-bee1: install-dirs
	$(MAKE) -C bdb install
	$(MAKE) -C runtime install-bee

install-bee: install-bee0
	@ if [ "$(INSTALLBEE)" = "full" ]; then \
            $(MAKE) install-bee1; \
          fi

install-dirs:
	if [ ! -d $(DESTDIR)$(BINDIR) ]; then \
	   mkdir -p $(DESTDIR)$(BINDIR) && \
             chmod $(MODDIR) $(DESTDIR)$(BINDIR) || exit 1; \
        fi;
	(base=`echo $(DESTDIR)$(LIBDIR)/$(FILDIR) | sed 's/[/][^/]*$$//'`; \
         bbase=`echo $$base | sed 's/[/][^/]*$$//'`; \
         if [ ! -d $(DESTDIR)$(LIBDIR) ]; then \
            mkdir -p $(DESTDIR)$(LIBDIR) && chmod $(MODDIR) $(DESTDIR)$(LIBDIR); \
         fi && \
         if [ ! -d $(DESTDIR)$(LIBDIR)/pkgconfig ]; then \
            mkdir -p $(DESTDIR)$(LIBDIR)/pkgconfig && chmod $(MODDIR) $(DESTDIR)$(LIBDIR)/pkgconfig; \
         fi && \
         if [ ! -d $$bbase ]; then \
            mkdir -p $$bbase && chmod $(MODDIR) $$bbase; \
         fi && \
         if [ ! -d $$base ]; then \
            mkdir -p $$base && chmod $(MODDIR) $$base; \
         fi)
	if [ ! -d $(DESTDIR)$(LIBDIR)/$(FILDIR) ]; then \
          mkdir -p $(DESTDIR)$(LIBDIR)/$(FILDIR) && chmod $(MODDIR) $(DESTDIR)$(LIBDIR)/$(FILDIR); \
        fi
	if [ ! -d $(DOCDIR) ]; then \
	  mkdir -p $(DOCDIR) && chmod $(MODDIR) $(DOCDIR); \
        fi
	if [ ! -d $(MANDIR) ]; then \
	  mkdir -p $(MANDIR) && chmod $(MODDIR) $(MANDIR); \
        fi
	if [ ! -d $(INFODIR) ]; then \
	  mkdir -p $(INFODIR) && chmod $(MODDIR) $(INFODIR); \
        fi

uninstall: uninstall-bee
	$(MAKE) -C autoconf uninstall
	$(MAKE) -C bde uninstall
	$(MAKE) -C comptime uninstall
	if [ "$(GCCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gc uninstall uninstall-thread; \
        fi
	if [ "$(GMPCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C gmp uninstall; \
        fi
	if [ "$(PCRECUSTOM)" = "yes" ]; then \
	  $(MAKE) -C pcre uninstall; \
        fi
	if [ "$(UNISTRINGCUSTOM)" = "yes" ]; then \
	  $(MAKE) -C libunistring uninstall; \
        fi
	$(MAKE) -C runtime uninstall
	-$(MAKE) -C manuals uninstall
	$(MAKE) -C api uninstall
	$(RM) -f $(DESTDIR)$(LIBDIR)/Makefile.config
	$(RM) -f $(DESTDIR)$(LIBDIR)/Makefile.misc
	$(MAKE) -C bglpkg uninstall
	$(MAKE) -C api uninstall-devel

uninstall-bee0:
	$(MAKE) -C cigloo uninstall
	$(MAKE) -C jigloo uninstall
	(if [ -d bdb ]; then $(MAKE) -C bdb uninstall; fi)
	$(MAKE) -C runtime uninstall-bee
	-$(MAKE) -C manuals uninstall-bee
	-$(MAKE) -C bmacs uninstall

uninstall-bee: uninstall-bee0

#*---------------------------------------------------------------------*/
#*    unconfigure                                                      */
#*---------------------------------------------------------------------*/
.PHONY: unconfigure

unconfigure: 
	@ if [ "`pwd`" = "$$HOME/prgm/project/bigloo" ]; then \
             echo "*** ERROR:Illegal dir to make an unconfigure `pwd`"; \
             exit 1; \
          fi
	$(RM) -f lib/$(RELEASE)/bigloo_config.h
	$(RM) -f lib/$(RELEASE)/bigloo_config.sch
	$(RM) -f Makefile.config
	$(RM) -f configure.log

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean 

clean:
	@ if [ "`pwd`" = "$$HOME/prgm/project/bigloo" ]; then \
             echo "*** ERROR:Illegal dir to make a clean `pwd`"; \
             exit 1; \
          fi
	$(RM) -f configure.log
	$(RM) -f ChangeLog
	$(MAKE) -C gc clean
	$(MAKE) -C gmp clean
	$(MAKE) -C pcre clean
	$(MAKE) -C libunistring clean
	(cd comptime && $(MAKE) clean)
	(cd runtime && $(MAKE) clean)
	(cd manuals && $(MAKE) clean)
	(cd cigloo && $(MAKE) clean)
	(cd jigloo && $(MAKE) clean)
	(cd bmacs && $(MAKE) clean)
	(cd bde && $(MAKE) clean)
	if [ -d bdb ]; then \
	   (cd bdb && $(MAKE) clean); \
	fi
	(cd recette && $(MAKE) clean)
	(cd api && $(MAKE) clean)
	(cd bdl && $(MAKE) clean)
	(cd bglpkg && $(MAKE) clean)

cleanall: 
	@ if [ "`pwd`" = "$$HOME/prgm/project/bigloo" ]; then \
             echo "*** ERROR:Illegal dir to make a cleanall `pwd`"; \
             exit 1; \
          fi
	$(RM) -f configure.log
	$(RM) -f autoconf/runtest
	$(MAKE) -C gc cleanall
	$(MAKE) -C gmp cleanall
	$(MAKE) -C pcre cleanall
	$(MAKE) -C libunistring cleanall
	(cd comptime && $(MAKE) cleanall)
	(cd runtime && $(MAKE) cleanall)
	(cd manuals && $(MAKE) cleanall)
	(cd cigloo && $(MAKE) cleanall)
	(cd jigloo && $(MAKE) cleanall)
	(cd bmacs && $(MAKE) cleanall)
	(cd bde && $(MAKE) cleanall)
	if [ -d bdb ]; then \
	   (cd bdb && $(MAKE) cleanall); \
        fi
	(cd api && $(MAKE) cleanall)
	(cd bdl && $(MAKE) cleanall)
	(cd bglpkg && $(MAKE) cleanall)

distclean: 
	@ if [ "`pwd`" = "$$HOME/prgm/project/bigloo" ]; then \
             echo "*** ERROR:Illegal dir to make a distclean `pwd`"; \
             exit 1; \
          fi
	if [ -f Makefile.config ]; then \
	  touch configure.log; $(RM) configure.log; \
	  (cd comptime && $(MAKE) distclean); \
	  (cd runtime && $(MAKE) distclean); \
	  (cd manuals && $(MAKE) distclean); \
	  (cd cigloo && $(MAKE) distclean); \
	  (cd jigloo && $(MAKE) distclean); \
	  (cd bmacs && $(MAKE) distclean); \
	  (cd bde && $(MAKE) distclean); \
	  if [ -d bdb ]; then \
	     (cd bdb && $(MAKE) distclean); \
	  fi; \
	  (cd api && $(MAKE) distclean); \
	  (cd bdl && $(MAKE) distclean); \
	  (cd bglpkg && $(MAKE) distclean); \
	  $(MAKE) unconfigure; \
	  $(RM) -rf bin; \
	  $(RM) -rf lib; \
        fi

#*---------------------------------------------------------------------*/
#*    population                                                       */
#*    -------------------------------------------------------------    */
#*    The list of all files that have to be placed inside the          */
#*    repository for revision.                                         */
#*---------------------------------------------------------------------*/
pop:
	@ echo LICENSE COPYING \
               configure INSTALL.md INSTALL.jvm README.md \
               Makefile Makefile.misc \
               Makefile.mercurial \
               Makefile.git \
               .hgignore \
               .gitignore \
               tutorial
	@ for d in $(DIRECTORIES); do \
             (cd $$d && $(MAKE) -s pop); \
          done

popfilelist:
	@ (for p in `$(MAKE) -s pop`; do \
            echo $$p; \
           done) | sort

checkpop:
	@ for f in `$(MAKE) -s popfilelist`; do \
	    if [ ! -e $$f ]; then \
	      echo "Missing file: " $$f; \
	    fi; \
	  done

#*---------------------------------------------------------------------*/
#*    checkgmake                                                       */
#*---------------------------------------------------------------------*/
.PHONY: checkgmake

checkgmake:
	@ autoconf/gmaketest --make=$(MAKE) || \
          (echo "GNU-Make is required to install Bigloo. Aborting."; exit 1)

#*---------------------------------------------------------------------*/
#*    revision                                                         */
#*    -------------------------------------------------------------    */
#*    Generic revision entry point.                                    */
#*---------------------------------------------------------------------*/
.PHONY: branch revision checkout populate revision-pop push

