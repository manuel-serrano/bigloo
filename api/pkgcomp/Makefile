#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/pkgcomp/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jul 15 14:43:10 2007 (serrano)                */
#*    Copyright   :  2001-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for PKGCOMP                                         */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src

POPULATION	= Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: boot
boot:
	@ $(MAKE) boot-c && \
          $(MAKE) boot-jvm && \
          $(MAKE) boot-dotnet
jvm: boot-jvm
dotnet: boot-dotnet

boot-c:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
            echo "[0m[1;32m>>> C[0m"; \
	    (cd src && $(MAKE) c); \
          fi

boot-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m";  \
	    (cd src && $(MAKE) jvm); \
          fi

boot-dotnet:
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> .NET[0m"; \
	    (cd src && $(MAKE) dotnet); \
          fi

#*---------------------------------------------------------------------*/
#*    ude                                                              */
#*---------------------------------------------------------------------*/
ude:
	(cd src && $(MAKE) ude)

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=pkgcomp/%)
	@ for d in $(POPDIR); do \
            (cd $$d && $(MAKE) -s pop) \
          done;

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	(cd src && $(MAKE) clean)

distclean: clean
	(cd src && $(MAKE) distclean)

cleanall: distclean
	(cd src && $(MAKE) cleanall)
	$(RM) -f lib/*pkgcomp* >/dev/null 2>&1
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install install-c install-jvm uninstall

include ../../Makefile.misc

install: install-init
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
            $(MAKE) install-jvm; \
          fi
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
            $(MAKE) install-dotnet; \
          fi

install-init:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/pkgcomp.init $(LIBDIR)/$(FILDIR)/pkgcomp.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pkgcomp.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/pkgcomp.heap $(LIBDIR)/$(FILDIR)/pkgcomp.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pkgcomp.heap; \
        fi
	$(MAKE) install-lib LIB=libbigloopkgcomp_s-$(RELEASE);
	$(MAKE) install-shared-lib LIB=libbigloopkgcomp_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbigloopkgcomp_s-$(RELEASE) LN=libbigloopkgcomp_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/pkgcomp.jheap $(LIBDIR)/$(FILDIR)/pkgcomp.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pkgcomp.jheap && \
	  cp $(BOOTLIBDIR)/bigloopkgcomp_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloopkgcomp_s-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopkgcomp_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloopkgcomp_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloopkgcomp_e-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopkgcomp_e-$(RELEASE).zip && \
	  (cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloopkgcomp_u-$(RELEASE).zip && \
           $(LN_S) bigloopkgcomp_s-$(RELEASE).zip bigloopkgcomp_u-$(RELEASE).zip); \
        fi

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/pkgcomp.jheap $(LIBDIR)/$(FILDIR)/pkgcomp.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pkgcomp.jheap && \
	  cp $(BOOTLIBDIR)/bigloopkgcomp_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloopkgcomp_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopkgcomp_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloopkgcomp_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloopkgcomp_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopkgcomp_e-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloopkgcomp_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloopkgcomp_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopkgcomp_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pkgcomp.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pkgcomp.heap
	-$(MAKE) uninstall-lib LIB=libbigloopkgcomp_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloopkgcomp_e-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloopkgcomp_u-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pkgcomp.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkgcomp_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkgcomp_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkgcomp_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkgcomp_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkgcomp_e-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkgcomp_u-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
