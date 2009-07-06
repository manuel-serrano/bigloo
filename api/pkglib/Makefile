#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/pkglib/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jul 15 14:43:53 2007 (serrano)                */
#*    Copyright   :  2001-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for ScmPkg repository library.                      */
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
            $(MAKE) $(BOOTLIBDIR)/pkglib.init && \
            (cd src && $(MAKE) c); \
          fi

boot-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m"; \
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
	@ echo $(POPULATION:%=pkglib/%)
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
	$(RM) -f lib/*pkglib* >/dev/null 2>&1
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install install-c install-jvm uninstall

include ../../Makefile.misc

install: 
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
            $(MAKE) install-jvm; \
          fi
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
            $(MAKE) install-dotnet; \
          fi

$(BOOTLIBDIR)/pkglib.init: src/Misc/pkglib.init
	cp $< $@ && chmod $(BMASK) $@

install-init:
	cp src/Misc/pkglib.init $(LIBDIR)/$(FILDIR)/pkglib.init && \
	   chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pkglib.init; \

install-c:
	if [ "$(SQLITELIBS)" != "no" ]; then \
	  if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	    cp $(BOOTLIBDIR)/pkglib.heap $(LIBDIR)/$(FILDIR)/pkglib.heap \
               && chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pkglib.heap; \
          fi; \
	  $(MAKE) install-lib LIB=libbigloopkglib_s-$(RELEASE); \
	  $(MAKE) install-shared-lib LIB=libbigloopkglib_e-$(RELEASE); \
	  $(MAKE) install-lnlib LIB=libbigloopkglib_s-$(RELEASE) LN=libbigloopkglib_u-$(RELEASE); \
        fi

install-jvm:

install-dotnet:

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pkglib.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pkglib.heap
	-$(MAKE) uninstall-lib LIB=libbigloopkglib_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloopkglib_e-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloopkglib_u-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pkglib.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkglib_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkglib_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkglib_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkglib_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkglib_e-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopkglib_u-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
