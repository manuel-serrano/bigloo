#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/web/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jul 15 14:43:33 2007 (serrano)                */
#*    Copyright   :  2001-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for WEB                                             */
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
	@ echo $(POPULATION:%=web/%)
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
	$(RM) -f lib/*web* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/web.init $(LIBDIR)/$(FILDIR)/web.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/web.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/web.heap $(LIBDIR)/$(FILDIR)/web.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/web.heap; \
        fi
	$(MAKE) install-lib LIB=libbiglooweb_s-$(RELEASE);
	$(MAKE) install-lib LIB=libbiglooweb_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbiglooweb_s-$(RELEASE) LN=libbiglooweb_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/web.jheap $(LIBDIR)/$(FILDIR)/web.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/web.jheap && \
	  cp $(BOOTLIBDIR)/biglooweb_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/biglooweb_s-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglooweb_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/biglooweb_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/biglooweb_e-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglooweb_e-$(RELEASE).zip && \
	  (cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f biglooweb_u-$(RELEASE).zip && \
           $(LN_S) biglooweb_s-$(RELEASE).zip biglooweb_u-$(RELEASE).zip); \
        fi

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/web.jheap $(LIBDIR)/$(FILDIR)/web.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/web.jheap && \
	  cp $(BOOTLIBDIR)/biglooweb_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/biglooweb_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglooweb_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/biglooweb_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/biglooweb_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglooweb_e-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/biglooweb_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/biglooweb_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglooweb_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/web.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/web.heap
	-$(MAKE) uninstall-lib LIB=libbiglooweb_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbiglooweb_e-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbiglooweb_u-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/web.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglooweb_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglooweb_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglooweb_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglooweb_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglooweb_e-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglooweb_u-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
