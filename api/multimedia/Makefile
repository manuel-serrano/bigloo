#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/multimedia/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Feb  3 17:17:27 2008 (serrano)                */
#*    Copyright   :  2001-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for the MULTIMEDIA library                          */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src examples

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
	@ echo $(POPULATION:%=multimedia/%)
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
	$(RM) -f lib/*multimedia* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/multimedia.init $(LIBDIR)/$(FILDIR)/multimedia.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/multimedia.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/multimedia.heap $(LIBDIR)/$(FILDIR)/multimedia.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/multimedia.heap; \
        fi
	$(MAKE) install-lib LIB=libbigloomultimedia_s-$(RELEASE);
	$(MAKE) install-lib LIB=libbigloomultimedia_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbigloomultimedia_s-$(RELEASE) LN=libbigloomultimedia_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/multimedia.jheap $(LIBDIR)/$(FILDIR)/multimedia.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/multimedia.jheap && \
	  cp $(BOOTLIBDIR)/bigloomultimedia_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloomultimedia_s-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomultimedia_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloomultimedia_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloomultimedia_e-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomultimedia_e-$(RELEASE).zip && \
	  (cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloomultimedia_u-$(RELEASE).zip && \
           $(LN_S) bigloomultimedia_s-$(RELEASE).zip bigloomultimedia_u-$(RELEASE).zip); \
        fi

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/multimedia.jheap $(LIBDIR)/$(FILDIR)/multimedia.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/multimedia.jheap && \
	  cp $(BOOTLIBDIR)/bigloomultimedia_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloomultimedia_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomultimedia_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloomultimedia_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloomultimedia_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomultimedia_u-$(RELEASE).dll ; \
	  cp $(BOOTLIBDIR)/bigloomultimedia_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloomultimedia_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomultimedia_e-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/multimedia.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/multimedia.heap
	-$(MAKE) uninstall-lib LIB=libbigloomultimedia_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloomultimedia_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloomultimedia_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/multimedia.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomultimedia_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomultimedia_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomultimedia_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomultimedia_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomultimedia_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomultimedia_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
