#*=====================================================================*/
#*    Author      :  Manuel Serrano                                    */
#*        adapted :  Florian Loitsch                                   */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for the CRYPTO library                              */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src recette

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
	@ echo $(POPULATION:%=crypto/%)
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
	$(RM) -f lib/*crypto* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/crypto.init $(LIBDIR)/$(FILDIR)/crypto.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/crypto.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/crypto.heap $(LIBDIR)/$(FILDIR)/crypto.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/crypto.heap; \
        fi
	$(MAKE) install-lib LIB=libbigloocrypto_s-$(RELEASE);
	$(MAKE) install-shared-lib LIB=libbigloocrypto_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbigloocrypto_s-$(RELEASE) LN=libbigloocrypto_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/crypto.jheap $(LIBDIR)/$(FILDIR)/crypto.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/crypto.jheap && \
	  cp $(BOOTLIBDIR)/bigloocrypto_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloocrypto_s-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocrypto_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloocrypto_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloocrypto_e-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocrypto_e-$(RELEASE).zip && \
	  (cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloocrypto_u-$(RELEASE).zip && \
           $(LN_S) bigloocrypto_s-$(RELEASE).zip bigloocrypto_u-$(RELEASE).zip); \
        fi

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/crypto.jheap $(LIBDIR)/$(FILDIR)/crypto.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/crypto.jheap && \
	  cp $(BOOTLIBDIR)/bigloocrypto_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloocrypto_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocrypto_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloocrypto_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloocrypto_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocrypto_u-$(RELEASE).dll ; \
	  cp $(BOOTLIBDIR)/bigloocrypto_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloocrypto_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocrypto_e-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/crypto.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/crypto.heap
	-$(MAKE) uninstall-lib LIB=libbigloocrypto_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloocrypto_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloocrypto_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/crypto.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocrypto_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocrypto_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocrypto_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocrypto_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocrypto_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocrypto_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
