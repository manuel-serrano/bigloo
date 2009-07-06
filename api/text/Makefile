#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/text/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Tue Feb 24 13:02:18 2009 (serrano)                */
#*    Copyright   :  2001-09 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for Text                                            */
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
	@ echo $(POPULATION:%=text/%)
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
	$(RM) -f lib/*text* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/text.init $(LIBDIR)/$(FILDIR)/text.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/text.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/text.heap $(LIBDIR)/$(FILDIR)/text.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/text.heap; \
        fi
	$(MAKE) install-lib LIB=libbiglootext_s-$(RELEASE);
	$(MAKE) install-shared-lib LIB=libbiglootext_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbiglootext_s-$(RELEASE) LN=libbiglootext_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/text.jheap $(LIBDIR)/$(FILDIR)/text.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/text.jheap && \
	  cp $(BOOTLIBDIR)/biglootext_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglootext_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/biglootext_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglootext_e-$(RELEASE).zip; \
        fi
	(cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f biglootext_u-$(RELEASE).zip && \
           $(LN_S) biglootext_s-$(RELEASE).zip biglootext_u-$(RELEASE).zip)

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/text.jheap $(LIBDIR)/$(FILDIR)/text.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/text.jheap && \
	  cp $(BOOTLIBDIR)/biglootext_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/biglootext_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglootext_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/biglootext_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/biglootext_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglootext_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/biglootext_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/biglootext_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/biglootext_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/text.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/text.heap
	-$(MAKE) uninstall-lib LIB=libbiglootext_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbiglootext_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbiglootext_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/text.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglootext_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglootext_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglootext_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglootext_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglootext_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/biglootext_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
