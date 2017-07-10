#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api-contribs/jabber/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jul  9 09:58:01 2017 (serrano)                */
#*    Copyright   :  2001-17 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for CALENDAR                                        */
#*=====================================================================*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src

POPULATION	= Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: boot-c boot-jvm boot-dotnet
boot: boot-c boot-jvm boot-dotnet
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
	@ echo $(POPULATION:%=jabber/%)
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
	$(RM) -f lib/*jabber* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/jabber.init $(LIBDIR)/$(FILDIR)/jabber.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/jabber.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/jabber.heap $(LIBDIR)/$(FILDIR)/jabber.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/jabber.heap; \
        fi
	$(MAKE) install-lib LIB=libbigloojabber_s-$(RELEASE);
	$(MAKE) install-shared-lib LIB=libbigloojabber_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbigloojabber_s-$(RELEASE) LN=libbigloojabber_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/jabber.jheap $(LIBDIR)/$(FILDIR)/jabber.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/jabber.jheap && \
	  cp $(BOOTLIBDIR)/bigloojabber_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloojabber_s-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloojabber_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloojabber_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloojabber_e-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloojabber_e-$(RELEASE).zip && \
	  (cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloojabber_u-$(RELEASE).zip && \
           ln bigloojabber_s-$(RELEASE).zip bigloojabber_u-$(RELEASE).zip); \
        fi

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/jabber.jheap $(LIBDIR)/$(FILDIR)/jabber.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/jabber.jheap && \
	  cp $(BOOTLIBDIR)/bigloojabber_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloojabber_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloojabber_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloojabber_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloojabber_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloojabber_e-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloojabber_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloojabber_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloojabber_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/jabber.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/jabber.heap
	-$(MAKE) uninstall-lib LIB=libbigloojabber_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloojabber_e-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloojabber_u-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/jabber.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloojabber_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloojabber_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloojabber_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloojabber_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloojabber_e-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloojabber_u-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
