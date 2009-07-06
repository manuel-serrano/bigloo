#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/srfi27/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Mon Apr 20 10:38:52 2009 (serrano)                */
#*    Copyright   :  2001-09 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for Srfi27                                          */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src recette

POPULATION	= Makefile README recette/Makefile recette/recette.scm

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
	@ echo $(POPULATION:%=srfi27/%)
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
	$(RM) -f lib/*srfi27* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/srfi27.init $(LIBDIR)/$(FILDIR)/srfi27.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/srfi27.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/srfi27.heap $(LIBDIR)/$(FILDIR)/srfi27.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/srfi27.heap; \
        fi
	$(MAKE) install-lib LIB=libbigloosrfi27_s-$(RELEASE);
	$(MAKE) install-shared-lib LIB=libbigloosrfi27_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbigloosrfi27_s-$(RELEASE) LN=libbigloosrfi27_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/srfi27.jheap $(LIBDIR)/$(FILDIR)/srfi27.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/srfi27.jheap && \
	  cp $(BOOTLIBDIR)/bigloosrfi27_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosrfi27_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloosrfi27_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosrfi27_e-$(RELEASE).zip; \
        fi
	(cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloosrfi27_u-$(RELEASE).zip && \
           $(LN_S) bigloosrfi27_s-$(RELEASE).zip bigloosrfi27_u-$(RELEASE).zip)

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/srfi27.jheap $(LIBDIR)/$(FILDIR)/srfi27.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/srfi27.jheap && \
	  cp $(BOOTLIBDIR)/bigloosrfi27_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloosrfi27_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosrfi27_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloosrfi27_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloosrfi27_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosrfi27_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloosrfi27_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloosrfi27_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosrfi27_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/srfi27.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/srfi27.heap
	-$(MAKE) uninstall-lib LIB=libbigloosrfi27_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloosrfi27_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloosrfi27_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/srfi27.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosrfi27_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosrfi27_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosrfi27_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosrfi27_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosrfi27_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosrfi27_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
