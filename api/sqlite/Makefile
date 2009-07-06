#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/sqlite/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Mon Apr 20 08:27:27 2009 (serrano)                */
#*    Copyright   :  2001-09 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for sqlite                                          */
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
	@ echo $(POPULATION:%=sqlite/%)
	@ for d in $(POPDIR); do \
            (cd $$d && $(MAKE) -s pop) \
          done;

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	(cd src && $(MAKE) clean)
	(cd recette && $(MAKE) clean)

distclean: clean
	(cd src && $(MAKE) distclean)
	(cd recette && $(MAKE) distclean)

cleanall: distclean
	(cd src && $(MAKE) cleanall)
	$(RM) -f lib/*sqlite* >/dev/null 2>&1
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

install-init:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/sqlite.init $(LIBDIR)/$(FILDIR)/sqlite.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/sqlite.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/sqlite.heap $(LIBDIR)/$(FILDIR)/sqlite.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/sqlite.heap; \
        fi; \
	$(MAKE) install-lib LIB=libbigloosqlite_s-$(RELEASE); \
	$(MAKE) install-shared-lib LIB=libbigloosqlite_e-$(RELEASE); \
	$(MAKE) install-lnlib LIB=libbigloosqlite_s-$(RELEASE) LN=libbigloosqlite_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/sqlite.jheap $(LIBDIR)/$(FILDIR)/sqlite.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/sqlite.jheap && \
	  cp $(BOOTLIBDIR)/bigloosqlite_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosqlite_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloosqlite_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosqlite_e-$(RELEASE).zip; \
        fi
	(cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloosqlite_u-$(RELEASE).zip && \
           $(LN_S) bigloosqlite_s-$(RELEASE).zip bigloosqlite_u-$(RELEASE).zip)

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/sqlite.jheap $(LIBDIR)/$(FILDIR)/sqlite.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/sqlite.jheap && \
	  cp $(BOOTLIBDIR)/bigloosqlite_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloosqlite_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosqlite_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloosqlite_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloosqlite_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosqlite_e-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloosqlite_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloosqlite_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloosqlite_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/sqlite.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/sqlite.heap
	-$(MAKE) uninstall-lib LIB=libbigloosqlite_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloosqlite_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloosqlite_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/sqlite.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosqlite_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosqlite_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosqlite_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosqlite_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosqlite_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloosqlite_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
