#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/api/ssl/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Tue Mar 24 12:21:46 2020 (serrano)                */
#*    Copyright   :  2001-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for ssl                                             */
#*=====================================================================*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    The name of the API (used by Makefile.api).                      */
#*---------------------------------------------------------------------*/
API = ssl

SSLZIP = lib/commons-ssl.zip

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR = src
POPULATION = Makefile README $(SSLZIP)

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build build build-c build-jvm

all: build
boot: build

build:
	@ $(MAKE) build-c && $(MAKE) build-jvm

c: build-c
jvm: build-jvm

build-c:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
            if [ "$(OPENSSLIBS)" != "no" ]; then \
	      echo "[0m[1;32m>>> C[0m"; \
	      (cd src && $(MAKE) build-c); \
            fi \
          fi

build-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m"; \
	    (cd src && $(MAKE) build-jvm); \
          fi

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=$(API)/%)
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
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install install-c install-jvm uninstall

install: api-install-init
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
            $(MAKE) install-jvm; \
          fi

install-c:
	if [ "$(OPENSSLIBS)" != "no" ]; then \
	  $(MAKE) api-install-c; \
        fi

install-jvm: api-install-jvm
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/commons-ssl.zip $(DESTDIR)$(LIBDIR)/$(FILDIR) && \
	  chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/commons-ssl.zip; \
        fi

uninstall: api-uninstall-init \
           api-uninstall-c \
           api-uninstall-jvm
	-$(RM) -f $(DESTDIR)$(LIBDIR)/$(FILDIR)/commons-ssl.zip

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)

#*---------------------------------------------------------------------*/
#*    Common rules                                                     */
#*---------------------------------------------------------------------*/
include ../../Makefile.misc
include ../Makefile.api

