#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/text/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Wed Nov 16 11:07:38 2011 (serrano)                */
#*    Copyright   :  2001-11 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for Text                                            */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    The name of the API (used by Makefile.api).                      */
#*---------------------------------------------------------------------*/
API = text

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR = src
POPULATION = Makefile README \
  data/gb2312.sch data/en-hyphens.sch data/fr-hyphens.sch

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: boot build

boot: build
build:
	@ $(MAKE) build-c && $(MAKE) build-jvm

c: build-c
jvm: build-jvm

build-c:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
            echo "[0m[1;32m>>> C[0m"; \
	    (cd src && $(MAKE) build-c); \
          fi

build-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m";  \
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
	$(RM) -f lib/*calendar* >/dev/null 2>&1
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install text-install-data text-uninstall-data

install: api-install-init text-install-data
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) api-install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
            $(MAKE) api-install-jvm; \
          fi

text-install-data:
	if [ ! -d $(LIBDIR)/$(FILDIR)/text ]; then \
	   mkdir -p $(LIBDIR)/$(FILDIR)/text \
             && chmod $(MODDIR) $(LIBDIR)/$(FILDIR)/text \
             && mkdir -p $(LIBDIR)/$(FILDIR)/text/data \
             && chmod $(MODDIR) $(LIBDIR)/$(FILDIR)/text/data; \
	elif [ ! -d $(LIBDIR)/text/data ]; then \
          mkdir -p $(LIBDIR)/$(FILDIR)/text/data \
             && chmod $(MODDIR) $(LIBDIR)/$(FILDIR)/text/data; \
        fi
	for p in data/*; do \
          cp $$p $(LIBDIR)/$(FILDIR)/text/data \
            && chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/text/$$p; \
        done

uninstall: api-uninstall-init \
           api-uninstall-c \
           api-uninstall-jvm \
           text-uninstall-data

text-uninstall-data:
	-$(RM) -rf $(LIBDIR)/$(FILDIR)/text

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
