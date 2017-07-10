#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/dbus/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jul  9 10:07:47 2017 (serrano)                */
#*    Copyright   :  2001-17 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for dbus                                            */
#*=====================================================================*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    The name of the API (used by Makefile.api).                      */
#*---------------------------------------------------------------------*/
API = dbus

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR = src

POPULATION = Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build build build-c

all: build
boot: build

build:
	@ $(MAKE) build-c

c: build-c

build-c:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
            if [ "$(OPENSSLIBS)" != "no" ]; then \
	      echo "[0m[1;32m>>> C[0m"; \
	      (cd src && $(MAKE) build-c); \
            fi \
          fi

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=dbus/%)
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
	$(RM) -f lib/*dbus* >/dev/null 2>&1
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install install-c

install: api-install-init
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
            $(MAKE) install-jvm; \
          fi
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
            $(MAKE) api-install-dotnet; \
          fi

install-c:
	if [ "$(OPENSSLIBS)" != "no" ]; then \
	  $(MAKE) api-install-c; \
        fi

uninstall: api-uninstall-init \
           api-uninstall-c

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

