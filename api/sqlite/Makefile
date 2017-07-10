#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/sqlite/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jul  9 10:16:22 2017 (serrano)                */
#*    Copyright   :  2001-17 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for sqlite                                          */
#*=====================================================================*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    The name of the API (used by Makefile.api).                      */
#*---------------------------------------------------------------------*/
API = sqlite

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR = src recette
POPULATION = Makefile README recette/Makefile recette/recette.scm

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR = src recette
POPULATION = Makefile README

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
	    echo "[0m[1;32m>>> C[0m"; \
	    $(MAKE) -C src build-c; \
          fi

build-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m"; \
	    $(MAKE) -C src build-jvm; \
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
            $(MAKE) -C $$d -s pop; \
          done;

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(MAKE) -C src clean
	$(MAKE) -C recette clean

distclean: clean
	$(MAKE) -C src distclean
	$(MAKE) -C recette distclean

cleanall: distclean
	$(MAKE) -C src cleanall
	$(RM) -f lib/*fth* >/dev/null 2>&1
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install

install: api-install-init
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) api-install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
            $(MAKE) api-install-jvm; \
          fi

uninstall: api-uninstall-init \
           api-uninstall-c \
           api-uninstall-jvm

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	$(MAKE) -C src ude

#*---------------------------------------------------------------------*/
#*    Common rules                                                     */
#*---------------------------------------------------------------------*/
include ../../Makefile.misc
include ../Makefile.api
