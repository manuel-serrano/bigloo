#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdl/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Wed Nov  9 17:28:29 2011 (serrano)                */
#*    Copyright   :  2001-11 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for bdl                                             */
#*=====================================================================*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR = src example

POPULATION = Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build build build-c build-jvm build-dotnet

all: build
boot: build

build:
	@ $(MAKE) build-c && $(MAKE) build-jvm

c: build-c
jvm: build-jvm
dotnet: build-dotnet

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

build-dotnet:
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> .NET[0m"; \
	    $(MAKE) -C src build-dotnet; \
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
	@ echo $(POPULATION:%=bdl/%)
	@ for d in $(POPDIR); do \
            $(MAKE) -C $$d -s pop; \
          done;

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	$(MAKE) -C src clean
	$(MAKE) -C example clean

distclean: clean
cleanall: distclean
	(cd src && $(MAKE) cleanall)
	$(RM) -f lib/*bdl* >/dev/null 2>&1
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	$(MAKE) -C src install

uninstall:
	$(MAKE) -C src uninstall

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	$(MAKE) -C src ude
