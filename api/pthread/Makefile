#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/pthread/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jun 20 12:33:44 2010 (serrano)                */
#*    Copyright   :  2001-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for pthread                                         */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    The name of the API (used by Makefile.api).                      */
#*---------------------------------------------------------------------*/
API = pthread

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR = src recette recette2
POPULATION = Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
.PHONY: build build build-c build-jvm build-dotnet

all: build
boot: build

build:
	@ $(MAKE) build-c && \
          $(MAKE) build-jvm && \
          $(MAKE) build-dotnet

c: build-c
jvm: build-jvm
dotnet: build-dotnet

build-c:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    if [ "$(PTHREADLIBS)" = "no" ]; then \
              echo "[0m[1;32m>>> No native support[0m"; \
            else \
	      if [ "$(CGCTHREADFLAGS)" = "no" ]; then \
                echo "[0m[1;32m>>> No native support[0m"; \
              else \
	        echo "[0m[1;32m>>> C[0m"; \
	        (cd src && $(MAKE) build-c) || exit 1; \
              fi \
            fi \
          fi

build-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m"; \
	    (cd src && $(MAKE) build-jvm) \
          fi

build-dotnet:
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> .NET[0m"; \
	    (cd src && $(MAKE) build-dotnet) \
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
	(cd recette && $(MAKE) clean)
	(cd recette2 && $(MAKE) clean)

distclean: clean
	(cd src && $(MAKE) distclean)
	(cd recette && $(MAKE) distclean)
	(cd recette2 && $(MAKE) distclean)

cleanall: distclean
	(cd src && $(MAKE) cleanall)
	$(RM) -f lib/*fth* >/dev/null 2>&1
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install install-c install-jvm uninstall

install: api-install-init
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) api-install-c; \
          fi
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
            $(MAKE) api-install-jvm; \
          fi
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
            $(MAKE) api-install-dotnet; \
          fi

uninstall: api-uninstall-init \
           api-uninstall-c \
           api-uninstall-jvm \
           api-uninstall-dotnet

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
