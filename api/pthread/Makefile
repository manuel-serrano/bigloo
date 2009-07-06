#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/pthread/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Fri Jul  3 10:47:18 2009 (serrano)                */
#*    Copyright   :  2001-09 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for pthread                                         */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src recette recette2

POPULATION	= Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: boot
boot:
	@ $(MAKE) boot-c && \
          $(MAKE) boot-jvm && \
          $(MAKE) boot-dotnet

boot-c:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    if [ "$(PTHREADLIBS)" = "no" ]; then \
              echo "[0m[1;32m>>> No native support[0m"; \
            else \
	      if [ "$(CGCTHREADFLAGS)" = "no" ]; then \
                echo "[0m[1;32m>>> No native support[0m"; \
              else \
	        echo "[0m[1;32m>>> C[0m"; \
	        (cd src && $(MAKE) c) || exit 1; \
              fi \
            fi \
          fi

boot-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m"; \
	    (cd src && $(MAKE) jvm) \
          fi
lib-jvm: boot-jvm

boot-dotnet:
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> .NET[0m"; \
	    (cd src && $(MAKE) dotnet) \
          fi
lib-dotnet: boot-dotnet

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
	@ echo $(POPULATION:%=pthread/%)
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
	$(RM) -f lib/*pth* >/dev/null 2>&1
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
	if [ -f $(BOOTLIBDIR)/pthread.init ]; then \
           if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	     cp $(BOOTLIBDIR)/pthread.init $(LIBDIR)/$(FILDIR)/pthread.init && \
	     chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pthread.init; \
           fi; \
        fi

install-c:
	if [ "$(PTHREADLIBS)" != "no" ]; then \
	  if [ "$(CGCTHREADFLAGS)" != "no" ]; then \
	    if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	      cp $(BOOTLIBDIR)/pthread.heap $(LIBDIR)/$(FILDIR)/pthread.heap && \
	      chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pthread.heap; \
            fi; \
	    $(MAKE) install-lib LIB=libbigloopth_s-$(RELEASE); \
	    $(MAKE) install-shared-lib LIB=libbigloopth_e-$(RELEASE); \
	    $(MAKE) install-lnlib LIB=libbigloopth_s-$(RELEASE) LN=libbigloopth_u-$(RELEASE); \
          fi; \
        fi

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/pthread.jheap $(LIBDIR)/$(FILDIR)/pthread.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pthread.jheap && \
	  cp $(BOOTLIBDIR)/bigloopth_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloopth_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloopth_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloopth_e-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopth_s-$(RELEASE).zip && \
	  (cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloopth_u-$(RELEASE).zip && \
           $(LN_S) bigloopth_s-$(RELEASE).zip bigloopth_u-$(RELEASE).zip); \
        fi

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/pthread.jheap $(LIBDIR)/$(FILDIR)/pthread.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/pthread.jheap && \
	  cp $(BOOTLIBDIR)/bigloopth_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloopth_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopth_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloopth_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloopth_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopth_u-$(RELEASE).dll ; \
	  cp $(BOOTLIBDIR)/bigloopth_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloopth_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloopth_e-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pthread.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pthread.heap
	-$(MAKE) uninstall-lib LIB=libbigloopth_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloopth_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloopth_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/pthread.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopth_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopth_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopth_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopth_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopth_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloopth_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
