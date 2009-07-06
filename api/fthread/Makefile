#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/fthread/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Wed Dec 19 17:01:09 2007 (serrano)                */
#*    Copyright   :  2001-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for fthread                                         */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src examples recette semantics

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

boot-dotnet:
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> .NET[0m"; \
	    (cd src && $(MAKE) dotnet) \
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
	@ echo $(POPULATION:%=fthread/%)
	@ for d in $(POPDIR); do \
            (cd $$d && $(MAKE) -s pop) \
          done;

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	(cd src && $(MAKE) clean)
	(cd recette && $(MAKE) clean)
	(cd examples && $(MAKE) clean)

distclean: clean
	(cd src && $(MAKE) distclean)
	(cd recette && $(MAKE) distclean)
	(cd examples && $(MAKE) distclean)

cleanall: distclean
	(cd src && $(MAKE) cleanall)
	$(RM) -f lib/*fth* >/dev/null 2>&1
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
	if [ -f $(BOOTLIBDIR)/fthread.init ]; then \
	  if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	    cp $(BOOTLIBDIR)/fthread.init $(LIBDIR)/$(FILDIR)/fthread.init && \
	    chmod $(BMASK) $(LIBDIR)/$(FILDIR)/fthread.init; \
          fi; \
        fi

install-c:
	if [ "$(PTHREADLIBS)" != "no" ]; then \
	  if [ "$(CGCTHREADFLAGS)" != "no" ]; then \
	    if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	      cp $(BOOTLIBDIR)/fthread.heap $(LIBDIR)/$(FILDIR)/fthread.heap && \
	      chmod $(BMASK) $(LIBDIR)/$(FILDIR)/fthread.heap; \
            fi; \
	    $(MAKE) install-lib LIB=libbigloofth_s-$(RELEASE); \
	    $(MAKE) install-shared-lib LIB=libbigloofth_e-$(RELEASE); \
	    $(MAKE) install-lnlib LIB=libbigloofth_s-$(RELEASE) LN=libbigloofth_u-$(RELEASE); \
         fi; \
       fi

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/fthread.jheap $(LIBDIR)/$(FILDIR)/fthread.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/fthread.jheap && \
	  cp $(BOOTLIBDIR)/bigloofth_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloofth_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloofth_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloofth_e-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloofth_s-$(RELEASE).zip && \
	  (cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloofth_u-$(RELEASE).zip && \
           $(LN_S) bigloofth_s-$(RELEASE).zip bigloofth_u-$(RELEASE).zip); \
        fi

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/fthread.jheap $(LIBDIR)/$(FILDIR)/fthread.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/fthread.jheap && \
	  cp $(BOOTLIBDIR)/bigloofth_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloofth_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloofth_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloofth_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloofth_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloofth_u-$(RELEASE).dll ; \
	  cp $(BOOTLIBDIR)/bigloofth_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloofth_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloofth_e-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/fthread.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/fthread.heap
	-$(MAKE) uninstall-lib LIB=libbigloofth_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloofth_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloofth_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/fthread.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloofth_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloofth_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloofth_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloofth_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloofth_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloofth_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
