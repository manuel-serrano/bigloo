#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/gstreamer/Makefile               */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Feb  3 17:17:06 2008 (serrano)                */
#*    Copyright   :  2001-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for gstreamer                                       */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src recette examples

POPULATION	= Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: boot-c # boot-jvm boot-dotnet
boot: boot-c # boot-jvm boot-dotnet
jvm: boot-jvm
dotnet: boot-dotnet

boot-c:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
            echo "[0m[1;32m>>> C[0m"; \
	    (cd src && $(MAKE) c); \
          fi

boot-jvm:
	@ echo "GSTREAMER .jvm: Not implemented";

boot-jvm.not-implemented:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m"; \
	    (cd src && $(MAKE) jvm); \
          fi

boot-dotnet:
	@ echo "GSTREAMER .net: Not implemented";

boot-dotnet.not-implemented:
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
	@ echo $(POPULATION:%=gstreamer/%)
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
	$(RM) -f lib/*gstreamer* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/gstreamer.init $(LIBDIR)/$(FILDIR)/gstreamer.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/gstreamer.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/gstreamer.heap $(LIBDIR)/$(FILDIR)/gstreamer.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/gstreamer.heap; \
        fi; \
	$(MAKE) install-lib LIB=libbigloogstreamer_s-$(RELEASE); \
	$(MAKE) install-shared-lib LIB=libbigloogstreamer_e-$(RELEASE); \
	$(MAKE) install-lnlib LIB=libbigloogstreamer_s-$(RELEASE) LN=libbigloogstreamer_u-$(RELEASE);

install-jvm:

install-jvm_not_implemented:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/gstreamer.jheap $(LIBDIR)/$(FILDIR)/gstreamer.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/gstreamer.jheap && \
	  cp $(BOOTLIBDIR)/bigloogstreamer_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloogstreamer_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloogstreamer_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloogstreamer_e-$(RELEASE).zip; \
        fi
	(cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloogstreamer_u-$(RELEASE).zip && \
           $(LN_S) bigloogstreamer_s-$(RELEASE).zip bigloogstreamer_u-$(RELEASE).zip)

install-dotnet:

install-dotnet_not_implemented:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/gstreamer.jheap $(LIBDIR)/$(FILDIR)/gstreamer.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/gstreamer.jheap && \
	  cp $(BOOTLIBDIR)/bigloogstreamer_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloogstreamer_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloogstreamer_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloogstreamer_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloogstreamer_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloogstreamer_e-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloogstreamer_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloogstreamer_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloogstreamer_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/gstreamer.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/gstreamer.heap
	-$(MAKE) uninstall-lib LIB=libbigloogstreamer_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloogstreamer_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloogstreamer_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/gstreamer.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloogstreamer_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloogstreamer_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloogstreamer_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloogstreamer_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloogstreamer_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloogstreamer_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
