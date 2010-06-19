#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/ssl/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sat Jun 19 07:33:21 2010 (serrano)                */
#*    Copyright   :  2001-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for ssl                                             */
#*=====================================================================*/
include ../../Makefile.config

SSLZIP          = lib/commons-ssl.zip

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src

POPULATION	= Makefile README $(SSLZIP)

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
            if [ "$(OPENSSLIBS)" != "no" ]; then \
	      echo "[0m[1;32m>>> C[0m"; \
	      (cd src && $(MAKE) c); \
            fi \
          fi

boot-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m"; \
	    (cd src && $(MAKE) jvm); \
          fi

boot-dotnet:
boot-dotnet_unimplemented:
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
	@ echo $(POPULATION:%=ssl/%)
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
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/ssl.init $(LIBDIR)/$(FILDIR)/ssl.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/ssl.init; \
        fi

install-c:
	if [ "$(OPENSSLIBS)" != "no" ]; then \
	  if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	    cp $(BOOTLIBDIR)/ssl.heap $(LIBDIR)/$(FILDIR)/ssl.heap && \
	    chmod $(BMASK) $(LIBDIR)/$(FILDIR)/ssl.heap; \
          fi; \
	  $(MAKE) install-lib LIB=libbigloossl_s-$(RELEASE); \
	  $(MAKE) install-lib LIB=libbigloossl_e-$(RELEASE); \
	  $(MAKE) install-lnlib LIB=libbigloossl_s-$(RELEASE) LN=libbigloossl_u-$(RELEASE); \
        fi

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/ssl.jheap $(LIBDIR)/$(FILDIR)/ssl.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/ssl.jheap && \
	  cp $(BOOTLIBDIR)/bigloossl_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloossl_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloossl_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloossl_e-$(RELEASE).zip; \
	  cp $(BOOTLIBDIR)/commons-ssl.zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/commons-ssl.zip; \
        fi
	(cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloossl_u-$(RELEASE).zip && \
           $(LN_S) bigloossl_s-$(RELEASE).zip bigloossl_u-$(RELEASE).zip)

install-dotnet:
install-dotnet_unimplemented:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/ssl.jheap $(LIBDIR)/$(FILDIR)/ssl.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/ssl.jheap && \
	  cp $(BOOTLIBDIR)/bigloossl_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloossl_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloossl_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloossl_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloossl_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloossl_e-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloossl_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloossl_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloossl_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/ssl.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/ssl.heap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/commons-ssl.zip
	-$(MAKE) uninstall-lib LIB=libbigloossl_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloossl_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloossl_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/ssl.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloossl_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloossl_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloossl_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloossl_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloossl_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloossl_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
