#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/mail/Makefile                    */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jul 15 14:43:02 2007 (serrano)                */
#*    Copyright   :  2001-07 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for MAIL                                            */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src

POPULATION	= Makefile README

PACKAGES	= quoted-printable

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
	    echo "[0m[1;32m>>> C[0m"; \
	    (cd src && $(MAKE) c); \
          fi

boot-jvm:
	@ if [ "$(JVMBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> JVM[0m"; \
	    (cd src && $(MAKE) jvm); \
          fi

boot-dotnet:
	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \
	    echo "[0m[1;31m>>> .NET[0m"; \
	    (cd src && $(MAKE) dotnet); \
          fi

#*---------------------------------------------------------------------*/
#*    pkgs                                                             */
#*---------------------------------------------------------------------*/
pkgs: src/Pkgs

src/Pkgs:
	$(BGLPKG_EXE) -C $@ -x $(PACKAGES) --flat

update-pkgs:
	$(BGLPKG_EXE) -Y -C $@ -x $(PACKAGES) --flat

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
	@ echo $(POPULATION:%=mail/%)
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
	$(RM) -f lib/*mail* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/mail.init $(LIBDIR)/$(FILDIR)/mail.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/mail.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/mail.heap $(LIBDIR)/$(FILDIR)/mail.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/mail.heap; \
        fi
	$(MAKE) install-lib LIB=libbigloomail_s-$(RELEASE);
	$(MAKE) install-shared-lib LIB=libbigloomail_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbigloomail_s-$(RELEASE) LN=libbigloomail_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/mail.jheap $(LIBDIR)/$(FILDIR)/mail.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/mail.jheap && \
	  cp $(BOOTLIBDIR)/bigloomail_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomail_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloomail_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomail_e-$(RELEASE).zip; \
        fi
	(cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloomail_u-$(RELEASE).zip && \
           $(LN_S) bigloomail_s-$(RELEASE).zip bigloomail_u-$(RELEASE).zip)

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/mail.jheap $(LIBDIR)/$(FILDIR)/mail.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/mail.jheap && \
	  cp $(BOOTLIBDIR)/bigloomail_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloomail_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomail_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloomail_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloomail_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomail_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloomail_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloomail_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloomail_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/mail.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/mail.heap
	-$(MAKE) uninstall-lib LIB=libbigloomail_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloomail_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloomail_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/mail.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomail_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomail_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomail_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomail_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomail_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloomail_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
