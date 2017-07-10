#*=====================================================================*/
#*    .../bigloo/api-contribs/bigloo-with-dbus/api/dbus/Makefile       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Sun Jul  9 09:56:48 2017 (serrano)                */
#*    Copyright   :  2001-17 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for dbus                                            */
#*=====================================================================*/
include ../../Makefile.buildconfig
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src

POPULATION	= Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: boot-c #boot-jvm boot-dotnet
boot: boot-c boot-jvm boot-dotnet
jvm: boot-jvm
dotnet: boot-dotnet

boot-c:
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
            if [ "$(DBUSLIBS) " != "no " ]; then \
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
.PHONY: install install-c #install-jvm uninstall

include ../../Makefile.misc

install: install-init
	@ if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	    $(MAKE) install-c; \
          fi
#* 	@ if [ "$(JVMBACKEND)" = "yes" ]; then \                       */
#*             $(MAKE) install-jvm; \                                  */
#*           fi                                                        */
#* 	@ if [ "$(DOTNETBACKEND)" = "yes" ]; then \                    */
#*             $(MAKE) install-dotnet; \                               */
#*           fi                                                        */

install-init:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/dbus.init $(LIBDIR)/$(FILDIR)/dbus.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/dbus.init; \
        fi

install-c: copybglheaders
	if [ "$(DBUSLIBS) " != "no" ]; then \
	  if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	    cp $(BOOTLIBDIR)/dbus.heap $(LIBDIR)/$(FILDIR)/dbus.heap && \
	    chmod $(BMASK) $(LIBDIR)/$(FILDIR)/dbus.heap; \
          fi; \
	  $(MAKE) install-lib LIB=libbigloodbus_s-$(RELEASE); \
	  $(MAKE) install-shared-lib LIB=libbigloodbus_e-$(RELEASE); \
	  $(MAKE) install-lnlib LIB=libbigloodbus_s-$(RELEASE) LN=libbigloodbus_u-$(RELEASE); \
        fi
copybglheaders:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/bglconnection.h $(LIBDIR)/$(FILDIR)/bglconnection.h;\
	  cp $(BOOTLIBDIR)/bglmessage.h $(LIBDIR)/$(FILDIR)/bglmessage.h; \
	  cp $(BOOTLIBDIR)/bglservice.h $(LIBDIR)/$(FILDIR)/bglservice.h; \
	fi

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/dbus.jheap $(LIBDIR)/$(FILDIR)/dbus.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/dbus.jheap && \
	  cp $(BOOTLIBDIR)/bigloodbus_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloodbus_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloodbus_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR) && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloodbus_e-$(RELEASE).zip; \
        fi
	(cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloodbus_u-$(RELEASE).zip && \
           ln bigloodbus_s-$(RELEASE).zip bigloodbus_u-$(RELEASE).zip)

install-dotnet:
install-dotnet_unimplemented:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/dbus.jheap $(LIBDIR)/$(FILDIR)/dbus.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/dbus.jheap && \
	  cp $(BOOTLIBDIR)/bigloodbus_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloodbus_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloodbus_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloodbus_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloodbus_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloodbus_e-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloodbus_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloodbus_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloodbus_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/dbus.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/dbus.heap
	-$(MAKE) uninstall-lib LIB=libbigloodbus_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloodbus_u-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloodbus_e-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/dbus.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloodbus_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloodbus_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloodbus_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloodbus_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloodbus_u-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloodbus_e-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
