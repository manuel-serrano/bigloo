#*=====================================================================*/
#*    serrano/prgm/project/bigloo/api/calendar/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Tue Jul 22 09:46:31 2008 (serrano)                */
#*    Copyright   :  2001-08 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for CALENDAR                                        */
#*=====================================================================*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src

POPULATION	= Makefile README

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
	    echo "[0m[1;31m>>> JVM[0m";  \
	    (cd src && $(MAKE) jvm); \
          fi

boot-dotnet:
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
	@ echo $(POPULATION:%=calendar/%)
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
	$(RM) -f lib/*calendar* >/dev/null 2>&1
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
	  cp $(BOOTLIBDIR)/calendar.init $(LIBDIR)/$(FILDIR)/calendar.init && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/calendar.init; \
        fi

install-c:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/calendar.heap $(LIBDIR)/$(FILDIR)/calendar.heap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/calendar.heap; \
        fi
	$(MAKE) install-lib LIB=libbigloocalendar_s-$(RELEASE);
	$(MAKE) install-shared-lib LIB=libbigloocalendar_e-$(RELEASE);
	$(MAKE) install-lnlib LIB=libbigloocalendar_s-$(RELEASE) LN=libbigloocalendar_u-$(RELEASE);

install-jvm:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/calendar.jheap $(LIBDIR)/$(FILDIR)/calendar.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/calendar.jheap && \
	  cp $(BOOTLIBDIR)/bigloocalendar_s-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloocalendar_s-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocalendar_s-$(RELEASE).zip && \
	  cp $(BOOTLIBDIR)/bigloocalendar_e-$(RELEASE).zip $(LIBDIR)/$(FILDIR)/bigloocalendar_e-$(RELEASE).zip && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocalendar_e-$(RELEASE).zip && \
	  (cd $(LIBDIR)/$(FILDIR) && \
           $(RM) -f bigloocalendar_u-$(RELEASE).zip && \
           $(LN_S) bigloocalendar_s-$(RELEASE).zip bigloocalendar_u-$(RELEASE).zip); \
        fi

install-dotnet:
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/calendar.jheap $(LIBDIR)/$(FILDIR)/calendar.jheap && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/calendar.jheap && \
	  cp $(BOOTLIBDIR)/bigloocalendar_s-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloocalendar_s-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocalendar_s-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloocalendar_e-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloocalendar_e-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocalendar_e-$(RELEASE).dll && \
	  cp $(BOOTLIBDIR)/bigloocalendar_u-$(RELEASE).dll $(LIBDIR)/$(FILDIR)/bigloocalendar_u-$(RELEASE).dll && \
	  chmod $(BMASK) $(LIBDIR)/$(FILDIR)/bigloocalendar_u-$(RELEASE).dll ; \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/calendar.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/calendar.heap
	-$(MAKE) uninstall-lib LIB=libbigloocalendar_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloocalendar_e-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloocalendar_u-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/calendar.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocalendar_s-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocalendar_e-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocalendar_u-$(RELEASE).zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocalendar_s-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocalendar_e-$(RELEASE).dll
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bigloocalendar_u-$(RELEASE).dll

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
