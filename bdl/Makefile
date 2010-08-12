#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bdl/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Oct  3 05:37:54 2001                          */
#*    Last change :  Thu Aug 12 11:34:50 2010 (serrano)                */
#*    Copyright   :  2001-10 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    This Makefile *requires* GNU-Make.                               */
#*    -------------------------------------------------------------    */
#*    The Makefile for bdl                                             */
#*=====================================================================*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Directories where to find the files composing a revision         */
#*---------------------------------------------------------------------*/
POPDIR		= src example

POPULATION	= Makefile README

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: boot-jvm boot-c
boot: boot-c
jvm: boot-jvm

boot-c:
	@if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	  echo "[0m[1;32m>>> (cd src; $(MAKE))[0m"; \
	  (cd src && $(MAKE) c); \
        fi

boot-jvm:
	@if [ "$(JVMBACKEND)" = "yes" ]; then \
	  echo "[0m[1;31m>>> (cd src; $(MAKE) jvm)[0m"; \
	  (cd src && $(MAKE) jvm); \
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
            (cd $$d && $(MAKE) -s pop) \
          done;

#*---------------------------------------------------------------------*/
#*    Clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	(cd src && $(MAKE) clean)
	(cd example && $(MAKE) clean)

distclean: clean
cleanall: distclean
	(cd src && $(MAKE) cleanall)
	$(RM) -f lib/*bdl* >/dev/null 2>&1
	$(RM) -f *~ >/dev/null 2>&1

#*---------------------------------------------------------------------*/
#*    Installation                                                     */
#*---------------------------------------------------------------------*/
.PHONY: install install-c install-jvm uninstall

include ../Makefile.misc

install: install-c install-jvm
	if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	  cp $(BOOTLIBDIR)/bdl.init $(LIBDIR)/$(FILDIR)/bdl.init && \
	  chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/bdl.init; \
        fi

install-c:
	@if [ "$(NATIVEBACKEND)" = "yes" ]; then \
	  $(MAKE) install-lib LIB=libbigloobdl_s-$(RELEASE) && \
	  $(MAKE) install-lnlib LIB=libbigloobdl_s-$(RELEASE) LN=libbigloobdl_u-$(RELEASE) && \
	  if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	    cp $(BOOTLIBDIR)/bdl.heap $(LIBDIR)/$(FILDIR)/bdl.heap && \
	    chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/bdl.heap; \
          fi \
        fi

install-jvm:
	@if [ "$(JVMBACKEND)" = "yes" ]; then \
	  if [ $(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	    cp $(BOOTLIBDIR)/bdl.jheap $(LIBDIR)/$(FILDIR)/bdl.jheap && \
	    chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/bdl.jheap && \
	    cp $(BOOTLIBDIR)/bdl_s.zip $(LIBDIR)/$(FILDIR)/bdl_s.zip && \
	    chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/bdl_s.zip && \
	    (cd $(LIBDIR)/$(FILDIR) && \
             $(RM) -f bdl_u.zip && \
             ln bdl_s.zip bdl_u.zip); \
          fi \
        fi

uninstall:
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bdl.init
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bdl.heap
	-$(MAKE) uninstall-lib LIB=libbigloobdl_s-$(RELEASE)
	-$(MAKE) uninstall-lib LIB=libbigloobdl_u-$(RELEASE)
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bdl.jheap
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bdl_s.zip
	-$(RM) -f $(LIBDIR)/$(FILDIR)/bdl_u.zip

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
distrib:
	(cd src && $(MAKE) ude)
