#*=====================================================================*/
#*    serrano/prgm/project/bigloo/gc/Makefile                          */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 15:43:12 1998                          */
#*    Last change :  Wed Dec 29 09:30:36 2010 (serrano)                */
#*    Copyright   :  1998-2010 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The small Makefile to handle the GC library compilation          */
#*=====================================================================*/
do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config
include ../Makefile.misc

POPULATION = Makefile \
             install-gc6.8 gc6.8.patch Makefile-gc6.8 \
             install-gc7.0alpha7 gc7.0alpha7.patch Makefile-gc7.0alpha7 \
             install-gc-7.0 gc-7.0.patch Makefile-gc-7.0 \
             install-gc-7.1alpha2 gc-7.1alpha2.patch \
             install-gc-7.1alpha3-080220 gc-7.1alpha3-080220.patch \
             install-gc-7.1 gc-7.1.patch Makefile-gc-7.1 \
             install-gc-7.2alpha2 configure-gc-7.2alpha2 \
             gc-7.2alpha2.patch Makefile-gc-7.2alpha2 \
             install-gc-7.2alpha4 configure-gc-7.2alpha4 \
             configure-gc-7.2alpha4-android \
             gc-7.2alpha4.patch gc-7.2alpha4-android.patch \
             Makefile-gc-7.2alpha4 \
             gc-6.5-gentoo.patch gc-6.6-gentoo-ulibc.patch \
             sparc_linux_mach_dep.c \
             $(CUSTOMGC).tar.gz

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
CCFLAGS = $(CFLAGS) $(CGCFLAGS) $(GCOFLAGS) $(CPICFLAGS) -I$(BOOTLIBDIR)

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot-gc boot-gc_fth
.NOTPARALLEL: boot

boot: boot-gc boot-gc_fth

include Makefile-$(CUSTOMGC)

#*---------------------------------------------------------------------*/
#*    the population goal                                              */
#*---------------------------------------------------------------------*/
pop:
	@ for f in `cd gc-forward && $(MAKE) -s pop`; do \
            echo gc/$$f; \
          done;
	@ echo $(POPULATION:%=gc/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	$(MAKE) -C $(CUSTOMGC) clean
	$(MAKE) -C $(CUSTOMGC)_fth clean

cleanall: clean

distclean:
	$(MAKE) -C $(CUSTOMGC) distclean
	$(MAKE) -C $(CUSTOMGC)_fth distclean
	$(RM) -rf gc

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
.PHONY: distrib

distrib:

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install install-thread

install:
	if [ "$(GCCUSTOM)" = "yes" ]; then \
	  $(MAKE) install-lib LIB=libbigloogc-$(RELEASE); \
	  if [ "$(GCTHREADSUPPORT) " = "yes " ]; then \
	    $(MAKE) install-lib LIB=libbigloogc_fth-$(RELEASE); \
          fi; \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall uninstall-thread

uninstall: uninstall-thread
	$(MAKE) uninstall-lib LIB=libbigloogc-$(RELEASE)
	$(MAKE) SHAREDSUFFIX=$(SHAREDSUFFIX).1 uninstall-lib LIB=libbigloogc-$(RELEASE)
	$(MAKE) uninstall-lib LIB=libbigloogc_fth-$(RELEASE)
	$(MAKE) SHAREDSUFFIX=$(SHAREDSUFFIX).1 uninstall-lib LIB=libbigloogc_fth-$(RELEASE)

