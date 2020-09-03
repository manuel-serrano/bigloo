#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/gc/Makefile                   */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 15:43:12 1998                          */
#*    Last change :  Tue Jan 21 08:28:25 2020 (serrano)                */
#*    Copyright   :  1998-2020 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The small Makefile to handle the GC library compilation          */
#*=====================================================================*/
do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config
include ../Makefile.misc

POPULATION = Makefile \
  $(POP65) \
  $(POP66) \
  $(POP68) \
  $(POP70ALPHA7) \
  $(POP70) \
  $(POP71ALPHA2) \
  $(POP71ALPHA3) \
  $(POP71) \
  $(POP72ALPHA2) \
  $(POP72ALPHA4) \
  $(POP72ALPHA5) \
  $(POP72ALPHA6) \
  $(POP72D) \
  $(POP73ALPHA1_20120316) \
  $(POP73ALPHA2) \
  $(POP73ALPHA3_20130330) \
  $(POP740) \
  $(POP742) \
  $(POP75027MAY2016) \
  $(POP762) \
  $(POP802) \
  $(POP804) \
  sparc_linux_mach_dep.c \
  gc-7.1.tar.gz \
  $(CUSTOMGC).tar.gz

POP65 = gc-6.5-gentoo.patch

POP66 = gc-6.6-gentoo-ulibc.patch

POP68 = install-gc6.8 \
  gc6.8.patch \
  Makefile-gc6.8

POP70ALPHA7 = install-gc7.0alpha7 \
  gc7.0alpha7.patch \
  Makefile-gc7.0alpha7

POP70 = install-gc-7.0 \
  gc-7.0.patch \
  Makefile-gc-7.0

POP71ALPHA2 = install-gc-7.1alpha2 \
  gc-7.1alpha2.patch

POP71ALPHA3 = install-gc-7.1alpha3-080220 \
  gc-7.1alpha3-080220.patch

POP71 = install-gc-7.1 \
  gc-7.1.patch \
  Makefile-gc-7.1

POP72ALPHA2 = install-gc-7.2alpha2 \
  configure-gc-7.2alpha2 \
  gc-7.2alpha2.patch \
  Makefile-gc-7.2alpha2

POP72ALPHA4 = install-gc-7.2alpha4 \
  configure-gc-7.2alpha4 \
  configure-gc-7.2alpha4-android \
  gc-7.2alpha4.patch \
  gc-7.2alpha4-android.patch \
  Makefile-gc-7.2alpha4

POP72ALPHA5_20110107 = install-bdwgc-7_2alpha5-20110107 \
  configure-bdwgc-7_2alpha5-20110107 \
  configure-bdwgc-7_2alpha5-20110107-android \
  bdwgc-7_2alpha5-20110107.patch \
  bdwgc-7_2alpha5-20110107-android.patch \
  Makefile-bdwgc-7_2alpha5-20110107
             

POP72ALPHA5_20110313 = install-bdwgc-7_2alpha5-20110313 \
  configure-bdwgc-7_2alpha5-20110313 \
  configure-bdwgc-7_2alpha5-20110313-android \
  bdwgc-7_2alpha5-20110313.patch \
  Makefile-bdwgc-7_2alpha5-20110313

POP72ALPHA5 = $(POP72ALPHA5_20110107) \
  $(POP72ALPHA5_20110313)

POP72ALPHA6 = install-gc-7.2alpha6 \
  configure-gc-7.2alpha6 \
  configure-gc-7.2alpha6-android \
  gc-7.2alpha6.patch \
  Makefile-gc-7.2alpha6

POP72D = install-gc-7.2d \
  configure-gc-7.2d \
  configure-gc-7.2d-android \
  gc-7.2d.patch \
  Makefile-gc-7.2d

POP73ALPHA1_20120316 = install-gc-7.3alpha1-20120316 \
  configure-gc-7.3alpha1-20120316 \
  configure-gc-7.3alpha1-20120316-android \
  gc-7.3alpha1-20120316.patch \
  Makefile-gc-7.3alpha1-20120316

POP73ALPHA2 = install-gc-7.3alpha2 \
  configure-gc-7.3alpha2 \
  configure-gc-7.3alpha2-android \
  gc-7.3alpha2.patch \
  Makefile-gc-7.3alpha2 \
  libatomic_ops-7.3alpha2.tar.gz

POP73ALPHA3_20130330 = install-gc-7.3alpha3-20130330 \
  configure-gc-7.3alpha3-20130330 \
  configure-gc-7.3alpha3-20130330-android \
  gc-7.3alpha3-20130330.patch \
  Makefile-gc-7.3alpha3-20130330 \
  libatomic_ops-7.3alpha3-20130330.tar.gz

POP740 = install-gc-7.4.0 \
  configure-gc-7.4.0 \
  configure-gc-7.4.0-android \
  gc-7.4.0.patch \
  Makefile-gc-7.4.0 \
  libatomic_ops-7.4.0.tar.gz

POP742 = install-gc-7.4.2 \
  install-gc-7.4.2-android \
  configure-gc-7.4.2 \
  configure-gc-7.4.2-android \
  gc-7.4.2.patch \
  gc-7.4.2-android.patch \
  Makefile-gc-7.4.2

POP75027MAY2016 = install-gc-7.5.0-27may2016 \
  install-gc-7.5.0-27may2016-android \
  configure-gc-7.5.0-27may2016 \
  configure-gc-7.5.0-27may2016-android \
  gc-7.5.0-27may2016.patch \
  gc-7.5.0-27may2016-android.patch \
  Makefile-gc-7.5.0-27may2016

POP762 = install-gc-7.6.2 \
  install-gc-7.6.2-android \
  configure-gc-7.6.2 \
  configure-gc-7.6.2-android \
  gc-7.6.2.patch \
  libatomic_ops-7.6.2.tar.gz \
  Makefile-gc-7.6.2

POP802 = install-gc-8.0.2 \
  configure-gc-8.0.2 \
  configure-gc-8.0.2-android \
  gc-8.0.2.patch \
  libatomic_ops-7.6.8.tar.gz \
  Makefile-gc-8.0.2

POP804 = install-gc-8.0.4 \
  configure-gc-8.0.4 \
  configure-gc-8.0.4-android \
  gc-8.0.4.patch \
  libatomic_ops-7.6.10.tar.gz \
  Makefile-gc-8.0.4

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
CCFLAGS = $(CFLAGS) $(CGCFLAGS) $(GCOFLAGS) $(CPICFLAGS) -I$(BOOTLIBDIR)

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot-gc boot-gc_mt
.NOTPARALLEL: boot

boot: boot-gc boot-gc_mt

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
	$(MAKE) -C $(CUSTOMGC)_mt clean

cleanall: clean

distclean:
	$(MAKE) -C $(CUSTOMGC) distclean
	$(MAKE) -C $(CUSTOMGC)_mt distclean
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
	    $(MAKE) install-lib EXTRALIB=$(PTHREADLIBS) LIB=libbigloogc_mt-$(RELEASE); \
          fi; \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall uninstall-thread

uninstall: uninstall-thread
	$(MAKE) uninstall-lib LIB=libbigloogc-$(RELEASE)
	$(MAKE) SHAREDSUFFIX=$(SHAREDSUFFIX).1 uninstall-lib LIB=libbigloogc-$(RELEASE)
	$(MAKE) uninstall-lib LIB=libbigloogc_mt-$(RELEASE)
	$(MAKE) SHAREDSUFFIX=$(SHAREDSUFFIX).1 uninstall-lib LIB=libbigloogc_mt-$(RELEASE)
