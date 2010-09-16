#*=====================================================================*/
#*    serrano/prgm/project/bigloo/gmp/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 15:43:12 1998                          */
#*    Last change :  Thu Aug 12 11:35:38 2010 (serrano)                */
#*    Copyright   :  1998-2010 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The small Makefile to handle custom GMP libraries.               */
#*=====================================================================*/
do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config
include ../Makefile.misc

POPULATION = Makefile configure-gmp install-gmp

#*---------------------------------------------------------------------*/
#*    Flags                                                            */
#*---------------------------------------------------------------------*/
CCFLAGS = $(CFLAGS) $(CGMPFLAGS) $(GMPOFLAGS) $(CPICFLAGS) -I $(BOOTLIBDIR)
GMP = $(CUSTOMGMP)
OUTDIR = $(GMP)/.libs
OBJDIR = objs

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot boot-gmp
.NOTPARALLEL: boot

boot: boot-gmp

boot-gmp: $(OUTDIR)/libgmp.a
	mkdir -p $(OBJDIR)
	(cd $(OBJDIR) && $(AR) x ../$(OUTDIR)/libgmp.a)
	$(MAKE) $(BOOTLIBDIR)/libbigloogmp-$(RELEASE).$(SHAREDSUFFIX)
	$(MAKE) $(BOOTLIBDIR)/libbigloogmp-$(RELEASE).a

$(OUTDIR)/libgmp.a:
	$(MAKE) -C $(GMP) CFLAGS="$(CCFLAGS)"

$(BOOTLIBDIR)/libbigloogmp-$(RELEASE).$(SHAREDSUFFIX):
	$(MAKE) shared-lib \
             LIBDEST=$@ \
             SONAME=libbigloogmp-$(RELEASE).$(SHAREDSUFFIX) \
             GCCLOSELIB= \
             CLOSELIBS= -lm -lc \
             EXTRALIBS= \
             LDLIBS= -lm -lc \
             OBJECTS="$(OBJDIR)/*.o"

$(BOOTLIBDIR)/libbigloogmp-$(RELEASE).a:
	cp $(OUTDIR)/libgmp.a $@
	$(RANLIB) $@

#*---------------------------------------------------------------------*/
#*    the population goal                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=gmp/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	if [ -d "$(GMP)" ]; then \
	  $(MAKE) -C $(GMP) clean; \
        fi
	$(RM) -rf objs

cleanall: clean

distclean:
	$(RM) -rf gmp

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
.PHONY: distrib

distrib:

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install

install:
	if [ "$(GMPCUSTOM)" = "yes" ]; then \
	  $(MAKE) install-lib LIB=libbigloogmp-$(RELEASE); \
          cp $(GMP)/gmp.h $(LIBDIR)/$(FILDIR)/gmp.h && \
          chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/gmp.h; \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) uninstall-lib LIB=libbigloogmp-$(RELEASE)
	$(MAKE) SHAREDSUFFIX=$(SHAREDSUFFIX).1 uninstall-lib LIB=libbigloogmp-$(RELEASE)

