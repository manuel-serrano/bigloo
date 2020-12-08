#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/gmp/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 15:43:12 1998                          */
#*    Last change :  Tue Mar 24 12:24:54 2020 (serrano)                */
#*    Copyright   :  1998-2020 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The small Makefile to handle custom GMP libraries.               */
#*=====================================================================*/
do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config
include ../Makefile.misc

GMP=gmp-$(GMPCUSTOMVERSION)

POPULATION = Makefile configure-gmp install-gmp $(GMP).tgz

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot boot-gmp
.NOTPARALLEL: boot

boot: boot-gmp

boot-gmp: 
	$(MAKE) -C $(GMP)
	$(MAKE) -C $(GMP) install DESTDIR=$(BOOTDIR)/gmp

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
	if [ -d " gmp-$(GMPCUSTOMVERSION)$(GMP)" ]; then \
	  $(MAKE) -C $(GMP) clean; \
        fi

cleanall: clean

distclean:
	$(RM) -rf $(GMP)

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*---------------------------------------------------------------------*/
.PHONY: distrib

distrib:

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install

install.old:
	if [ "$(GMPCUSTOM)" = "yes" ]; then \
	  $(MAKE) install-lib LIB=libgmp; \
          cp $(GMP)/gmp.h $(DESTDIR)$(LIBDIR)/$(FILDIR)/gmp.h && \
          chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/gmp.h; \
	fi

install:
	$(MAKE) -C $(GMP) install exec_prefix=$(BINDIR)/BGLINSTALLTMP
	rm -rf $(DESTDIR)$(BINDIR)/BGLINSTALLTMP
	cp $(GMP)/gmp.h $(DESTDIR)$(LIBDIR)/$(FILDIR)/gmp.h && \
	chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/gmp.h

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) -C $(GMP) uninstall

