#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/libbacktrace/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jul 18 17:53:07 2014                          */
#*    Last change :  Thu Dec  2 15:09:08 2021 (serrano)                */
#*    Copyright   :  2014-21 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    A small Makefile to handle custom LIBBACKTRACE library.          */
#*=====================================================================*/

do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config
include ../Makefile.misc

LIBBACKTRACE=libbacktrace-$(LIBBACKTRACECUSTOMVERSION)

POPULATION = Makefile configure-libbacktrace install-libbacktrace $(LIBBACKTRACE).tgz

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot boot-libbacktrace
.NOTPARALLEL: boot

boot: boot-libbacktrace

boot-libbacktrace: 
	$(MAKE) -C $(LIBBACKTRACE)
	$(MAKE) -C $(LIBBACKTRACE) install DESTDIR=$(BOOTDIR)/libbacktrace

#*---------------------------------------------------------------------*/
#*    the population goal                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=libbacktrace/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	if [ -d " $(LIBBACKTRACE)" ]; then \
	  $(MAKE) -C $(LIBBACKTRACE) clean; \
        fi

cleanall: clean

distclean:
	$(RM) -rf $(LIBBACKTRACE)

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
	$(MAKE) -C $(LIBBACKTRACE) install exec_prefix=$(BOOTDIR)/BGLINSTALLTMP
	rm -rf $(BOOTDIR)/BGLINSTALLTMP

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) -C $(LIBBACKTRACE) uninstall


