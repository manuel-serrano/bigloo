#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/libunistring/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jul 18 17:53:07 2014                          */
#*    Last change :  Wed Dec 11 09:32:21 2019 (serrano)                */
#*    Copyright   :  2014-19 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    A small Makefile to handle custom UNISTRING library.             */
#*=====================================================================*/

do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config
include ../Makefile.misc

UNISTRING=libunistring-$(UNISTRINGCUSTOMVERSION)

POPULATION = Makefile configure-unistring install-unistring $(UNISTRING).tgz

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot boot-unistring
.NOTPARALLEL: boot

boot: boot-unistring

boot-unistring: 
	$(MAKE) -C $(UNISTRING)
	$(MAKE) -C $(UNISTRING) install DESTDIR=$(BOOTDIR)/libunistring

#*---------------------------------------------------------------------*/
#*    the population goal                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=libunistring/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	if [ -d " $(UNISTRING)" ]; then \
	  $(MAKE) -C $(UNISTRING) clean; \
        fi

cleanall: clean

distclean:
	$(RM) -rf $(UNISTRING)

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
	$(MAKE) -C $(UNISTRING) install exec_prefix=$(BOOTDIR)/BGLINSTALLTMP
	rm -rf $(BOOTDIR)/BGLINSTALLTMP

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) -C $(UNISTRING) uninstall


