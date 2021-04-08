#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/pcre2/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jul 18 17:53:07 2014                          */
#*    Last change :  Thu Apr  8 09:29:50 2021 (serrano)                */
#*    Copyright   :  2014-21 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    A small Makefile to handle custom PCRE2 library.                 */
#*=====================================================================*/

do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config
include ../Makefile.misc

PCRE2=pcre2-$(PCRE2CUSTOMVERSION)

POPULATION = Makefile configure-pcre2 install-pcre2 $(PCRE2).tgz

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot boot-pcre2
.NOTPARALLEL: boot

boot: boot-pcre2

boot-pcre2: 
	$(MAKE) -C $(PCRE2)
	$(MAKE) -C $(PCRE2) install DESTDIR=$(BOOTDIR)/pcre2

#*---------------------------------------------------------------------*/
#*    the population goal                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=pcre2/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	if [ -d " $(PCRE2)" ]; then \
	  $(MAKE) -C $(PCRE2) clean; \
        fi

cleanall: clean

distclean:
	$(RM) -rf $(PCRE2)

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
	$(MAKE) -C $(PCRE2) install exec_prefix=$(BINDIR)/BGLINSTALLTMP
	rm -rf $(DESTDIR)$(BINDIR)/BGLINSTALLTMP

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) -C $(PCRE2) uninstall
