#*=====================================================================*/
#*    serrano/prgm/project/bigloo/pcre/Makefile                        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jul 18 17:53:07 2014                          */
#*    Last change :  Thu Jun 15 15:45:42 2017 (serrano)                */
#*    Copyright   :  2014-17 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    A small Makefile to handle custom PCRE library.                  */
#*=====================================================================*/

do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config
include ../Makefile.misc

PCRE=pcre$(PCRECUSTOMVERSION)

POPULATION = Makefile configure-pcre install-pcre $(PCRE).tgz

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot boot-pcre
.NOTPARALLEL: boot

boot: boot-pcre

boot-pcre: 
	$(MAKE) -C $(PCRE)
	$(MAKE) -C $(PCRE) install
	cp lib/libpcre2-8.a $(BOOTLIBDIR)/libpcre.a
	cp lib/libpcre2-8.so.0.5.0 $(BOOTLIBDIR)/libpcre.so

#*---------------------------------------------------------------------*/
#*    the population goal                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=pcre/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	if [ -d " $(PCRE)" ]; then \
	  $(MAKE) -C $(PCRE) clean; \
        fi

cleanall: clean

distclean:
	$(RM) -rf $(PCRE)

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
	$(MAKE) -C $(PCRE) install
	if [ "$(PCRECUSTOM)" = "yes" ]; then \
	  $(MAKE) install-lib LIB=libbigloopcre-$(RELEASE); \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) -C $(PCRE) uninstall


