#*=====================================================================*/
#*    serrano/prgm/project/bigloo/libunistring/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jul 18 17:53:07 2014                          */
#*    Last change :  Mon Jun 19 17:15:11 2017 (serrano)                */
#*    Copyright   :  2014-17 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    A small Makefile to handle custom UNISTRING library.             */
#*=====================================================================*/

do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
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
	$(MAKE) -C $(UNISTRING) install
	cp lib/libunistring2-8.a $(BOOTLIBDIR)/libunistring.a
	cp lib/libunistring2-8.so.0.5.0 $(BOOTLIBDIR)/libunistring.so

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
	$(MAKE) -C $(UNISTRING) install
	if [ "$(UNISTRINGCUSTOM)" = "yes" ]; then \
	  $(MAKE) install-lib LIB=libbigloounistring-$(RELEASE); \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) -C $(UNISTRING) uninstall


