#*=====================================================================*/
#*    serrano/prgm/project/bigloo/libuv/Makefile                       */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jul 18 17:53:07 2014                          */
#*    Last change :  Mon Jul 21 08:38:24 2014 (serrano)                */
#*    Copyright   :  2014 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    A small Makefile to handle custom LIBUV library.                 */
#*=====================================================================*/

do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config
include ../Makefile.misc

LIBUV=libuv-$(LIBUVCUSTOMVERSION)

POPULATION = Makefile configure-libuv install-libuv $(LIBUV).tgz

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*---------------------------------------------------------------------*/
.PHONY: boot boot-libuv
.NOTPARALLEL: boot

boot: boot-libuv

boot-libuv: 
	$(MAKE) -C $(LIBUV)

#*---------------------------------------------------------------------*/
#*    the population goal                                              */
#*---------------------------------------------------------------------*/
pop:
	@ echo $(POPULATION:%=libuv/%)

#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	if [ -d " libuv-$(LIBUVCUSTOMVERSION)$(LIBUV)" ]; then \
	  $(MAKE) -C $(LIBUV) clean; \
        fi

cleanall: clean

distclean:
	$(RM) -rf $(LIBUV)

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
	$(MAKE) -C $(LIBUV) install
	if [ "$(LIBUVCUSTOM)" = "yes" ]; then \
	  $(MAKE) install-lib LIB=libbigloouv-$(RELEASE); \
          cp $(LIBUV)/include/uv.h $(LIBDIR)/$(FILDIR)/uv.h && \
          chmod $(MODFILE) $(LIBDIR)/$(FILDIR)/uv.h; \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) -C $(LIBUV) uninstall


