#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/libuv/Makefile                */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Fri Jul 18 17:53:07 2014                          */
#*    Last change :  Tue Mar 24 12:25:06 2020 (serrano)                */
#*    Copyright   :  2014-20 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    A small Makefile to handle custom LIBUV library.                 */
#*=====================================================================*/

do: boot

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
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
          cp $(LIBUV)/include/uv.h $(DESTDIR)$(LIBDIR)/$(FILDIR)/uv.h && \
          chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/uv.h; \
          if [ -d $(DESTDIR)$(LIBDIR)/$(FILDIR)/uv ]; then \
            chmod $(MODDIR) $(DESTDIR)$(LIBDIR)/$(FILDIR)/uv; \
          fi; \
	fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
.PHONY: uninstall 

uninstall:
	$(MAKE) -C $(LIBUV) uninstall


