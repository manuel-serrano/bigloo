#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bigloo/bdb/Makefile                  */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Aug  4 20:11:07 1999                          */
#*    Last change :  Wed Jun 26 15:01:47 2024 (serrano)                */
#*    Copyright   :  1999-2024 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The bdb Makefile                                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.buildconfig
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    The directory that compose a version                             */
#*---------------------------------------------------------------------*/
DIRECTORIES	= bdb blib

#*---------------------------------------------------------------------*/
#*    Booting bdb on a bare system                                     */
#*---------------------------------------------------------------------*/
boot: boot-blib boot-bdb

boot-blib:
	@ $(MAKE) -C blib all

boot-bdb:
	@ $(MAKE) -C bdb MODE=final mode

#*---------------------------------------------------------------------*/
#*    Populating bdb                                                   */
#*---------------------------------------------------------------------*/
pop: 
	@ for d in $(DIRECTORIES) ; do \
             for f in `cd $$d; $(MAKE) -s pop;`; do \
                echo bdb/$$d/$$f; \
             done; \
          done;
	@ echo bdb/Makefile

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@ for d in $(DIRECTORIES) ; do \
             ($(MAKE) -C $$d ROOT="$(ROOT)" clean); \
          done;

cleanall: 
	@ for d in $(DIRECTORIES) ; do \
             ($(MAKE) -C $$d ROOT="$(ROOT)" cleanall); \
          done;

distclean: 
	@ for d in $(DIRECTORIES) ; do \
             ($(MAKE) -C $dd ROOT="$(ROOT)" cleanall); \
          done;


#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    Prepare bdb for a distribution                                   */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOOTDIR)/bdb ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ for d in $(DIRECTORIES) ; do \
             $(MAKE) -C $$d ude; \
          done;
	@ $(RM) -f bdb/Makefile~

#*---------------------------------------------------------------------*/
#*    install & uninstall                                              */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

include ../Makefile.misc

install:
	if [ $(DESTDIR)$(BINDIR) != $(BOOTBINDIR) ]; then \
	   cp $(BOOTBINDIR)/bdb $(DESTDIR)$(BINDIR) && \
	   chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bdb; \
        fi
	if [ $(DESTDIR)$(LIBDIR)/$(FILDIR) != $(BOOTLIBDIR) ]; then \
	   cp $(BOOTLIBDIR)/bdb.heap $(DESTDIR)$(LIBDIR)/$(FILDIR)/bdb.heap && \
           chmod $(MODFILE) $(DESTDIR)$(LIBDIR)/$(FILDIR)/bdb.heap || exit 1; \
        fi
	$(MAKE) install-lib LIB=libbigloobdb_s-$(RELEASE)


uninstall:
	-$(RM) -f $(DESTDIR)$(BINDIR)/bdb
	-$(RM) -f $(DESTDIR)$(LIBDIR)/bdb.heap
	-$(MAKE) uninstall-lib LIB=libbigloobdb_s-$(RELEASE)

